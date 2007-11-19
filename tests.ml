(*
 *  This file is part of JavaLib
 *  Copyright (c)2007 Université de Rennes 1 / CNRS
 *  Tiphaine.Turpin@irisa.fr
 *  Laurent.Hubert@irisa.fr
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)


open JDumpBasics
open JDumpLow
open IO

(** {2 Tests of parsing/unparsing functions}*)

let input_copy ch =
  let queue = Queue.create ()
  and eq = ref true in
  let ch' =
    create_in
      ~read:(function () ->
	       let c = read ch in
		 Queue.push c queue;
		 c)
      ~input:(fun s p l ->
		let n = input ch s p l in
		  for i = p to p + n - 1 do
		    Queue.push s.[i] queue
		  done;
		  n)
      ~close:(function () -> ())
  and ch'' =
    create_out
      ~write:(function c ->
		eq :=
		  (try
		     Queue.pop queue = c && ! eq
		   with Queue.Empty -> false);
		assert ! eq) (* fail fast *)
      ~output:(fun s p l ->
		 (try
		    for i = p to p + l - 1 do
		      eq := Queue.pop queue = s.[i] && !eq
		    done
		  with Queue.Empty -> eq := false);
		 assert ! eq; (* fail fast *)
		 l)
      ~flush:(function () -> ())
      ~close:(function () -> ! eq && Queue.is_empty queue)
  in
    ch', ch''


open JBasics
open JClassLow

let parse_code ch consts len =
  let ch , pos = IO.pos_in ch in
  let code = Array.create len JClass.OpInvalid in
    while pos() < len do
      let p = pos() in
      let ch, ch' = input_copy ch in
	code.(p) <- JInstruction.opcode2instruction consts (JCode.parse_full_opcode ch pos);
	let ch', count = pos_out ch' in
	  JCode.unparse_instruction ch' (function () -> p + count ())
	    (JInstruction.instruction2opcode (DynArray.of_array consts) code.(p));
	  if
	    not (IO.close_out ch')
	  then failwith ("bad unparsing for " ^ opcode (JInstruction.instruction2opcode (DynArray.of_array consts) code.(p)))
    done;
    code

(*
let  _ =
  let cp = JFile.class_path ["tmp" ; "tmp.jar"] in
    ignore (JFile.lookup cp "p1.A");
    ignore (JFile.lookup cp "p2.B");
    JFile.close_class_path cp;
    prerr_endline "individual class lookup is OK."
*)

(** {2 Tests of conversion between low and high level represetations of classes}*)


let filter_flags authorized fl =
  List.filter (fun f->List.exists ((=)f) authorized) fl

let dump_to_string f d =
  let ch = IO.output_string () in (f ch d; IO.close_out ch)

let attributes_to_string cl al =
  let ch = IO.output_string () in
    ExtList.List.iteri (fun i a ->
      IO.printf ch "att%d: " i;
      dump_attrib ch cl a) al;
    IO.close_out ch

let eq_list eq l1 l2 =
  try
    List.fold_left2
      (fun b e1 e2 -> b && eq e1 e2)
      true
      (List.sort compare l1)
      (List.sort compare l2)
  with Invalid_argument "List.fold_left2" -> false

let eq_ccode cl c1 c2 =
  let i = ref 0 in
    while !i < Array.length c1 do
      if c1.(!i) <> c2.(!i) then
	begin
	  match c1.(!i), c2.(!i) with
	    | OpCodeLdc1 index1, OpCodeLdc1w index2
	    | OpCodeLdc1w index1, OpCodeLdc1 index2 when index1 = index2 ->
		i := !i +2
	    | OpCodeInvokeVirtual c1, OpCodeInvokeVirtual c2
	    | OpCodeInvokeNonVirtual c1, OpCodeInvokeNonVirtual c2
	    | OpCodeInvokeStatic c1, OpCodeInvokeStatic c2
	    | OpCodeInvokeInterface (c1,_), OpCodeInvokeInterface (c2,_)
	    | OpCodeCheckCast c1, OpCodeCheckCast c2
		when cl.j_consts.(c1) = cl.j_consts.(c2) -> ()
	    | _,_ ->
		failwith ("instructions differ: "^opcode c1.(!i)^" <> "^opcode c2.(!i))
	end;
      incr i
    done;
    true



let rec eq_code cl c1 c2 =
  match c1,c2 with
    | None, None -> true
    | None, Some _
    | Some _, None -> failwith "a method has no code while the other has."
    | Some c1, Some c2 ->
	if c1.c_max_stack <> c2.c_max_stack
	  || c1.c_max_locals <> c2.c_max_locals
	  || c1.c_exc_tbl <> c2.c_exc_tbl
	then failwith "max_local, max_stack or exc_tbl differs";
	(try eq_attribs cl c1.c_attributes c2.c_attributes
	  with Failure msg -> failwith ("code attributes differ: "^msg));
	if not (eq_ccode cl c1.c_code c2.c_code)
	then prerr_endline "instructions differ"; (* should have already been tested, it happens that wide instruction are replaced by standard instructions when arguments are small enough.*)
	true

and eq_inner_classes cl icl1 icl2 =
  let eq_inner_class cl
      (inner_class_info1, outer_class_info1, inner_name1, inner_class_access_flags1)
       (inner_class_info2, outer_class_info2, inner_name2, inner_class_access_flags2) =
    if inner_class_info1 <> inner_class_info2
    then failwith "innerclasses inner_class_info differ";
    if outer_class_info1 <> outer_class_info2
    then failwith "innerclasses outer_class_info differ";
    if inner_name1 <> inner_name2
    then failwith "innerclasses inner_name differ";
    if List.sort compare (filter_flags [AccPublic;AccPrivate;AccProtected;AccStatic;AccFinal;AccInterface;AccAbstract] inner_class_access_flags1)
      <> List.sort compare inner_class_access_flags2
    then prerr_endline ("innerclasses flags differ ("
			 ^access_flags inner_class_access_flags1^","
			 ^access_flags inner_class_access_flags2^")");
    true
  in
    if not (eq_list (eq_inner_class cl) icl1 icl2)
    then failwith "innerclasses differ: not the same number of classes.";
    true

and eq_attrib cl a1 a2 =
  match a1,a2 with
    | AttributeCode c1, AttributeCode c2 -> eq_code cl (Some c1) (Some c2)
    | AttributeInnerClasses icl1, AttributeInnerClasses icl2 ->
	if eq_inner_classes cl icl1 icl2
	then true
	else failwith "innerclasses differ"
    | a1,a2 ->
	if compare a1 a2 = 0
	then true
	else
	  match a1,a2 with
	    | AttributeUnknown _ , _
	    | _ , AttributeUnknown _  when
		  compare
		    (JUnparse.unparse_attribute_to_strings (DynArray.of_array cl.j_consts) a1)
		    (JUnparse.unparse_attribute_to_strings (DynArray.of_array cl.j_consts) a2)
		  = 0
		-> true
	    | _,_ ->
		failwith ("attributes differ ("
			   ^(dump_to_string (fun ch -> dump_attrib ch cl)) a1^","
			   ^(dump_to_string (fun ch -> dump_attrib ch cl)) a2^")")

and eq_attribs cl al1 al2 =
  if not (eq_list (eq_attrib cl) al1 al2)
  then
    begin
      prerr_endline (attributes_to_string cl al1);
      prerr_endline ("************");
      prerr_endline (attributes_to_string cl al2);
      failwith ("attributes' length differ ("
		 ^string_of_int (List.length al1)^","
		 ^string_of_int (List.length al2)^")")
    end

let eq_method cl m1 m2 =
  if m1.m_name <> m2.m_name || m1.m_signature <> m2.m_signature
  then failwith ("method signatures differ ("
		  ^ signature m1.m_name (SMethod m1.m_signature)^","
		  ^ signature m2.m_name (SMethod m2.m_signature)^")");
  if List.sort compare (filter_flags [AccPublic;AccPrivate;AccProtected;AccStatic;AccFinal;AccSynchronized;AccNative;AccAbstract;AccStrict] m1.m_flags)
    <> List.sort compare m2.m_flags
  then prerr_endline ("method flags differ ("^access_flags m1.m_flags^","^access_flags m2.m_flags^")");
  (try eq_attribs cl m1.m_attributes m2.m_attributes
    with Failure msg -> failwith ("in method "^access_flags m1.m_flags^" "^signature m1.m_name (SMethod m1.m_signature)^" attributes differ: "^msg));
  if not (eq_code cl m1.m_code m2.m_code)
  then failwith ("method code differ ("^","^")");
  true

let eq_fields cl f1 f2 =
  if f1.f_name <> f2.f_name || f1.f_signature <> f2.f_signature
  then failwith ("field signatures differ("
		  ^signature f1.f_name (SValue f1.f_signature)^"<>"
		  ^signature f2.f_name (SValue f2.f_signature)^")");
  if List.sort compare (filter_flags [AccPublic;AccPrivate;AccProtected;AccStatic;AccFinal;AccVolatile;AccTransient] f1.f_flags)
    <> List.sort compare f2.f_flags
  then prerr_endline ("field flags differ ("^access_flags f1.f_flags^","^access_flags f2.f_flags^")");
  (try eq_attribs cl f1.f_attributes f2.f_attributes
    with Failure msg -> failwith ("field attributes differ: "^msg));
  true


let eq_class c1 c2 =
  let dump_super ch = function
    | None -> ()
    | Some c -> IO.printf ch "%s" (class_name c)
  in
    if c1.j_name <> c2.j_name
    then failwith ("class names differ ("^class_name c1.j_name^","^class_name c2.j_name^")");
    if c1.j_super <> c2.j_super
    then failwith ("super classes differ ("^(dump_to_string dump_super) c1.j_super ^","^(dump_to_string dump_super) c2.j_super^")");
    if compare c1.j_consts c2.j_consts <> 0
    then
      begin
	prerr_endline ((dump_to_string dump_constantpool) c1.j_consts);
	prerr_endline ((dump_to_string dump_constantpool) c2.j_consts);
	Pervasives.flush stderr;
	failwith ("constant pools differ")
      end;
    if List.sort compare c1.j_interfaces <> List.sort compare c2.j_interfaces
    then failwith ("interfaces differ ("^","^")");
    if List.sort compare (filter_flags [AccPublic;AccFinal;AccSynchronized;AccInterface;AccAbstract] c1.j_flags)
      <> List.sort compare c2.j_flags
    then prerr_endline ("class flags differ ("^access_flags c1.j_flags^","^access_flags c2.j_flags^")");

    if not (eq_list (eq_fields c1) c1.j_fields c2.j_fields)
    then failwith ("numbers fields of differ ("
		    ^string_of_int (List.length c1.j_fields)^","
		    ^string_of_int (List.length c1.j_fields)^")");

    if not (eq_list (eq_method c1) c1.j_methods c2.j_methods)
    then failwith ("numbers of methods differ ("
		    ^string_of_int (List.length c1.j_methods)^","
		    ^string_of_int (List.length c2.j_methods)^")");
    try
      eq_attribs c1 c1.j_attributes c2.j_attributes
    with Failure msg -> failwith ("class attributes differ: "^msg)

let h2l_and_l2h_conversions class_path input_files =
  let res =
    JFile.read_low
      class_path
      (fun _ c ->
	try
	  let c_high = JLow2High.low2high_class c in
	  let c_low' = JHigh2Low.high2low c_high in
	    try
	      eq_class c c_low'
	    with Failure msg ->
	      failwith ("error on "^(class_name c.JClassLow.j_name)^": "^msg)
	with Failure msg -> failwith msg)
      ()
      input_files
  in res;;

let h2l_conversions class_path input_files =
  let res =
    JFile.read_low
      class_path
      (fun _ c -> ignore (JLow2High.low2high_class c))
      ()
      input_files
  in res;;


(** {2 Tests of the program representation }*)

open JProgram

let test_jprogram class_path input_files =
  prerr_string "loading program...  ";
  let p =
    try JProgram.load_program "library.bin"
    with _ ->
      try parse_program class_path input_files
      with JProgram.Class_not_found cn -> raise (Failure ("class not found "^class_name cn))
  in
  let class_path = JFile.class_path class_path
  in 
    prerr_string "adding files... ";
    List.fold_left
      (fun p cn -> 
	JProgram.add_file
	  class_path
	  (JFile.get_class class_path cn)
	  p)
      p
      input_files


(** It should run the test suite. *)
let _ =
  let class_path = "/System/Library/Frameworks/JavaVM.framework/Versions/1.5.0/Classes/"
  and jars = ["charsets.jar";"dt.jar";"laf.jar";"classes.jar";"jce.jar";"jsse.jar";"ui.jar"]
  in let class_path_jar = String.concat ":" (List.map (fun s -> class_path^s) jars)
  in let class_path = "./:"^class_path^":"^class_path_jar
  and input_files = ["java.lang.Object"]
  in
    (* h2l_and_l2h_conversions class_path input_files; *)
    test_jprogram class_path input_files
