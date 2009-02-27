(*
 *  This file is part of JavaLib
 *  Copyright (c)2007-2008 Université de Rennes 1 / CNRS
 *  Tiphaine Turpin <first.last@irisa.fr>
 *  Laurent Hubert <first.last@irisa.fr>
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

open JBasics
open JClass
open JProgram

module PP = struct
  type t = {cl:interface_or_class;
	    meth:concrete_method;
	    pc:int;}

  let eqc = JProgram.equal
  let eqm = (==)
  let eqi = (=)

  let equal pp1 pp2 =
    eqm pp1.meth pp2.meth
    && eqi pp1.pc pp2.pc
    && eqc pp1.cl pp2.cl

  let hash pp1 =
    Hashtbl.hash (get_index pp1.cl,pp1.meth.cm_index,pp1.pc)


  let compare pp1 pp2 =
    if equal pp1 pp2
    then 0
    else
      match compare (get_index pp1.cl) (get_index pp2.cl) with
	| 0 ->
	    begin
	      match compare pp1.meth.cm_index pp2.meth.cm_index with
		| 0 -> compare pp1.pc pp2.pc
		| n -> n
	    end
	| n -> n

  exception NoCode of (class_name * method_signature)

  let to_string (pp:t) : string =
    let s = pp.meth.cm_signature in
    JDumpBasics.class_name (get_name pp.cl)
      ^ "."^ s.ms_name ^"("
      ^ (String.concat ", "
	    (List.map JDumpBasics.value_signature s.ms_parameters))
      ^ "): " ^ string_of_int pp.pc

  let pprint fmt pp : unit =
    Format.pp_print_string fmt (to_string pp)

  let get_class (pp:t) : interface_or_class =
    pp.cl

  let get_meth (pp:t) : concrete_method =
    pp.meth

  let get_pc (pp:t) : int = pp.pc

  let get_pp cl' meth' pc' : t = {cl=cl';meth=meth';pc=pc';}

  let get_first_pp prog cn ms : t =
    match get_interface_or_class prog cn with
      | `Interface {i_initializer = Some m} as c when m.cm_signature = ms ->
	  {cl=c;meth=m;pc=0;}
      | `Class c as cl' when
	  let msi = prog.dictionary.get_ms_index ms in
	    MethodMap.mem msi c.c_methods ->
	  begin
	    let msi = prog.dictionary.get_ms_index ms in
	      match MethodMap.find msi c.c_methods with
		| ConcreteMethod m->
		    {cl=cl';
		     meth=m;
		     pc=0;}
		| _ -> raise (NoCode (cn,ms))
	  end
      | _ -> raise (NoCode (cn,ms))

  let get_first_pp_wp c msi : t =
    match get_method c msi with
      | ConcreteMethod ({cm_implementation = Java _} as m) ->
	  {cl=c;meth=m;pc=0;}
      | ConcreteMethod ({cm_implementation = Native} as m) ->
	  raise (NoCode (get_name c,m.cm_signature))
      | AbstractMethod m ->
	  raise (NoCode (get_name c,m.am_signature))

  let goto_absolute pp i : t = {pp with pc=i;}

  let goto_relative pp jmp : t ={pp with pc=pp.pc+jmp;}

end

open PP
type pp = PP.t

let get_code (pp:pp): opcodes =
  match pp.meth.cm_implementation with
    | Java c -> (Lazy.force c).c_code
    | Native -> raise (NoCode (get_name pp.cl,pp.meth.cm_signature))

let get_opcode (pp:pp) :opcode = (get_code pp).(pp.pc)

let next_instruction pp =
  let opcodes = get_code pp
  and i = ref (succ pp.pc)
  in
    while opcodes.(!i) = OpInvalid do
      incr i;
    done;
    goto_absolute pp !i

let normal_successors pp =
  match get_opcode pp with
    | OpIf (_,l)
    | OpIfCmp (_,l) ->
	[next_instruction pp; goto_relative pp l]
    | OpJsr l
    | OpGoto l -> [goto_relative pp l]
    | OpRet _ -> (* all instruction following a jsr are returned. *)
	let code = get_code pp in
	let i = ref 0 in
	let l = ref [] in
	  while !i < Array.length code do
	    begin
	      match code.(!i) with
		| OpJsr _ ->
		    l := next_instruction (goto_absolute pp !i)::!l
		| _ -> ()
	    end;
	    incr i;
	  done;
	  !l
    | OpTableSwitch (default,_,_,others) ->
	Array.fold_left
	  (fun ppl jmp -> goto_relative pp jmp::ppl)
	  [goto_relative pp default]
	  others
    | OpLookupSwitch (default,others) ->
	List.fold_left
	  (fun ppl (_,jmp) -> goto_relative pp jmp::ppl)
	  [goto_relative pp default]
	  others
    | OpThrow
    | OpReturn _ -> []
    | OpInvalid
    | OpBreakpoint ->
	raise (Class_structure_error "Instructions Invalid and Breakpoint are not authorized")
    | _ -> [next_instruction pp]



let resolve_class program cn =
  try get_interface_or_class program cn with Not_found -> raise NoClassDefFoundError

let rec resolve_field' result fsi c : unit =
  let get_interfaces = function
    | `Interface i -> i.i_interfaces
    | `Class c -> c.c_interfaces
  in
    if defines_field fsi c
    then
      begin
	if not (List.exists (JProgram.equal c) !result)
	then result := c::!result
      end
    else
      begin
	ClassMap.iter
	  (fun _ i -> resolve_field' result fsi (`Interface i))
	  (get_interfaces c);
	if !result = []
	then
	  begin
	    match super_class c with
	      | Some super -> resolve_field' result fsi (`Class super)
	      | None -> ()
	  end
      end

let resolve_field fsi c : interface_or_class list =
  let result = ref [] in
    resolve_field' result fsi c;
    !result


let rec resolve_method' msi (c:class_file) : class_file =
  if defines_method msi (`Class c)
  then c
  else
    match super_class (`Class c) with
      | Some super -> resolve_method' msi super
      | None -> raise NoSuchMethodError

let rec resolve_interface_method' ?(acc=[]) msi (c:interface_or_class) : interface_file list =
  ClassMap.fold
    (fun _ i acc ->
       if defines_method msi (`Interface i)
       then i::acc
       else resolve_interface_method' ~acc msi (`Interface i))
    (get_interfaces c)
    acc

(* TODO : like resolve_field, resolve_method should return a list in
   case the method is defined in several interfaces at the same time. *)
(* TODO : we could use c_resolve_methods or update it if there are no
   matches to see if it increases performance. According to some
   tests on the loading of soot.jar, it's not significant *)
let rec resolve_method msi (c:class_file) : interface_or_class =
  try `Class (resolve_method' msi c)
  with NoSuchMethodError ->
    match resolve_interface_method' msi (`Class c) with
      | resolved::_ -> `Interface resolved
      | [] -> match super_class (`Class c) with
	  | None -> raise NoSuchMethodError
	  | Some c' -> resolve_method msi c'


let resolve_interface_method msi (c:interface_file) : interface_or_class =
  if defines_method msi (`Interface c)
  then (`Interface c)
  else
    match resolve_interface_method' msi (`Interface c) with
      | resolved::_ -> `Interface resolved
      | [] -> `Class (resolve_method' msi c.i_super) (* super = java.lang.object *)

let resolve_all_interface_methods msi (i:interface_file) : interface_file list =
  if defines_method msi (`Interface i)
  then [i]
  else resolve_interface_method' msi (`Interface i)

let lookup_virtual_method msi (c:class_file) : class_file =
  let c' =
    try resolve_method' msi c
    with NoSuchMethodError -> raise AbstractMethodError
  in
    try
      match get_method (`Class c') msi with
	| ConcreteMethod _ -> c'
	| AbstractMethod _ -> raise AbstractMethodError
    with Not_found -> raise AbstractMethodError

let lookup_interface_method = lookup_virtual_method

let overrides_methods msi c =
  let result = ref [] in
    match c.c_super_class with
      | None -> []
      | Some c ->
	  let sc = ref c in
	    try
	      while true do
		let c = resolve_method' msi c
		in
		  result := c::!result;
		  sc :=
		    (match c.c_super_class with
		      | Some c -> c
		      | None -> raise NoSuchMethodError);
	      done;
	      assert false
	    with NoSuchMethodError ->
	      !result

let implements_method c msi =
  try
    match MethodMap.find msi c.c_methods with
      | ConcreteMethod _ -> true
      | AbstractMethod _ -> false
  with Not_found -> false

let implements_methods msi c =
  ClassMap.fold
    (fun _ i l -> resolve_all_interface_methods msi i @ l)
    c.c_interfaces
    []

let handlers program pp =
  let ioc2c = function
    | `Class c -> c
    | `Interface _ -> raise IncompatibleClassChangeError
  in
    match pp.meth.cm_implementation with
      | Java code ->
	  let is_prunable exn pp =
	    match exn.e_catch_type with
	      | None -> false
	      | Some exn_name ->
		  (* an exception handler can be pruned for an instruction if:
		     - the exception handler is a subtype of Exception and
		     - the exception handler is not a subtype nor a super-type of RuntimeException and
		     - the instruction is not a method call or if
                     the instruction is a method call which is not declared to throw
		     an exception of a subtype of the handler
		  *)
		  try
		    let exn_class =
		      ioc2c (JProgram.get_interface_or_class program exn_name)
		    and javalangexception =
		      ioc2c (JProgram.get_interface_or_class program ["java";"lang";"Exception"])
		    in
		      if not (JProgram.extends_class exn_class javalangexception)
		      then false
		      else
			let javalangruntimeexception =
			  ioc2c (JProgram.get_interface_or_class program ["java";"lang";"RuntimeException"])
			in
			  if JProgram.extends_class exn_class javalangruntimeexception
			    || JProgram.extends_class javalangruntimeexception exn_class
			  then false
			  else
			    let op = get_opcode pp in
			      try
				let (cni,msi) = retrieve_invoke_index
				  program.dictionary op in
				let cml = ClassMethSet.elements
				  (program.static_lookup cni msi (get_pc pp)) in
				let cl = List.map
				  (fun (cni,_) ->
				     ClassMap.find cni program.classes
				  ) cml
				and throws_instance_of m exn =
				  (* return true if the method m is
				     declared to throw exceptions of a
				     subtype of exn *)
				  List.exists
				    (fun e ->
				       let e = JProgram.get_interface_or_class program e
				       in JProgram.extends_class (ioc2c e) exn)
				    (match m with
				       | AbstractMethod {am_exceptions=exn_list}
				       | ConcreteMethod {cm_exceptions=exn_list} -> exn_list)
				in
				  not (List.exists
					 (fun c ->
					    let m = JProgram.get_method c msi
					    in throws_instance_of m exn_class)
					 cl)
			      with _ -> true
		  with Not_found -> false
		    (* false is safe, but it would be stange to end up
		       here as it would mean that some classes have not
		       been loaded.*)
	  in
	    List.filter
	      (fun e -> e.e_start <= pp.pc && pp.pc < e.e_end && not (is_prunable e pp))
	      (Lazy.force code).c_exc_tbl
      | Native ->
	  raise (NoCode (get_name pp.cl,pp.meth.cm_signature))
	    
let exceptional_successors program pp =
  List.map (fun e -> goto_absolute pp e.e_handler) (handlers program pp)
