(*
 * This file is part of Javalib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1, with the special exception on linking described in file
 * LICENSE.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)

open JBasics
   
type jconst = [
  | `ANull (* AConstNull  *)
  | `Int of int32
  | `Long of int64
  | `Float of float
  | `Double of float
  | `Byte of int (* BIPush *)
  | `Short of int
  | `String of jstr
  | `Class of object_type
  | `MethodType of method_descriptor (* Since Java 7 *)
  | `MethodHandle of method_handle (* Since Java 7 *)
]

type jinterface_or_class = [ `Class | `Interface ]

type jopcode =

  (* Access to a local variable *)
  | OpLoad of jvm_type * int
  | OpStore of jvm_type * int
  | OpIInc of int * int

  (* Stack permutation *)
  | OpPop
  | OpPop2
  | OpDup
  | OpDupX1
  | OpDupX2
  | OpDup2
  | OpDup2X1
  | OpDup2X2
  | OpSwap

  (* Constant loading / it corresponds to instructions *const* and ldc* *)
  | OpConst of jconst

  (* Arithmetic *)
  | OpAdd of jvm_basic_type
  | OpSub of jvm_basic_type
  | OpMult of jvm_basic_type
  | OpDiv of jvm_basic_type
  | OpRem of jvm_basic_type
  | OpNeg of jvm_basic_type

  (* Logic *)
  | OpIShl (* Use an I/L argument *)
  | OpLShl
  | OpIShr
  | OpLShr
  | OpIUShr
  | OpLUShr
  | OpIAnd
  | OpLAnd
  | OpIOr
  | OpLOr
  | OpIXor
  | OpLXor

  (* Conversion *)
  | OpI2L (* Use `I of [`L | `F  | `D] *)
  | OpI2F
  | OpI2D
  | OpL2I
  | OpL2F
  | OpL2D
  | OpF2I
  | OpF2L
  | OpF2D
  | OpD2I
  | OpD2L
  | OpD2F
  | OpI2B (* Those three are different *)
  | OpI2C
  | OpI2S

  | OpCmp of [`L | `FL | `FG | `DL | `DG]

  (* Conditional jump *)
  | OpIf of [`Eq | `Ne | `Lt | `Ge | `Gt | `Le | `Null | `NonNull] * int
  | OpIfCmp of [`IEq | `INe | `ILt | `IGe | `IGt | `ILe | `AEq | `ANe] * int

  (* Unconditional jump *)
  | OpGoto of int
  | OpJsr of int
  | OpRet of int
  | OpTableSwitch of int * int32 * int32 * int array
  | OpLookupSwitch of int * (int32 * int) list

  (* Heap and static fields *)
  | OpNew of class_name
  | OpNewArray of value_type
  | OpAMultiNewArray of object_type * int (* ClassInfo, dims *)
  | OpCheckCast of object_type
  | OpInstanceOf of object_type
  | OpGetStatic of class_name * field_signature
  | OpPutStatic of class_name * field_signature
  | OpGetField of class_name * field_signature
  | OpPutField of class_name * field_signature
  | OpArrayLength
  | OpArrayLoad of jvm_array_type
  | OpArrayStore of jvm_array_type

  (* Method invocation and return *)
  | OpInvoke of [
    | `Virtual of object_type
    | `Special of jinterface_or_class * class_name
    | `Static of jinterface_or_class * class_name
    | `Interface of class_name
    | `Dynamic of bootstrap_method
    ]
    * method_signature
  | OpReturn of jvm_return_type

  (* Exceptions and threads *)
  | OpThrow
  | OpMonitorEnter
  | OpMonitorExit

  (* Other *)
  | OpNop
  | OpBreakpoint
  | OpInvalid

type jopcodes = jopcode array

(* Exception handler. *)
type exception_handler = {
  e_start : int;
  e_end : int;
  e_handler : int;
  e_catch_type : class_name option
}

type jcode = {
  c_max_stack : int;
  c_max_locals : int;
  c_code : jopcodes;
  c_exc_tbl : exception_handler list;
  c_line_number_table : (int * int) list option;
  c_local_variable_table : (int * int * string * value_type * int) list option;
  c_local_variable_type_table : (int * int * string * JSignature.fieldTypeSignature * int) list option;
  c_stack_map : stackmap_frame list option;
  c_attributes : (string * string) list;
}

let empty = {
  c_max_stack = 0;
  c_max_locals = 0;
  c_code = Array.of_list [];
  c_exc_tbl = [];
  c_line_number_table = None;
  c_local_variable_table = None;
  c_local_variable_type_table = None;
  c_stack_map = None;
  c_attributes = [];
}

let get_local_variable_info i pp code =
  match code.c_local_variable_table with
    | None -> None
    | Some lvt ->
        let offset =
          (* when an [store v] is done, [v] will have its type at the
             next program point.  Therefore, the LocalVariableTable
             only refers to [v] from the next program point.  To have
             the name and type of [v] we therefore need to look at the
             next program point. *)
          let code = code.c_code in
	    match code.(pp) with
	      | OpStore _ ->
                  let i = ref (pp + 1) in
                    while !i < Array.length code && code.(!i) = OpInvalid do
                      incr i
                    done;
                    !i - pp
	      | _ -> 0
        in
	  try
	    let (_,_,s,sign,_) =
	      List.find
		(fun (start,len,_,_,index) ->
		   pp + offset >= start
                   && pp + offset < start + len
                   && index = i
                ) lvt
            in
              Some (s,sign)
          with _ -> None

let get_source_line_number' pp lnt =
  let rec find_line prev = function
    | (start_pc,line_number)::r ->
	if (start_pc > pp) then Some prev
	else find_line line_number r
    | [] -> Some prev
  in
    try find_line (snd (List.hd lnt)) lnt
    with _ -> None

let get_source_line_number pp code =
  match code.c_line_number_table with
    | None -> None
    | Some lnt ->
        get_source_line_number' pp lnt

let renumber_instruction pp_ins n_ins pp opcode =
  let gen_offset offset =
    let pp_jump = pp + offset in
    if offset > 0 && pp_jump > pp_ins && pp_ins > pp then offset+n_ins
    else if offset < 0 && pp_jump <= pp_ins && pp_ins < pp then offset-n_ins
    else offset
  in
  match opcode with
  | OpGoto offset ->
     let offset = gen_offset offset in
     OpGoto offset
  | OpIfCmp (kind, offset) ->
     let offset = gen_offset offset in
     OpIfCmp (kind, offset)
  | OpIf (kind, offset) ->
     let offset = gen_offset offset in
     OpIf (kind, offset)
  | OpJsr offset ->
     let offset = gen_offset offset in
     OpJsr offset
  | OpLookupSwitch (default, l) ->
     let default = gen_offset default in
     OpLookupSwitch (default,
                     List.map (fun (mch, offset) -> (mch, gen_offset offset)) l)
  | OpTableSwitch (default, low, high, jumps) ->
     OpTableSwitch (gen_offset default, low, high,
                    Array.map (fun offset -> gen_offset offset) jumps)
  | op -> op       
      
let renumber_tables lnt lvt lvtt pp n_ins =
  let shift_line line =
    if line <= pp then line else line+n_ins
  in
  let shift_pc_length pc length =
    if pc <= pp then
      if (pc+length-1) <= pp then (pc, length)
      else (pc, length+n_ins)
    else (pc+n_ins,length)
  in
  let renumber_lvt lvt =
    match lvt with
    | None -> None
    | Some l ->
       Some (List.map
               (fun (start_pc, length, name, typ, index) ->
                 let (start_pc, length) = shift_pc_length start_pc length in
                 (start_pc, length, name, typ, index)) l)
  in
  let lnt' =
    match lnt with
    | None -> None
    | Some l ->
       Some (List.map
               (fun (l_byte, l_src) ->
                 (shift_line l_byte, l_src)) l)
  and lvt' = renumber_lvt lvt
  and lvtt' = renumber_lvt lvtt in
  (lnt', lvt', lvtt')

let get_offset_delta frame =
  match frame with
  | SameFrame index -> index
  | SameLocals (index, _) -> index-64
  | SameLocalsExtended (_, offset, _) -> offset
  | ChopFrame (_, offset) -> offset
  | SameFrameExtended (_, offset) -> offset
  | AppendFrame (_, offset, _) -> offset
  | FullFrame (_, offset, _, _) -> offset

let get_stackmap_pps (stackmap : stackmap_frame list) =
  let l = List.fold_left
            (fun acc frame -> ((List.hd acc)
                               + (get_offset_delta frame) + 1)::acc
            ) [-1] stackmap in
  List.tl (List.rev l)

let renumber_stackmap smt pp_ins n_ins =
  let shift_frame frame =
    match frame with
    | SameFrame index ->
       let offset = index+n_ins in
       if offset <= 63 then SameFrame offset
       else SameFrameExtended (251, offset)
    | SameLocals (index, v) ->
       let offset = index-64+n_ins in
       if offset <= 63 then SameLocals (index+n_ins, v)
       else SameLocalsExtended (247, offset, v)
    | SameLocalsExtended (index, offset, v) ->
       SameLocalsExtended (index, offset+n_ins, v)
    | ChopFrame (index, offset) -> ChopFrame (index, offset+n_ins)
    | SameFrameExtended (index, offset) ->
       SameFrameExtended (index, offset+n_ins)
    | AppendFrame (index, offset, v) -> AppendFrame (index, offset+n_ins, v)
    | FullFrame (index, offset, v1, v2) -> FullFrame (index, offset+n_ins, v1, v2)
  in
  match smt with
  | None -> None
  | Some stackmap ->
     let pps = get_stackmap_pps stackmap in
     let first = ref true in
     let stackmap' = List.map2
                       (fun pp_frame frame ->
                         if pp_frame > pp_ins && !first then
                           (first := false;
                            shift_frame frame)
                         else frame
                       ) pps stackmap in
     Some stackmap'

let renumber_exception_table (exn_table : exception_handler list) pp_ins n_ins =
  let shift_handler handler =
    let (e_start, e_end) =
      if handler.e_start > pp_ins then
        (handler.e_start+n_ins, handler.e_end+n_ins)
      else if handler.e_end > pp_ins then
        (handler.e_start, handler.e_end+n_ins)
      else (handler.e_start, handler.e_end)
    and e_handler =
      if handler.e_handler > pp_ins then
        handler.e_handler+n_ins
      else handler.e_handler
    in { e_start = e_start;
         e_end = e_end;
         e_handler = e_handler;
         e_catch_type = handler.e_catch_type }
  in
  List.map (fun handler -> shift_handler handler) exn_table

let patch_switch pp_ins n_ins opcodes =
  let first_switch_pp opcodes =
    let contains_switch = ref false in
    let i = ref 0 in
    while !i < Array.length opcodes && not(!contains_switch) do
      match opcodes.(!i) with
      | OpTableSwitch _ | OpLookupSwitch _ ->
         contains_switch := true
      | _ -> i := !i + 1
    done;
    if !contains_switch then !i else -1
  in
  let pp_switch = first_switch_pp opcodes in
  if pp_switch >= pp_ins then
    let ins_mod4 = n_ins mod 4 in
    if ins_mod4 == 0 then
      []
    else if ins_mod4 == 1 then
      [OpNop; OpNop; OpNop]
    else if ins_mod4 == 2 then
      [OpNop; OpNop]
    else
      [OpNop]
  else
    []

let count_opinvalids_before_next_op opcodes pp =
  let n_ops = Array.length opcodes in
  let n = ref 0 in
  let () = while (pp + !n + 1 < n_ops && opcodes.(pp + !n + 1) = OpInvalid) do
             n := !n + 1
           done
  in !n

let check_not_invalid opcodes pp message =
  let op = opcodes.(pp) in
  match op with
  | OpInvalid -> failwith message
  | _ -> ()

let replace_code code pp ins_opcodes =
  let old_opcodes = code.c_code in
  let () = check_not_invalid old_opcodes pp
             "Cannot insert a code fragment in place of an OpInvalid." in
  let n_pp = 1 + (count_opinvalids_before_next_op old_opcodes pp) in
  let ins_opcodes = (patch_switch pp ((List.length ins_opcodes)-n_pp) old_opcodes)
                    @ ins_opcodes in
  let n_ins = List.length ins_opcodes in
  let n_old = Array.length old_opcodes in
  let old_opcodes = Array.mapi
                      (fun pp0 opcode ->
                        renumber_instruction pp (n_ins-n_pp) pp0 opcode) old_opcodes in
  let new_opcodes = Array.make (n_old + n_ins - n_pp) OpInvalid in
  let () = Array.blit old_opcodes 0 new_opcodes 0 pp in
  let () = Array.blit old_opcodes (pp+n_pp) new_opcodes (pp+n_ins) (n_old-pp-n_pp) in
  let () = Array.blit (Array.of_list ins_opcodes) 0 new_opcodes pp n_ins in
  let (lnt, lvt, lvtt) = (renumber_tables
                            code.c_line_number_table
                            code.c_local_variable_table
                            code.c_local_variable_type_table pp (n_ins-n_pp)) in
  let stackmap = renumber_stackmap code.c_stack_map pp (n_ins-n_pp) in
  let exn_table = renumber_exception_table code.c_exc_tbl pp (n_ins-n_pp) in
  { code with c_code = new_opcodes;
              c_line_number_table = lnt;
              c_local_variable_table = lvt;
              c_local_variable_type_table = lvtt;
              c_stack_map = stackmap;
              c_exc_tbl = exn_table }

let insert_code code pp ins_opcodes =
  let old_opcodes = code.c_code in
  let () = check_not_invalid old_opcodes pp
             "Cannot insert a code fragment before an OpInvalid." in
  let n_pp = 1 + (count_opinvalids_before_next_op old_opcodes pp) in
  let curr_op = Array.sub old_opcodes pp n_pp in
  let () = curr_op.(0) <- renumber_instruction (pp-1)
                            (List.length ins_opcodes) pp curr_op.(0) in
  replace_code code pp (ins_opcodes @ (Array.to_list curr_op))
       
type lambda_info = {
  functional_interface : class_method_signature;
  captured_arguments : value_type list;
  checkcast_arguments : value_type list;
  lambda_handle : method_handle;
}

let get_bm_args bm =
  match bm.bm_args with
  | (`MethodType invoked_md) :: (`MethodHandle mh)
    :: (`MethodType checkcast_md) :: [] ->
     (invoked_md, checkcast_md, mh)
  | _ -> failwith "Bad bootstrap arguments for a lambda expression."

let build_lambda_info bm ms =
  let m_name = ms_name ms in
  let captured_args = ms_args ms in
  let interface_name = match ms_rtype ms with
    | Some (TObject (TClass cn)) -> cn
    | _ -> failwith "Bad functional interface name in invokedynamic parameter."
  in
  let (invoked_md, checkcast_md, mh) = get_bm_args bm in
  { functional_interface = make_cms interface_name
                             (make_ms m_name
                                (md_args invoked_md)
                                (md_rtype invoked_md));
    captured_arguments = captured_args;
    checkcast_arguments = md_args checkcast_md;
    lambda_handle = mh }

module BCV = struct
  type typ = JBasics.verification_type

  (******* STACK MANIPULATION **************)

  (** [Bad_stack] is raised in case the stack does not fit the length/content
      constraint of the bytecode instruction being transformed. *)
  exception Bad_stack

  (* Returns the top element of the stack *)
  let top = function [] -> raise Bad_stack | x :: _ -> x

  (* Pops one element off the stack *)
  let pop = function [] -> raise Bad_stack | _ :: q -> q

  (* Pops n elements off the stack *)
  let rec popn n s = if n = 0 then s else pop (popn (n - 1) s)

  let pop2 s = popn 2 s

  let pop3 s = popn 3 s

  (**************** STACK TYPE INFERENCE ****)

  exception Subroutine

  exception End_of_method

  let next c i =
    try
      let k = ref (i + 1) in
      while c.(!k) = OpInvalid do
        incr k
      done ;
      !k
    with _ -> raise End_of_method

  (*Computes successors of instruction i. They can be several successors in case
   * of conditionnal instruction.*)
  let normal_next opcodes i =
    match opcodes.(i) with
    | OpIf (_, n) | OpIfCmp (_, n) ->
       [next opcodes i; i + n]
    | OpGoto n ->
       [i + n]
    | OpJsr _ | OpRet _ ->
       raise Subroutine
    | OpTableSwitch (default, _, _, table) ->
       List.map (( + ) i) (default :: Array.to_list table)
    | OpLookupSwitch (default, npairs) ->
       List.map (( + ) i) (default :: List.map snd npairs)
    | OpReturn _ ->
       []
    | OpThrow ->
       []
    | OpBreakpoint ->
       failwith "breakpoint"
    | OpInvalid ->
       failwith "invalid"
    | _ ->
       [next opcodes i]

  let compute_handlers code i =
    let handlers = code.c_exc_tbl in
    let handlers =
      List.filter (fun e -> e.e_start <= i && i < e.e_end) handlers
    in
    let handlers = List.map (fun e -> (e.e_handler, e.e_catch_type)) handlers in
    handlers

  (* The first element is the stack, the second one is the local var map. *)
  type t = typ list * typ Ptmap.t

  (* Environment to store the class hierarchy. Only classes should be
     stored, not interfaces. *)
  type env = class_name ClassMap.t

  let rec get_rev_superclasses env cn l =
    if cn_equal cn java_lang_object then
      java_lang_object :: l
    else
      if ClassMap.mem cn env then
        get_rev_superclasses env (ClassMap.find cn env) (cn :: l)
      else
        failwith "Bad Class Hierarchy"

  let get_rev_superclasses env cn = get_rev_superclasses env cn []

  let rec last_common_element l1 l2 e =
    match l1,l2 with
    | [], _ | _, [] -> e
    | hd1::tl1, hd2::tl2 when cn_equal hd1 hd2 -> last_common_element tl1 tl2 hd1
    | _ -> e

  let lub_cn (e:env) cn1 cn2 =
    if cn_equal cn1 cn2 then cn1
    else
      if ClassMap.mem cn1 e && ClassMap.mem cn2 e then
        let sup1 = get_rev_superclasses e cn1 in
        let sup2 = get_rev_superclasses e cn2 in
        last_common_element sup1 sup2 java_lang_object
      else
        java_lang_object

  let rec lub_object_type (e:env) o1 o2 =
    match o1,o2 with
    | TClass cn1, TClass cn2 -> TClass (lub_cn e cn1 cn2)
    | TArray (TBasic b1), TArray (TBasic b2) ->
       if b1 = b2 then o1 else TClass java_lang_object
    | TArray (TObject o1'), TArray (TObject o2') ->
       TArray (TObject (lub_object_type e o1' o2'))
    | _ -> TClass java_lang_object

  let lub (e:env) x y =
    match x with
    | VTop | VInteger | VFloat | VDouble | VLong
      | VUninitializedThis | VUninitialized _ ->
       if y = x then x else VTop
    | VNull ->
       (match y with
        | VTop | VInteger | VFloat | VDouble | VLong
          | VUninitializedThis | VUninitialized _-> VTop
        | _ -> y)
    | VObject o1 ->
       (match y with
        | VObject o2 -> VObject (lub_object_type e o1 o2)
        | _ -> VTop
       )

  let lub (e:env) (s1, l1) (s2, l2) =
    let l = ref Ptmap.empty in
    let () = Ptmap.iter (fun i v ->
                 if Ptmap.mem i l2 then l := Ptmap.add i (lub e v (Ptmap.find i l2)) !l
               ) l1 in
    (List.map2 (lub e) s1 s2, !l)

  let conv = function
    | TObject o -> VObject o
    | TBasic jbt ->
       (match jbt with
        | `Int | `Short | `Char | `Byte | `Bool -> VInteger
        | `Float -> VFloat
        | `Long -> VLong
        | `Double -> VDouble
       )

  let conv_array_type t =
    match t with
    | `Int | `Short | `Char | `Int2Bool | `ByteBool -> VInteger
    | `Float -> VFloat
    | `Object -> VObject (TClass java_lang_object)
    | `Long -> VLong
    | `Double -> VDouble

  let basic = function
    | `Int2Bool ->
       VInteger
    | `Long ->
       VLong
    | `Double ->
       VDouble
    | `Float ->
       VFloat

  let java_lang_string = make_cn "java.lang.String"
  let java_lang_class = make_cn "java.lang.Class"
  let java_lang_invoke_method_handle = make_cn "java.lang.invoke.MethodHandle"
  let java_lang_invoke_method_type = make_cn "java.lang.invoke.MethodType"

  let get l n = try Ptmap.find n l with Not_found -> assert false

  let upd l n t = Ptmap.add n t l

  exception ArrayContent

  let array_content i t = function
    | VObject (TArray v) ->
       conv v
    | VNull ->
       conv_array_type t
    | _ ->
       Printf.printf "\n\nbad array_content at %d\n\n\n" i ;
       raise ArrayContent

  let replace_stack_locals v_in v_out s l =
    (List.map (fun v -> if v = v_in then v_out else v) s,
     Ptmap.map (fun v -> if v = v_in then v_out else v) l)

  let next opcodes i =
    match opcodes.(i) with
    | OpNop -> (
      function (s, l) -> (s, l) )
    | OpConst x ->
       fun (s, l) ->
       let c =
         match x with
         | `ANull ->
            VNull
         | `String _ -> VObject (TClass java_lang_string)
         | `Class _ -> VObject (TClass java_lang_class)
         | `MethodHandle _ -> VObject (TClass java_lang_invoke_method_handle)
         | `MethodType _ -> VObject (TClass java_lang_invoke_method_type)
         | `Byte _ | `Short _ | `Int _ ->
            VInteger
         | `Long _ ->
            VLong
         | `Float _ ->
            VFloat
         | `Double _ ->
            VDouble
       in
       (c :: s, l)
    | OpLoad (_, n) ->
       fun (s, l) -> (get l n :: s, l)
    | OpArrayLoad t ->
       fun (s, l) -> (array_content i t (top (pop s)) :: pop2 s, l) (* To Check *)
    | OpStore (_, n) ->
       fun (s, l) -> (pop s, upd l n (top s))
    | OpArrayStore _ ->
       fun (s, l) -> (pop3 s, l)
    | OpPop ->
       fun (s, l) -> (pop s, l)
    | OpPop2 ->
       fun (s, l) ->
       ((match top s with VLong | VDouble -> pop s | _ -> pop2 s), l)
    | OpDup ->
       fun (s, l) -> (top s :: s, l)
    | OpDupX1 ->
       fun (s, l) -> (top s :: top (pop s) :: top s :: pop2 s, l)
    | OpDupX2 ->
       (fun (s, l) ->
         match top (pop s) with
         | VLong | VDouble ->
            (top s :: top (pop s) :: top s :: pop2 s, l)
         | _ ->
            (top s :: top (pop s) :: top (pop2 s) :: top s :: pop3 s, l))
    | OpDup2 ->
       (fun (s, l) ->
         match top s with
         | VLong | VDouble ->
            (top s :: s, l)
         | _ ->
            (top s :: top (pop s) :: top s :: top (pop s) :: pop2 s, l))
    | OpDup2X1 ->
       (fun (s, l) ->
          match top s with
          | VLong | VDouble ->
             (top s :: top (pop s) :: top s :: pop2 s, l)
          | _ ->
             (top s
              :: top (pop s)
              :: top (pop2 s)
              :: top s
              :: top (pop s)
              :: pop3 s
             , l))
    | OpDup2X2 ->
       (fun (s, l) ->
         match top s with
         | VLong | VDouble -> (
           match top (pop s) with
           | VLong | VDouble ->
              (top s :: top (pop s) :: top s :: pop2 s, l)
           | _ ->
              (top s :: top (pop s) :: top (pop2 s) :: top s :: pop3 s, l)) 
         | _ -> (
           match top (pop2 s) with
           | VLong | VDouble ->
              (top s
               :: top (pop s)
               :: top (pop2 s)
               :: top s
               :: top (pop s)
               :: pop3 s
              , l)
           | _ ->
              ( top s
                :: top (pop s)
                :: top (pop2 s)
                :: top (pop3 s)
                :: top s
                :: top (pop s)
                :: pop (pop3 s)
              , l ))
        )
    | OpSwap ->
       fun (s, l) -> (top (pop s) :: top s :: pop2 s, l)
    | OpAdd k | OpSub k | OpMult k | OpDiv k | OpRem k ->
       fun (s, l) -> (basic k :: pop2 s, l)
    | OpNeg k ->
       fun (s, l) -> (basic k :: pop s, l)
    | OpIShl | OpIShr | OpIAnd | OpIOr | OpIXor | OpIUShr ->
       fun (s, l) -> (VInteger :: pop2 s, l)
    | OpLShr | OpLShl ->
       fun (s, l) -> (pop s, l)
    | OpLAnd | OpLOr | OpLXor | OpLUShr ->
       fun (s, l) -> (VLong :: pop2 s, l)
    | OpIInc (_, _) ->
       fun (s, l) -> (s, l)
    | OpI2L ->
       fun (s, l) -> (VLong :: pop s, l)
    | OpI2F ->
       fun (s, l) -> (VFloat :: pop s, l)
    | OpI2D ->
       fun (s, l) -> (VDouble :: pop s, l)
    | OpL2I ->
       fun (s, l) -> (VInteger :: pop s, l)
    | OpL2F ->
       fun (s, l) -> (VFloat :: pop s, l)
    | OpL2D ->
       fun (s, l) -> (VDouble :: pop s, l)
    | OpF2I ->
       fun (s, l) -> (VInteger :: pop s, l)
    | OpF2L ->
       fun (s, l) -> (VLong :: pop s, l)
    | OpF2D ->
       fun (s, l) -> (VDouble :: pop s, l)
    | OpD2I ->
       fun (s, l) -> (VInteger :: pop s, l)
    | OpD2L ->
       fun (s, l) -> (VLong :: pop s, l)
    | OpD2F ->
       fun (s, l) -> (VFloat :: pop s, l)
    | OpI2B ->
       fun (s, l) -> (VInteger :: pop s, l)
    | OpI2C ->
       fun (s, l) -> (VInteger :: pop s, l)
    | OpI2S ->
       fun (s, l) -> (VInteger :: pop s, l)
    | OpCmp _ ->
       fun (s, l) -> (VInteger :: pop2 s, l)
    | OpIf (_, _) ->
       fun (s, l) -> (pop s, l)
    | OpIfCmp (_, _) ->
       fun (s, l) -> (pop2 s, l)
    | OpGoto _ ->
       fun (s, l) -> (s, l)
    | OpJsr _ ->
       raise Subroutine
    | OpRet _ ->
       raise Subroutine
    | OpTableSwitch _ ->
       fun (s, l) -> (pop s, l)
    | OpLookupSwitch _ ->
       fun (s, l) -> (pop s, l)
    | OpReturn _ ->
       fun (s, l) -> (s, l)
    | OpGetField (_, fs) ->
        fun (s, l) -> (conv (fs_type fs) :: pop s, l)
    | OpGetStatic (_, fs) ->
        fun (s, l) -> (conv (fs_type fs) :: s, l)
    | OpPutStatic _ ->
        fun (s, l) -> (pop s, l)
    | OpPutField _ ->
        fun (s, l) -> (pop2 s, l)
    | OpInvoke (x, ms) -> (
        fun (s, l) ->
          let (s, l) =
            match x with
            | `Dynamic _ | `Static _ ->
               (popn (List.length (ms_args ms)) s, l)
            | `Special (_, cn) when (ms_name ms = "<init>") ->
               let s = popn (List.length (ms_args ms)) s in
               (match top s with
                | VUninitialized i ->
                   (match opcodes.(i) with
                    | OpNew cn ->
                       replace_stack_locals (VUninitialized i)
                         (VObject (TClass cn)) (pop s) l
                    | _ -> assert false)
                | VUninitializedThis ->
                   replace_stack_locals VUninitializedThis
                     (VObject (TClass cn)) (pop s) l
                | _ -> (pop s, l)
               )
            | _ ->
               (popn (List.length (ms_args ms)) (pop s), l)
          in
          match ms_rtype ms with None -> (s, l) | Some t -> (conv t :: s, l) )
    | OpNew _ ->
        fun (s, l) -> (VUninitialized i :: s, l)
    | OpNewArray t ->
        fun (s, l) -> (VObject (TArray t) :: pop s, l)
    | OpArrayLength ->
        fun (s, l) -> (VInteger :: pop s, l)
    | OpThrow ->
        fun (s, l) -> (s, l)
    | OpCheckCast t ->
        fun (s, l) -> (conv (TObject t) :: pop s, l)
    | OpInstanceOf _ ->
        fun (s, l) -> (VInteger :: pop s, l)
    | OpMonitorEnter ->
        fun (s, l) -> (pop s, l)
    | OpMonitorExit ->
        fun (s, l) -> (pop s, l)
    | OpAMultiNewArray (o, b) ->
        fun (s, l) -> (VObject o :: popn b s, l)
    | OpBreakpoint ->
        failwith "breakpoint"
    | OpInvalid ->
        failwith "invalid"

  let init cn ms is_static =
    let rec aux i = function
      | [] ->
         Ptmap.empty
      | v :: q ->
         (match v with
          | TBasic `Long  | TBasic `Double ->
             Ptmap.add i (conv v) (aux (i + 2) q)
          | _ ->
             Ptmap.add i (conv v) (aux (i + 1) q))
    in
    if is_static then ([], aux 0 (ms_args ms))
    else ([], Ptmap.add 0 (VObject (TClass cn)) (aux 1 (ms_args ms)))
    
  let run (e:env) cn ms is_static code =
    let rec array_fold f b t i =
      if i >= 0 then f i t.(i) (array_fold f b t (i - 1)) else b
    in
    let array_fold f b t = array_fold f b t (Array.length t - 1) in
    let ws =
      array_fold
        (fun i op ws -> if op = OpInvalid then ws else Ptset.add i ws)
        Ptset.empty code.c_code
    in
    let types : t option array = Array.make (Array.length code.c_code) None in
    let upd sl' ws i =
      match types.(i) with
      | None ->
         types.(i) <- Some sl' ; Ptset.add i ws
      | Some sl ->
         let sl' = lub e sl sl' in
         if sl = sl' then ws else ( types.(i) <- Some sl' ; Ptset.add i ws )
    in
    let rec loop ws =
      if Ptset.is_empty ws then ()
      else
        let i = Ptset.min_elt ws in
        let ws = Ptset.remove i ws in
        match types.(i) with
        | Some sl ->
           let sl' = next code.c_code i sl in
           let ws = List.fold_left (upd sl') ws (normal_next code.c_code i) in
           let ws = List.fold_left (fun ws (i, catch_t) ->
                        let cn = match catch_t with
                              | None -> java_lang_object
                              | Some cn -> cn in
                        upd ([VObject (TClass cn)], snd sl') ws i
                      ) ws (compute_handlers code i) in
           loop ws
        | None ->
           loop ws
    in
    assert (Array.length types > 0) ;
    types.(0) <- Some (init cn ms is_static) ;
    loop ws ;
    types

  let get_n_stack s =
    List.fold_left (fun n t ->
        match t with
        | VLong | VDouble -> n+2
        | _ -> n+1
      ) 0 s

  let get_n_locals l =
    let l = List.sort (fun (a,_) (b,_) -> compare b a) (Ptmap.elements l) in
    match l with
    | [] -> 0
    | (n,_) :: _ -> n+1
                  
  let get_max_stack types =
    Array.fold_left (fun n sl ->
        match sl with
        | None -> n
        | Some (s,_) -> max n (get_n_stack s)) 0 types

  let get_max_locals types =
    Array.fold_left (fun n sl ->
        match sl with
        | None -> n
        | Some (_,l) -> max n (get_n_locals l)) 0 types

  let get_jump_targets op i =
    match op with
    | OpIf (_, n) | OpIfCmp (_, n) ->
       [i + n]
    | OpGoto n ->
       [i + n]
    | OpTableSwitch (default, _, _, table) ->
       List.map (( + ) i) (default :: Array.to_list table)
    | OpLookupSwitch (default, npairs) ->
       List.map (( + ) i) (default :: List.map snd npairs)
    | OpJsr _ | OpRet _ ->
       raise Subroutine
    | _ ->
       []

  let get_handlers_targets code =
    let handlers = code.c_exc_tbl in
    List.map (fun e -> e.e_handler) handlers

  let get_branching_set code =
    let s_targets = ref Ptset.empty in
    let () = Array.iteri (fun i op ->
                 List.iter (fun j ->
                     s_targets := Ptset.add j !s_targets
                   ) (get_jump_targets op i)
               ) code.c_code in
    let () = List.iter (fun i ->
                 s_targets := Ptset.add i !s_targets)
               (get_handlers_targets code) in
    Ptset.elements !s_targets

end

let map_offset_deltas l =
  if l = [] then []
  else
    let _ = assert (List.hd l > 0) in
    let i_tmp = ref (-1) in
    let l_off = ref [] in
    let () = List.iter (fun i ->
                 let offset = i - !i_tmp - 1 in
                 i_tmp := i;
                 l_off := offset :: !l_off
               ) l in
    List.rev !l_off

let locals_to_list l =
  let l_rev = List.sort (fun (a,_) (b,_) -> compare b a) (Ptmap.elements l) in
  let max_l = match l_rev with
    | [] -> 0
    | (n,_)::_ -> n+1
  in
  let l_arr = Array.make max_l (Some VTop) in
  let () = List.iter (fun (n,v) -> Array.set l_arr n (Some v)) l_rev in
  let prev = ref (Some VTop) in
  let () = Array.iteri (fun i v ->
               match !prev with
               | Some VLong | Some VDouble -> prev := v; Array.set l_arr i None
               | _ -> prev := v
             ) l_arr in
  let r = List.filter (fun v -> match v with
                                | None -> false
                                | _ -> true) (Array.to_list l_arr) in
  List.map (fun v -> match v with
                     | None -> assert false
                     | Some v -> v) r

let gen_stack_map_info e cn ms is_static code =
  let types = BCV.run e cn ms is_static code in
  let max_stack = BCV.get_max_stack types in
  let max_locals = BCV.get_max_locals types in
  let targets = BCV.get_branching_set code in
  let offset_deltas = map_offset_deltas targets in
  let stackmaps = ref [] in
  let () = List.iter2 (fun tg offs ->
               match types.(tg) with
               | None -> ()
               | Some (s, l) ->
                  stackmaps := FullFrame (255, offs, locals_to_list l, s) :: !stackmaps
             ) targets offset_deltas in
  { code with c_max_stack = max_stack;
              c_max_locals = max_locals;
              c_stack_map = match !stackmaps with
                            | [] -> None
                            | l -> Some l }
