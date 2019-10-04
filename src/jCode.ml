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

let replace_code code pp ins_opcodes =
  let old_opcodes = code.c_code in
  let old_op = old_opcodes.(pp) in
  let () = match old_op with
    | OpInvalid -> failwith "Cannot insert a code fragment in place of an OpInvalid."
    | _ -> () in
  let n_old = Array.length old_opcodes in
  let n_pp = let n = ref 1 in
             let () = while (pp + !n < n_old && old_opcodes.(pp + !n) = OpInvalid) do
                        n := !n + 1
                      done in !n in
  let ins_opcodes = (patch_switch pp ((List.length ins_opcodes)-n_pp) old_opcodes)
                    @ ins_opcodes in
  let n_ins = List.length ins_opcodes in
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
  let old_op = old_opcodes.(pp) in
  let () = match old_op with
    | OpInvalid -> failwith "Cannot insert a code fragment before an OpInvalid."
    | _ -> () in
  let n_old = Array.length old_opcodes in
  let n_pp = let n = ref 1 in
             let () = while (pp + !n < n_old && old_opcodes.(pp + !n) = OpInvalid) do
                        n := !n + 1
                      done in !n in
  let curr_op = Array.sub old_opcodes pp n_pp in
  let () = curr_op.(0) <- renumber_instruction (pp-1)
                            (List.length ins_opcodes) pp curr_op.(0) in
  replace_code code pp (ins_opcodes @ (Array.to_list curr_op))
