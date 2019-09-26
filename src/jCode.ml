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
  let gen_opcode offset op_plus op_minus op =
    let pp_jump = pp + offset in
     if offset > 0 && pp_jump > pp_ins && pp_ins > pp then op_plus
     else if offset < 0 && pp_jump < pp_ins && pp_ins < pp then op_minus
     else op
  in
  match opcode with
  | OpGoto offset as op ->
     gen_opcode offset (OpGoto (offset+n_ins)) (OpGoto (offset-n_ins)) op
  | OpIfCmp (kind, offset) as op ->
     gen_opcode offset (OpIfCmp (kind, offset+n_ins)) (OpIfCmp (kind, offset-n_ins)) op
  | op -> op       
      
let insert_code_fragment code pp ins_opcodes =
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
  let n_ins = Array.length ins_opcodes in
  let old_opcodes = Array.mapi
                      (fun pp0 opcode ->
                        renumber_instruction pp (n_ins-n_pp) pp0 opcode) old_opcodes in
  let new_opcodes = Array.make (n_old + n_ins - n_pp) OpInvalid in
  let () = Array.blit old_opcodes 0 new_opcodes 0 pp in
  let () = Array.blit old_opcodes (pp+n_pp) new_opcodes (pp+n_ins) (n_old-pp-n_pp) in
  let () = Array.blit ins_opcodes 0 new_opcodes pp n_ins in
  { code with c_code = new_opcodes }
