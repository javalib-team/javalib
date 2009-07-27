(*
 * This file is part of JavaLib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008 Laurent Hubert (CNRS)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
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

open JClass
open JClassLow
open JBasics
open IO
open IO.BigEndian

let count =
  List.fold_left
    (fun n vt ->
       n + match vt with
	 | TBasic (`Double | `Long) -> 2
	 | _ -> 1)
    1

let opcode2instruction consts = function
	| OpNop -> JClass.OpNop
	| OpAConstNull -> JClass.OpConst `ANull
	| OpIConst v -> JClass.OpConst (`Int v)
	| OpLConst v -> JClass.OpConst (`Long v)
	| OpFConst v -> JClass.OpConst (`Float v)
	| OpDConst v -> JClass.OpConst (`Double v)
	| OpBIPush v -> JClass.OpConst (`Byte v)
	| OpSIPush v -> JClass.OpConst (`Short v)
	| OpLdc1 n
	| OpLdc1w n ->
	    JClass.OpConst
	      (match get_constant_value consts n with
		 | ConstInt c -> `Int c
		 | ConstFloat c -> `Float c
		 | ConstString c -> `String c
		 | ConstClass c -> `Class c
		 | ConstLong _ | ConstDouble _ -> raise (Class_structure_error ("Illegal constant for Ldc1: long/double")))
	| OpLdc2w n ->
	    JClass.OpConst
	      (match get_constant_value consts n with
		 | ConstInt _ | ConstFloat _ | ConstString _ | ConstClass _ ->
		     raise (Class_structure_error ("Illegal constant for Ldc2: int/float/string/class"))
		 | ConstLong c -> `Long c
		 | ConstDouble c -> `Double c)

	| OpLoad (k, l) ->
	    JClass.OpLoad ((k : jvm_basic_type :> [> jvm_basic_type]), l)
	| OpALoad l -> JClass.OpLoad (`Object, l)

	| OpArrayLoad k ->
	    JClass.OpArrayLoad (k : [`Int | other_num] :> [> `Int | other_num])
	| OpAALoad -> JClass.OpArrayLoad `Object
	| OpBALoad -> JClass.OpArrayLoad `ByteBool
	| OpCALoad -> JClass.OpArrayLoad `Char
	| OpSALoad -> JClass.OpArrayLoad `Short


	| OpStore (k, l) ->
	    JClass.OpStore ((k : jvm_basic_type :> [> jvm_basic_type]), l)
	| OpAStore l -> JClass.OpStore (`Object, l)

	| OpArrayStore k ->
	    JClass.OpArrayStore (k : [`Int | other_num] :> [> `Int | other_num])

	| OpAAStore -> JClass.OpArrayStore `Object
	| OpBAStore -> JClass.OpArrayStore `ByteBool
	| OpCAStore -> JClass.OpArrayStore `Char
	| OpSAStore -> JClass.OpArrayStore `Short

	| OpPop -> JClass.OpPop
	| OpPop2 -> JClass.OpPop2
	| OpDup -> JClass.OpDup
	| OpDupX1 -> JClass.OpDupX1
	| OpDupX2 -> JClass.OpDupX2
	| OpDup2 -> JClass.OpDup2
	| OpDup2X1 -> JClass.OpDup2X1
	| OpDup2X2 -> JClass.OpDup2X2
	| OpSwap -> JClass.OpSwap

	| OpAdd k -> JClass.OpAdd k
	| OpSub k -> JClass.OpSub k
	| OpMult k -> JClass.OpMult k
	| OpDiv k -> JClass.OpDiv k
	| OpRem k -> JClass.OpRem k
	| OpNeg k -> JClass.OpNeg k

	| OpIShl -> JClass.OpIShl
	| OpLShl -> JClass.OpLShl
	| OpIShr -> JClass.OpIShr
	| OpLShr -> JClass.OpLShr
	| OpIUShr -> JClass.OpIUShr
	| OpLUShr -> JClass.OpLUShr
	| OpIAnd -> JClass.OpIAnd
	| OpLAnd -> JClass.OpLAnd
	| OpIOr -> JClass.OpIOr
	| OpLOr -> JClass.OpLOr
	| OpIXor -> JClass.OpIXor
	| OpLXor -> JClass.OpLXor

	| OpIInc (index, incr) -> JClass.OpIInc (index, incr)

	| OpI2L -> JClass.OpI2L
	| OpI2F -> JClass.OpI2F
	| OpI2D -> JClass.OpI2D
	| OpL2I -> JClass.OpL2I
	| OpL2F -> JClass.OpL2F
	| OpL2D -> JClass.OpL2D
	| OpF2I -> JClass.OpF2I
	| OpF2L -> JClass.OpF2L
	| OpF2D -> JClass.OpF2D
	| OpD2I -> JClass.OpD2I
	| OpD2L -> JClass.OpD2L
	| OpD2F -> JClass.OpD2F
	| OpI2B -> JClass.OpI2B
	| OpI2C -> JClass.OpI2C
	| OpI2S -> JClass.OpI2S

	| OpLCmp -> JClass.OpCmp `L
	| OpFCmpL -> JClass.OpCmp `FL
	| OpFCmpG -> JClass.OpCmp `FG
	| OpDCmpL -> JClass.OpCmp `DL
	| OpDCmpG -> JClass.OpCmp `DG
	| OpIfEq pc -> JClass.OpIf (`Eq, pc)
	| OpIfNe pc -> JClass.OpIf (`Ne, pc)
	| OpIfLt pc -> JClass.OpIf (`Lt, pc)
	| OpIfGe pc -> JClass.OpIf (`Ge, pc)
	| OpIfGt pc -> JClass.OpIf (`Gt, pc)
	| OpIfLe pc -> JClass.OpIf (`Le, pc)
	| OpICmpEq pc -> JClass.OpIfCmp (`IEq, pc)
	| OpICmpNe pc -> JClass.OpIfCmp (`INe, pc)
	| OpICmpLt pc -> JClass.OpIfCmp (`ILt, pc)
	| OpICmpGe pc -> JClass.OpIfCmp (`IGe, pc)
	| OpICmpGt pc -> JClass.OpIfCmp (`IGt, pc)
	| OpICmpLe pc -> JClass.OpIfCmp (`ILe, pc)
	| OpACmpEq pc -> JClass.OpIfCmp (`AEq, pc)
	| OpACmpNe pc -> JClass.OpIfCmp (`ANe, pc)
	| OpGoto pc
        | OpGotoW pc -> JClass.OpGoto pc
	| OpJsr pc
        | OpJsrW pc -> JClass.OpJsr pc
	| OpRet l -> JClass.OpRet l

	| OpTableSwitch (def, low, high, tbl) -> JClass.OpTableSwitch  (def, low, high, tbl)
	| OpLookupSwitch (def, tbl) -> JClass.OpLookupSwitch (def, tbl)

	| OpReturn k -> JClass.OpReturn (k : jvm_basic_type :> [> jvm_basic_type])
	| OpAReturn -> JClass.OpReturn `Object
	| OpReturnVoid -> JClass.OpReturn `Void

	| OpGetStatic i ->
	    let c, n, s = get_field consts i in
	      JClass.OpGetStatic (c, {fs_name = n; fs_type = s})
	| OpPutStatic i ->
	    let c, n, s = get_field consts i in
	      JClass.OpPutStatic (c, {fs_name = n; fs_type = s})
	| OpGetField i ->
	    let c, n, s = get_field consts i in
	      JClass.OpGetField (c, {fs_name = n; fs_type = s})
	| OpPutField i ->
	    let c, n, s = get_field consts i in
	      JClass.OpPutField (c, {fs_name = n; fs_type = s})
	| OpInvokeVirtual i ->
	    let t, n, s = get_method consts i in
	      JClass.OpInvoke (`Virtual t, {ms_name = n; ms_parameters = fst s; ms_return_type = snd s})
	| OpInvokeNonVirtual i ->
	    (match get_method consts i with
	       | TClass t, n, s -> JClass.OpInvoke (`Special t, {ms_name = n; ms_parameters = fst s; ms_return_type = snd s})
	       | _ -> raise (Class_structure_error ("Illegal invokespecial: array class")))
	| OpInvokeStatic i ->
	    (match get_method consts i with
	       | TClass t, n, s -> JClass.OpInvoke (`Static t, {ms_name = n; ms_parameters = fst s; ms_return_type = snd s})
	       | _ -> raise (Class_structure_error ("Illegal invokestatic: array class")))
	| OpInvokeInterface (i, c) ->
	    let t, n, (vts, _ as s) = get_interface_method consts i in
	      if count vts <> c
	      then raise (Class_structure_error "wrong count in invokeinterface");
	      JClass.OpInvoke (`Interface t, {ms_name = n; ms_parameters = fst s; ms_return_type = snd s})

	| OpNew i -> JClass.OpNew (get_class consts i)
	| OpNewArray bt -> JClass.OpNewArray (TBasic bt)
	| OpANewArray i -> JClass.OpNewArray (TObject (get_object_type consts i))
	| OpArrayLength -> JClass.OpArrayLength
	| OpThrow -> JClass.OpThrow
	| OpCheckCast i -> JClass.OpCheckCast (get_object_type consts i)
	| OpInstanceOf i -> JClass.OpInstanceOf (get_object_type consts i)
	| OpMonitorEnter -> JClass.OpMonitorEnter
	| OpMonitorExit -> JClass.OpMonitorExit
	| OpAMultiNewArray (ot, dims) ->
	    JClass.OpAMultiNewArray (get_object_type consts ot, dims)
	| OpIfNull pc -> JClass.OpIf (`Null, pc)
	| OpIfNonNull pc -> JClass.OpIf (`NonNull, pc)
	| OpBreakpoint -> JClass.OpBreakpoint

	| OpInvalid -> JClass.OpInvalid

let opcodes2code consts opcodes =
  Array.map (opcode2instruction consts) opcodes

let instruction2opcode consts length = function
  | JClass.OpNop -> OpNop
  | JClass.OpConst x ->
      let opldc_w c =
	let index = (value_to_int consts c)
	in
          if length = 2 && index <= 0xFF
          then OpLdc1 index
          else if length = 3
          then OpLdc1w index
          else
            raise
              (Class_structure_error
                 ("OpConst cannot be encoded in "^string_of_int length^" bytes."))
      in
	(match x with
	   | `ANull -> OpAConstNull
	   | `Int v ->
	       if length = 1 && -1l <= v && v <= 5l
	       then OpIConst v
	       else opldc_w (ConstInt v)
	   | `Long v ->
	       if length = 1 && (v=0L || v=1L)
	       then OpLConst v
	       else OpLdc2w (value_to_int consts (ConstLong v))
	   | `Float v ->
	       if length = 1 && (v=0. || v=1. || v=2.)
	       then OpFConst v
	       else opldc_w (ConstFloat v)
	   | `Double v ->
	       if length = 1 && (v=0. || v=1.)
               then OpDConst v
	       else OpLdc2w (value_to_int consts (ConstDouble v))
	   | `Byte v -> OpBIPush v
	   | `Short v -> OpSIPush v
	   | `String v -> opldc_w (ConstString v)
	   | `Class v -> opldc_w (ConstClass v))

  | JClass.OpLoad (k, l) ->
      (match k with
	 | `Object -> OpALoad l
	 | #jvm_basic_type as k -> OpLoad (k, l))

  | JClass.OpArrayLoad k ->
      (match k with
	 | `Object -> OpAALoad
	 | `ByteBool -> OpBALoad
	 | `Char -> OpCALoad
	 | `Short -> OpSALoad
	 | `Int | #other_num as k -> OpArrayLoad k)

  | JClass.OpStore (k, l) ->
      (match k with
	 | `Object -> OpAStore l
	 | #jvm_basic_type as k -> OpStore (k, l))

  | JClass.OpArrayStore k ->
      (match k with
	 | `Object -> OpAAStore
	 | `ByteBool -> OpBAStore
	 | `Char -> OpCAStore
	 | `Short -> OpSAStore
	 | `Int | #other_num as k -> OpArrayStore k)

  | JClass.OpPop -> OpPop
  | JClass.OpPop2 -> OpPop2
  | JClass.OpDup -> OpDup
  | JClass.OpDupX1 -> OpDupX1
  | JClass.OpDupX2 -> OpDupX2
  | JClass.OpDup2 -> OpDup2
  | JClass.OpDup2X1 -> OpDup2X1
  | JClass.OpDup2X2 -> OpDup2X2
  | JClass.OpSwap -> OpSwap

  | JClass.OpAdd k -> OpAdd k
  | JClass.OpSub k -> OpSub k
  | JClass.OpMult k -> OpMult k
  | JClass.OpDiv k -> OpDiv k
  | JClass.OpRem k -> OpRem k
  | JClass.OpNeg k -> OpNeg k

  | JClass.OpIShl -> OpIShl
  | JClass.OpLShl -> OpLShl
  | JClass.OpIShr -> OpIShr
  | JClass.OpLShr -> OpLShr
  | JClass.OpIUShr -> OpIUShr
  | JClass.OpLUShr -> OpLUShr
  | JClass.OpIAnd -> OpIAnd
  | JClass.OpLAnd -> OpLAnd
  | JClass.OpIOr -> OpIOr
  | JClass.OpLOr -> OpLOr
  | JClass.OpIXor -> OpIXor
  | JClass.OpLXor -> OpLXor

  | JClass.OpIInc (index, incr) -> OpIInc (index, incr)

  | JClass.OpI2L -> OpI2L
  | JClass.OpI2F -> OpI2F
  | JClass.OpI2D -> OpI2D
  | JClass.OpL2I -> OpL2I
  | JClass.OpL2F -> OpL2F
  | JClass.OpL2D -> OpL2D
  | JClass.OpF2I -> OpF2I
  | JClass.OpF2L -> OpF2L
  | JClass.OpF2D -> OpF2D
  | JClass.OpD2I -> OpD2I
  | JClass.OpD2L -> OpD2L
  | JClass.OpD2F -> OpD2F
  | JClass.OpI2B -> OpI2B
  | JClass.OpI2C -> OpI2C
  | JClass.OpI2S -> OpI2S

  | JClass.OpCmp x ->
      (match x with
	 | `L -> OpLCmp
	 | `FL -> OpFCmpL
	 | `FG -> OpFCmpG
	 | `DL -> OpDCmpL
	 | `DG -> OpDCmpG)
  | JClass.OpIf (x, pc) ->
      (match x with
	   `Eq -> OpIfEq pc
	 | `Ne -> OpIfNe pc
	 | `Lt -> OpIfLt pc
	 | `Ge -> OpIfGe pc
	 | `Gt -> OpIfGt pc
	 | `Le -> OpIfLe pc
	 | `Null -> OpIfNull pc
	 | `NonNull -> OpIfNonNull pc)
  | JClass.OpIfCmp (x, pc) ->
      (match x with
	   `IEq -> OpICmpEq pc
	 | `INe -> OpICmpNe pc
	 | `ILt -> OpICmpLt pc
	 | `IGe -> OpICmpGe pc
	 | `IGt -> OpICmpGt pc
	 | `ILe -> OpICmpLe pc
	 | `AEq -> OpACmpEq pc
	 | `ANe -> OpACmpNe pc)
  | JClass.OpGoto pc ->
      if length = 3
      then OpGoto pc
      else if length = 5
      then OpGotoW pc
      else
        raise
          (Class_structure_error
             ("OpGoto "^string_of_int pc ^ " cannot be encoded in "
              ^ string_of_int length ^" bytes."))
  | JClass.OpJsr pc ->
      if length = 3
      then OpJsr pc
      else if length = 5
      then OpJsrW pc
      else
        raise
          (Class_structure_error
             ("OpJsr " ^ string_of_int pc ^ " cannot be encoded in "
              ^ string_of_int length ^" bytes."))
  | JClass.OpRet l -> OpRet l

  | JClass.OpTableSwitch (def, low, high, tbl) -> OpTableSwitch  (def, low, high, tbl)
  | JClass.OpLookupSwitch (def, tbl) -> OpLookupSwitch (def, tbl)

  | JClass.OpReturn k ->
      (match k with
	 | `Object -> OpAReturn
	 | `Void -> OpReturnVoid
	 | #jvm_basic_type as k -> OpReturn k)

  | JClass.OpGetStatic (c, s) ->
      OpGetStatic (field_to_int consts (c, s.fs_name, s.fs_type))
  | JClass.OpPutStatic (c, s) ->
      OpPutStatic (field_to_int consts (c, s.fs_name, s.fs_type))
  | JClass.OpGetField (c, s) ->
      OpGetField (field_to_int consts (c, s.fs_name, s.fs_type))
  | JClass.OpPutField (c, s) ->
      OpPutField (field_to_int consts (c, s.fs_name, s.fs_type))
  | JClass.OpInvoke (x, s) ->
      (match x with
	 | `Virtual t ->
	     OpInvokeVirtual
	       (method_to_int consts (t, s.ms_name, (s.ms_parameters,s.ms_return_type)))
	 | `Special t ->
	     OpInvokeNonVirtual
	       (method_to_int consts (TClass t, s.ms_name, (s.ms_parameters,s.ms_return_type)))
	 | `Static t ->
	     OpInvokeStatic
	       (method_to_int consts (TClass t, s.ms_name, (s.ms_parameters,s.ms_return_type)))
	 | `Interface t ->
	     OpInvokeInterface
	       (constant_to_int consts (ConstInterfaceMethod (t, s.ms_name, (s.ms_parameters,s.ms_return_type))), count s.ms_parameters))

  | JClass.OpNew n ->
      OpNew (class_to_int consts n)
  | JClass.OpNewArray t ->
      (match t with
	 | TBasic bt -> OpNewArray bt
	 | TObject ot ->
	     OpANewArray (object_type_to_int consts ot))
  | JClass.OpArrayLength -> OpArrayLength
  | JClass.OpThrow -> OpThrow
  | JClass.OpCheckCast ot -> OpCheckCast (object_type_to_int consts ot)
  | JClass.OpInstanceOf ot -> OpInstanceOf (object_type_to_int consts ot)
  | JClass.OpMonitorEnter -> OpMonitorEnter
  | JClass.OpMonitorExit -> OpMonitorExit
  | JClass.OpAMultiNewArray (i, dims) ->
      OpAMultiNewArray (object_type_to_int consts i, dims)
  | JClass.OpBreakpoint -> OpBreakpoint

  | JClass.OpInvalid -> OpInvalid

let check_space _consts offset length opcode =
  let ch = output_string () in
  let ch, count = pos_out ch in
  let offsetmod4 = offset mod 4 in
    for i = 1 to offsetmod4 do (* Pour les instructions alignées *)
      write_byte ch 0
    done;
    JCode.unparse_instruction ch count length opcode;
    let space_taken = count () - offsetmod4 in
    let opcodestring = close_out ch in
      if not (JBasics.get_permissive ()) && not  (String.length opcodestring - offsetmod4 = length)
      then failwith "check_space: count does not seems to provide the right result";
      length = space_taken
      

let code2opcodes consts code =
  let opcodes = Array.create (Array.length code) OpNop in
    Array.iteri
      (fun i instr ->
	 if instr <> JClass.OpInvalid
	 then (
           let length =
             let j = ref (i+1) in
               while !j < Array.length code && code.(!j) = JClass.OpInvalid do
                 opcodes.(!j) <- OpInvalid;
                 incr j
               done;
               !j-i
           in
	   let opcode = instruction2opcode consts length instr in
	     opcodes.(i) <- opcode;
	     if not (JBasics.get_permissive ()) && not (check_space consts i length opcode)
	     then 
               raise (Class_structure_error "Low level translation of instruction is too long for the allocated space in high level code");
	 ))
      code;
    opcodes
