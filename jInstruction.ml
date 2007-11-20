(*
 *  This file is part of JavaLib
 *  Copyright (c)2007 Université de Rennes 1
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
	| OpCodeNop -> OpNop
	| OpCodeAConstNull -> OpConst `ANull
	| OpCodeIConst v -> OpConst (`I v)
	| OpCodeLConst v -> OpConst (`L v)
	| OpCodeFConst v -> OpConst (`F v)
	| OpCodeDConst v -> OpConst (`D v)
	| OpCodeBIPush v -> OpConst (`B v)
	| OpCodeSIPush v -> OpConst (`S v)
	| OpCodeLdc1 n
	| OpCodeLdc1w n ->
	    OpLdc
	      (match get_constant_value consts n with
		 | ConstInt _ | ConstFloat _ | ConstString _ | ConstClass _ as c -> c
		 | ConstLong _ | ConstDouble _ -> raise (Illegal_value ("long/double", "constant for Ldc1")))
	| OpCodeLdc2w n ->
	    OpLdc
	      (match get_constant_value consts n with
		 | ConstInt _ | ConstFloat _ | ConstString _ | ConstClass _ ->
		     raise (Illegal_value ("int/float/string/class", "constant for Ldc2"))
		 | ConstLong _ | ConstDouble _ as c -> c)

	| OpCodeLoad (k, l) ->
	    OpLoad ((k : jvm_basic_type :> [> jvm_basic_type]), l)
	| OpCodeALoad l -> OpLoad (`Object, l)

	| OpCodeArrayLoad k ->
	    OpArrayLoad (k : [`Int | other_num] :> [> `Int | other_num])
	| OpCodeAALoad -> OpArrayLoad `Object
	| OpCodeBALoad -> OpArrayLoad `ByteBool
	| OpCodeCALoad -> OpArrayLoad `Char
	| OpCodeSALoad -> OpArrayLoad `Short


	| OpCodeStore (k, l) ->
	    OpStore ((k : jvm_basic_type :> [> jvm_basic_type]), l)
	| OpCodeAStore l -> OpStore (`Object, l)

	| OpCodeArrayStore k ->
	    OpArrayStore (k : [`Int | other_num] :> [> `Int | other_num])

	| OpCodeAAStore -> OpArrayStore `Object
	| OpCodeBAStore -> OpArrayStore `ByteBool
	| OpCodeCAStore -> OpArrayStore `Char
	| OpCodeSAStore -> OpArrayStore `Short

	| OpCodePop -> OpPop
	| OpCodePop2 -> OpPop2
	| OpCodeDup -> OpDup
	| OpCodeDupX1 -> OpDupX1
	| OpCodeDupX2 -> OpDupX2
	| OpCodeDup2 -> OpDup2
	| OpCodeDup2X1 -> OpDup2X1
	| OpCodeDup2X2 -> OpDup2X2
	| OpCodeSwap -> OpSwap

	| OpCodeAdd k -> OpAdd k
	| OpCodeSub k -> OpSub k
	| OpCodeMult k -> OpMult k
	| OpCodeDiv k -> OpDiv k
	| OpCodeRem k -> OpRem k
	| OpCodeNeg k -> OpNeg k

	| OpCodeIShl -> OpIShl
	| OpCodeLShl -> OpLShl
	| OpCodeIShr -> OpIShr
	| OpCodeLShr -> OpLShr
	| OpCodeIUShr -> OpIUShr
	| OpCodeLUShr -> OpLUShr
	| OpCodeIAnd -> OpIAnd
	| OpCodeLAnd -> OpLAnd
	| OpCodeIOr -> OpIOr
	| OpCodeLOr -> OpLOr
	| OpCodeIXor -> OpIXor
	| OpCodeLXor -> OpLXor

	| OpCodeIInc (index, incr) -> OpIInc (index, incr)

	| OpCodeI2L -> OpI2L
	| OpCodeI2F -> OpI2F
	| OpCodeI2D -> OpI2D
	| OpCodeL2I -> OpL2I
	| OpCodeL2F -> OpL2F
	| OpCodeL2D -> OpL2D
	| OpCodeF2I -> OpF2I
	| OpCodeF2L -> OpF2L
	| OpCodeF2D -> OpF2D
	| OpCodeD2I -> OpD2I
	| OpCodeD2L -> OpD2L
	| OpCodeD2F -> OpD2F
	| OpCodeI2B -> OpI2B
	| OpCodeI2C -> OpI2C
	| OpCodeI2S -> OpI2S

	| OpCodeLCmp -> OpCmp `L
	| OpCodeFCmpL -> OpCmp `FL
	| OpCodeFCmpG -> OpCmp `FG
	| OpCodeDCmpL -> OpCmp `DL
	| OpCodeDCmpG -> OpCmp `DG
	| OpCodeIfEq pc -> OpIf (`Eq, pc)
	| OpCodeIfNe pc -> OpIf (`Ne, pc)
	| OpCodeIfLt pc -> OpIf (`Lt, pc)
	| OpCodeIfGe pc -> OpIf (`Ge, pc)
	| OpCodeIfGt pc -> OpIf (`Gt, pc)
	| OpCodeIfLe pc -> OpIf (`Le, pc)
	| OpCodeICmpEq pc -> OpIfCmp (`IEq, pc)
	| OpCodeICmpNe pc -> OpIfCmp (`INe, pc)
	| OpCodeICmpLt pc -> OpIfCmp (`ILt, pc)
	| OpCodeICmpGe pc -> OpIfCmp (`IGe, pc)
	| OpCodeICmpGt pc -> OpIfCmp (`IGt, pc)
	| OpCodeICmpLe pc -> OpIfCmp (`ILe, pc)
	| OpCodeACmpEq pc -> OpIfCmp (`AEq, pc)
	| OpCodeACmpNe pc -> OpIfCmp (`ANe, pc)
	| OpCodeGoto pc -> OpGoto pc
	| OpCodeJsr pc -> OpJsr pc
	| OpCodeRet l -> OpRet l

	| OpCodeTableSwitch (def, low, high, tbl) -> OpTableSwitch  (def, low, high, tbl)
	| OpCodeLookupSwitch (def, tbl) -> OpLookupSwitch (def, tbl)

	| OpCodeReturn k -> OpReturn (k : jvm_basic_type :> [> jvm_basic_type])
	| OpCodeAReturn -> OpReturn `Object
	| OpCodeReturnVoid -> OpReturn `Void

	| OpCodeGetStatic i ->
	    let c, n, s = get_field consts i in
	      OpGetStatic (c, {fs_name = n; fs_type = s})
	| OpCodePutStatic i ->
	    let c, n, s = get_field consts i in
	      OpPutStatic (c, {fs_name = n; fs_type = s})
	| OpCodeGetField i ->
	    let c, n, s = get_field consts i in
	      OpGetField (c, {fs_name = n; fs_type = s})
	| OpCodePutField i ->
	    let c, n, s = get_field consts i in
	      OpPutField (c, {fs_name = n; fs_type = s})
	| OpCodeInvokeVirtual i ->
	    let t, n, s = get_method consts i in
	      OpInvoke (`Virtual t, {ms_name = n; ms_parameters = fst s}, snd s)
	| OpCodeInvokeNonVirtual i ->
	    (match get_method consts i with
	       | TClass t, n, s -> OpInvoke (`Special t, {ms_name = n; ms_parameters = fst s}, snd s)
	       | _ -> raise (Illegal_value ("array class", "invokespecial")))
	| OpCodeInvokeStatic i ->
	    (match get_method consts i with
	       | TClass t, n, s -> OpInvoke (`Static t, {ms_name = n; ms_parameters = fst s}, snd s)
	       | _ -> raise (Illegal_value ("array class", "invokestatic")))
	| OpCodeInvokeInterface (i, c) ->
	    let t, n, (vts, _ as s) = get_interface_method consts i in
	      if count vts <> c
	      then raise (Class_structure_error "wrong count in invokeinterface");
	      OpInvoke (`Interface t, {ms_name = n; ms_parameters = fst s}, snd s)

	| OpCodeNew i -> OpNew (get_class consts i)
	| OpCodeNewArray bt -> OpNewArray (TBasic bt)
	| OpCodeANewArray i -> OpNewArray (TObject (get_object_type consts i))
	| OpCodeArrayLength -> OpArrayLength
	| OpCodeThrow -> OpThrow
	| OpCodeCheckCast i -> OpCheckCast (get_object_type consts i)
	| OpCodeInstanceOf i -> OpInstanceOf (get_object_type consts i)
	| OpCodeMonitorEnter -> OpMonitorEnter
	| OpCodeMonitorExit -> OpMonitorExit
	| OpCodeAMultiNewArray (ot, dims) ->
	    OpAMultiNewArray (get_object_type consts ot, dims)
	| OpCodeIfNull pc -> OpIf (`Null, pc)
	| OpCodeIfNonNull pc -> OpIf (`NonNull, pc)
	| OpCodeGotoW pc -> OpGoto pc
	| OpCodeJsrW pc -> OpJsr pc
	| OpCodeBreakpoint -> OpBreakpoint
	| OpCodeRetW l -> OpRet l

	| OpCodeInvalid -> OpInvalid

let opcodes2code consts opcodes =
  Array.map (opcode2instruction consts) opcodes

let instruction2opcode consts = function
	| OpNop -> OpCodeNop
	| OpConst x ->
	    (match x with
	       | `ANull -> OpCodeAConstNull
	       | `I v -> OpCodeIConst v
	       | `L v -> OpCodeLConst v
	       | `F v -> OpCodeFConst v
	       | `D v -> OpCodeDConst v
	       | `B v -> OpCodeBIPush v
	       | `S v -> OpCodeSIPush v)
	| OpLdc v ->
	    let index = (constant_to_int consts (ConstValue v)) in
	      (match v with
		 | ConstInt _ | ConstFloat _ | ConstString _ | ConstClass _ ->
		     if index <= 0xFF
		     then OpCodeLdc1 index
		     else OpCodeLdc1w index
		 | ConstLong _ | ConstDouble _ -> OpCodeLdc2w index)

	| OpLoad (k, l) ->
	    (match k with
	       | `Object -> OpCodeALoad l
	       | #jvm_basic_type as k -> OpCodeLoad (k, l))

	| OpArrayLoad k ->
	    (match k with
	       | `Object -> OpCodeAALoad
	       | `ByteBool -> OpCodeBALoad
	       | `Char -> OpCodeCALoad
	       | `Short -> OpCodeSALoad
	       | `Int | #other_num as k -> OpCodeArrayLoad k)

	| OpStore (k, l) ->
	    (match k with
	       | `Object -> OpCodeAStore l
	       | #jvm_basic_type as k -> OpCodeStore (k, l))

	| OpArrayStore k ->
	    (match k with
	       | `Object -> OpCodeAAStore
	       | `ByteBool -> OpCodeBAStore
	       | `Char -> OpCodeCAStore
	       | `Short -> OpCodeSAStore
	       | `Int | #other_num as k -> OpCodeArrayStore k)

	| OpPop -> OpCodePop
	| OpPop2 -> OpCodePop2
	| OpDup -> OpCodeDup
	| OpDupX1 -> OpCodeDupX1
	| OpDupX2 -> OpCodeDupX2
	| OpDup2 -> OpCodeDup2
	| OpDup2X1 -> OpCodeDup2X1
	| OpDup2X2 -> OpCodeDup2X2
	| OpSwap -> OpCodeSwap

	| OpAdd k -> OpCodeAdd k
	| OpSub k -> OpCodeSub k
	| OpMult k -> OpCodeMult k
	| OpDiv k -> OpCodeDiv k
	| OpRem k -> OpCodeRem k
	| OpNeg k -> OpCodeNeg k

	| OpIShl -> OpCodeIShl
	| OpLShl -> OpCodeLShl
	| OpIShr -> OpCodeIShr
	| OpLShr -> OpCodeLShr
	| OpIUShr -> OpCodeIUShr
	| OpLUShr -> OpCodeLUShr
	| OpIAnd -> OpCodeIAnd
	| OpLAnd -> OpCodeLAnd
	| OpIOr -> OpCodeIOr
	| OpLOr -> OpCodeLOr
	| OpIXor -> OpCodeIXor
	| OpLXor -> OpCodeLXor

	| OpIInc (index, incr) -> OpCodeIInc (index, incr)

	| OpI2L -> OpCodeI2L
	| OpI2F -> OpCodeI2F
	| OpI2D -> OpCodeI2D
	| OpL2I -> OpCodeL2I
	| OpL2F -> OpCodeL2F
	| OpL2D -> OpCodeL2D
	| OpF2I -> OpCodeF2I
	| OpF2L -> OpCodeF2L
	| OpF2D -> OpCodeF2D
	| OpD2I -> OpCodeD2I
	| OpD2L -> OpCodeD2L
	| OpD2F -> OpCodeD2F
	| OpI2B -> OpCodeI2B
	| OpI2C -> OpCodeI2C
	| OpI2S -> OpCodeI2S

	| OpCmp x ->
	    (match x with
	       | `L -> OpCodeLCmp
	       | `FL -> OpCodeFCmpL
	       | `FG -> OpCodeFCmpG
	       | `DL -> OpCodeDCmpL
	       | `DG -> OpCodeDCmpG)
	| OpIf (x, pc) ->
	    (match x with
		 `Eq -> OpCodeIfEq pc
	       | `Ne -> OpCodeIfNe pc
	       | `Lt -> OpCodeIfLt pc
	       | `Ge -> OpCodeIfGe pc
	       | `Gt -> OpCodeIfGt pc
	       | `Le -> OpCodeIfLe pc
	       | `Null -> OpCodeIfNull pc
	       | `NonNull -> OpCodeIfNonNull pc)
	| OpIfCmp (x, pc) ->
	    (match x with
		 `IEq -> OpCodeICmpEq pc
	       | `INe -> OpCodeICmpNe pc
	       | `ILt -> OpCodeICmpLt pc
	       | `IGe -> OpCodeICmpGe pc
	       | `IGt -> OpCodeICmpGt pc
	       | `ILe -> OpCodeICmpLe pc
	       | `AEq -> OpCodeACmpEq pc
	       | `ANe -> OpCodeACmpNe pc)
	| OpGoto pc ->
 	    if - 0x8000 <= pc && pc <= 0x7FFF
	    then OpCodeGoto pc
	    else OpCodeGotoW pc
	| OpJsr pc ->
 	    if - 0x8000 <= pc && pc <= 0x7FFF
	    then OpCodeJsr pc
	    else OpCodeJsrW pc
	| OpRet l ->
 	    if l <= 0xFFFF
	    then OpCodeRet l
	    else OpCodeRetW l

	| OpTableSwitch (def, low, high, tbl) -> OpCodeTableSwitch  (def, low, high, tbl)
	| OpLookupSwitch (def, tbl) -> OpCodeLookupSwitch (def, tbl)

	| OpReturn k ->
	    (match k with
	       | `Object -> OpCodeAReturn
	       | `Void -> OpCodeReturnVoid
	       | #jvm_basic_type as k -> OpCodeReturn k)

	| OpGetStatic (c, s) ->
	    OpCodeGetStatic (constant_to_int consts (ConstField (c, s.fs_name, s.fs_type)))
	| OpPutStatic (c, s) ->
	    OpCodePutStatic (constant_to_int consts (ConstField (c, s.fs_name, s.fs_type)))
	| OpGetField (c, s) ->
	    OpCodeGetField (constant_to_int consts (ConstField (c, s.fs_name, s.fs_type)))
	| OpPutField (c, s) ->
	    OpCodePutField (constant_to_int consts (ConstField (c, s.fs_name, s.fs_type)))
	| OpInvoke (x, s, r) ->
	    (match x with
	       | `Virtual t ->
		   OpCodeInvokeVirtual
		     (constant_to_int consts (ConstMethod (t, s.ms_name, (s.ms_parameters,r))))
	       | `Special t ->
		   OpCodeInvokeNonVirtual
		     (constant_to_int consts (ConstMethod (TClass t, s.ms_name, (s.ms_parameters,r))))
	       | `Static t ->
		   OpCodeInvokeStatic
		     (constant_to_int consts (ConstMethod (TClass t, s.ms_name, (s.ms_parameters,r))))
	       | `Interface t ->
		   OpCodeInvokeInterface
		     (constant_to_int consts (ConstInterfaceMethod (t, s.ms_name, (s.ms_parameters,r))), count s.ms_parameters))

	| OpNew n ->
	    OpCodeNew
	      (constant_to_int consts (ConstValue (ConstClass (TClass n))))
	| OpNewArray t ->
	    (match t with
	       | TBasic bt -> OpCodeNewArray bt
	       | TObject ot ->
		   OpCodeANewArray (constant_to_int consts (ConstValue (ConstClass ot))))
	| OpArrayLength -> OpCodeArrayLength
	| OpThrow -> OpCodeThrow
	| OpCheckCast ot -> OpCodeCheckCast (constant_to_int consts (ConstValue (ConstClass ot)))
	| OpInstanceOf ot -> OpCodeInstanceOf (constant_to_int consts (ConstValue (ConstClass ot)))
	| OpMonitorEnter -> OpCodeMonitorEnter
	| OpMonitorExit -> OpCodeMonitorExit
	| OpAMultiNewArray (i, dims) ->
	    OpCodeAMultiNewArray (constant_to_int consts (ConstValue (ConstClass i)), dims)
	| OpBreakpoint -> OpCodeBreakpoint

	| OpInvalid -> OpCodeInvalid

let opcode_length consts offset opcode =
  let ch = output_string () in
  let ch, count = pos_out ch in
    for i = 1 to offset mod 4 do (* Pour les instructions alignÃ©es *)
      write_byte ch 0
    done;
    JCode.unparse_instruction ch count opcode;
    let length = count () - (offset mod 4) in
      assert (String.length (close_out ch) - (offset mod 4) = length);
      length

let code2opcodes consts code =
  let opcodes = Array.create (Array.length code) OpCodeNop in
    Array.iteri
      (fun i instr ->
	 if instr <> OpInvalid
	 then (
	   let opcode = instruction2opcode consts instr in
	   let length = opcode_length consts i opcode in
	     if opcodes.(i) = OpCodeInvalid || i + length > Array.length opcodes
	     then raise (Class_structure_error "Low level translation of instruction is too long for the allocated space in high level code");
	     opcodes.(i) <- opcode;
	     for j = i + 1 to i + length - 1 do
	       opcodes.(j) <- OpCodeInvalid
	     done
	 ))
      code;
    opcodes
