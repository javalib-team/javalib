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

open JOpCode
open JClass
open JConsts
open IO
open IO.BigEndian

let opcode2instruction consts = function
	| OpCodeNop -> OpNop
	| OpCodeAConstNull -> OpAConstNull
	| OpCodeIConst v -> OpIConst v
	| OpCodeLConst v -> OpLConst v
	| OpCodeFConst v -> OpFConst v
	| OpCodeDConst v -> OpDConst v
	| OpCodeBIPush v -> OpBIPush v
	| OpCodeSIPush v -> OpSIPush v
	| OpCodeLdc1 v -> OpLdc1 v
	| OpCodeLdc2w v -> OpLdc2w v

	| OpCodeLoad (k, l) -> OpLoad (k, l)
	| OpCodeALoad l -> OpALoad l

	| OpCodeArrayLoad k -> OpArrayLoad k
	| OpCodeAALoad -> OpAALoad
	| OpCodeBALoad -> OpBALoad
	| OpCodeCALoad -> OpCALoad
	| OpCodeSALoad -> OpSALoad

	| OpCodeStore (k, l) -> OpStore (k, l)
	| OpCodeAStore l -> OpAStore l

	| OpCodeArrayStore k -> OpArrayStore k
	| OpCodeAAStore -> OpAAStore
	| OpCodeBAStore -> OpBAStore
	| OpCodeCAStore -> OpCAStore
	| OpCodeSAStore -> OpSAStore

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

	| OpCodeLCmp -> OpLCmp
	| OpCodeFCmpL -> OpFCmpL
	| OpCodeFCmpG -> OpFCmpG
	| OpCodeDCmpL -> OpDCmpL
	| OpCodeDCmpG -> OpDCmpG
	| OpCodeIfEq pc -> OpIfEq pc
	| OpCodeIfNe pc -> OpIfNe pc
	| OpCodeIfLt pc -> OpIfLt pc
	| OpCodeIfGe pc -> OpIfGe pc
	| OpCodeIfGt pc -> OpIfGt pc
	| OpCodeIfLe pc -> OpIfLe pc
	| OpCodeICmpEq pc -> OpICmpEq pc
	| OpCodeICmpNe pc -> OpICmpNe pc
	| OpCodeICmpLt pc -> OpICmpLt pc
	| OpCodeICmpGe pc -> OpICmpGe pc
	| OpCodeICmpGt pc -> OpICmpGt pc
	| OpCodeICmpLe pc -> OpICmpLe pc
	| OpCodeACmpEq pc -> OpACmpEq pc
	| OpCodeACmpNe pc -> OpACmpNe pc
	| OpCodeGoto pc -> OpGoto pc
	| OpCodeJsr pc -> OpJsr pc
	| OpCodeRet l -> OpRet l

	| OpCodeTableSwitch (def, low, high, tbl) -> OpTableSwitch  (def, low, high, tbl)
	| OpCodeLookupSwitch (def, tbl) -> OpLookupSwitch (def, tbl)

	| OpCodeReturn k -> OpReturn k
	| OpCodeAReturn -> OpAReturn
	| OpCodeReturnVoid -> OpReturnVoid

	| OpCodeGetStatic (c, n, s) -> OpGetStatic (c, n, s)
	| OpCodePutStatic (c, n, s) -> OpPutStatic (c, n, s)
	| OpCodeGetField (c, n, s) -> OpGetField (c, n, s)
	| OpCodePutField (c, n, s) -> OpPutField (c, n, s)
	| OpCodeInvokeVirtual (t, n, s) -> OpInvokeVirtual (t, n, s)
	| OpCodeInvokeNonVirtual (t, n, s) -> OpInvokeNonVirtual (t, n, s)
	| OpCodeInvokeStatic (t, n, s) -> OpInvokeStatic (t, n, s)
	| OpCodeInvokeInterface (t, n, s, c) -> OpInvokeInterface (t, n, s, c)

	| OpCodeNew n -> OpNew n
	| OpCodeNewArray bt -> OpNewArray bt
	| OpCodeANewArray ot -> OpANewArray ot
	| OpCodeArrayLength -> OpArrayLength
	| OpCodeThrow -> OpThrow
	| OpCodeCheckCast ot -> OpCheckCast ot
	| OpCodeInstanceOf ot -> OpInstanceOf ot
	| OpCodeMonitorEnter -> OpMonitorEnter
	| OpCodeMonitorExit -> OpMonitorExit
	| OpCodeAMultiNewArray (ot, dims) -> OpAMultiNewArray (ot, dims)
	| OpCodeIfNull pc -> OpIfNull pc
	| OpCodeIfNonNull pc -> OpIfNonNull pc
	| OpCodeGotoW pc -> OpGotoW pc
	| OpCodeJsrW pc -> OpJsrW pc
	| OpCodeBreakpoint -> OpBreakpoint
	| OpCodeRetW l -> OpRetW l

	| OpCodeInvalid -> OpInvalid

let opcodes2code consts opcodes =
  Array.map (opcode2instruction consts) opcodes

let instruction2opcode consts = function
	| OpNop -> OpCodeNop
	| OpAConstNull -> OpCodeAConstNull
	| OpIConst v -> OpCodeIConst v
	| OpLConst v -> OpCodeLConst v
	| OpFConst v -> OpCodeFConst v
	| OpDConst v -> OpCodeDConst v
	| OpBIPush v -> OpCodeBIPush v
	| OpSIPush v -> OpCodeSIPush v
	| OpLdc1 v -> OpCodeLdc1 v
	| OpLdc2w v -> OpCodeLdc2w v

	| OpLoad (k, l) -> OpCodeLoad (k, l)
	| OpALoad l -> OpCodeALoad l

	| OpArrayLoad k -> OpCodeArrayLoad k
	| OpAALoad -> OpCodeAALoad
	| OpBALoad -> OpCodeBALoad
	| OpCALoad -> OpCodeCALoad
	| OpSALoad -> OpCodeSALoad

	| OpStore (k, l) -> OpCodeStore (k, l)
	| OpAStore l -> OpCodeAStore l

	| OpArrayStore k -> OpCodeArrayStore k
	| OpAAStore -> OpCodeAAStore
	| OpBAStore -> OpCodeBAStore
	| OpCAStore -> OpCodeCAStore
	| OpSAStore -> OpCodeSAStore

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

	| OpLCmp -> OpCodeLCmp
	| OpFCmpL -> OpCodeFCmpL
	| OpFCmpG -> OpCodeFCmpG
	| OpDCmpL -> OpCodeDCmpL
	| OpDCmpG -> OpCodeDCmpG
	| OpIfEq pc -> OpCodeIfEq pc
	| OpIfNe pc -> OpCodeIfNe pc
	| OpIfLt pc -> OpCodeIfLt pc
	| OpIfGe pc -> OpCodeIfGe pc
	| OpIfGt pc -> OpCodeIfGt pc
	| OpIfLe pc -> OpCodeIfLe pc
	| OpICmpEq pc -> OpCodeICmpEq pc
	| OpICmpNe pc -> OpCodeICmpNe pc
	| OpICmpLt pc -> OpCodeICmpLt pc
	| OpICmpGe pc -> OpCodeICmpGe pc
	| OpICmpGt pc -> OpCodeICmpGt pc
	| OpICmpLe pc -> OpCodeICmpLe pc
	| OpACmpEq pc -> OpCodeACmpEq pc
	| OpACmpNe pc -> OpCodeACmpNe pc
	| OpGoto pc -> OpCodeGoto pc
	| OpJsr pc -> OpCodeJsr pc
	| OpRet l -> OpCodeRet l

	| OpTableSwitch (def, low, high, tbl) -> OpCodeTableSwitch  (def, low, high, tbl)
	| OpLookupSwitch (def, tbl) -> OpCodeLookupSwitch (def, tbl)

	| OpReturn k -> OpCodeReturn k
	| OpAReturn -> OpCodeAReturn
	| OpReturnVoid -> OpCodeReturnVoid

	| OpGetStatic (c, n, s) -> OpCodeGetStatic (c, n, s)
	| OpPutStatic (c, n, s) -> OpCodePutStatic (c, n, s)
	| OpGetField (c, n, s) -> OpCodeGetField (c, n, s)
	| OpPutField (c, n, s) -> OpCodePutField (c, n, s)
	| OpInvokeVirtual (t, n, s) -> OpCodeInvokeVirtual (t, n, s)
	| OpInvokeNonVirtual (t, n, s) -> OpCodeInvokeNonVirtual (t, n, s)
	| OpInvokeStatic (t, n, s) -> OpCodeInvokeStatic (t, n, s)
	| OpInvokeInterface (t, n, s, c) -> OpCodeInvokeInterface (t, n, s, c)

	| OpNew n -> OpCodeNew n
	| OpNewArray bt -> OpCodeNewArray bt
	| OpANewArray ot -> OpCodeANewArray ot
	| OpArrayLength -> OpCodeArrayLength
	| OpThrow -> OpCodeThrow
	| OpCheckCast ot -> OpCodeCheckCast ot
	| OpInstanceOf ot -> OpCodeInstanceOf ot
	| OpMonitorEnter -> OpCodeMonitorEnter
	| OpMonitorExit -> OpCodeMonitorExit
	| OpAMultiNewArray (ot, dims) -> OpCodeAMultiNewArray (ot, dims)
	| OpIfNull pc -> OpCodeIfNull pc
	| OpIfNonNull pc -> OpCodeIfNonNull pc
	| OpGotoW pc -> OpCodeGotoW pc
	| OpJsrW pc -> OpCodeJsrW pc
	| OpBreakpoint -> OpCodeBreakpoint
	| OpRetW l -> OpCodeRetW l

	| OpInvalid -> OpCodeInvalid

let opcode_length consts offset opcode =
  let ch = output_string () in
  let ch, count = pos_out ch in
    for i = 1 to offset mod 4 do (* Pour les instructions alignées *)
      write_byte ch 0
    done;
    JCode.unparse_instruction ch consts count opcode;
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
	     then failwith ("Instruction too long");
	     opcodes.(i) <- opcode;
	     for j = i + 1 to i + length - 1 do
	       opcodes.(j) <- OpCodeInvalid
	     done
	 ))
      code;
    opcodes
