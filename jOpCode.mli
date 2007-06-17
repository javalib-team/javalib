(*
 *  This file is part of JavaLib
 *  Copyright (c)2004 Nicolas Cannasse
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

(** Low level representation of instructions. *)

open JClass

type opcode =
	| OpCodeNop
	| OpCodeAConstNull
	| OpCodeIConst of int32
	| OpCodeLConst of int64
	| OpCodeFConst of float
	| OpCodeDConst of float
	| OpCodeBIPush of int
	| OpCodeSIPush of int
	| OpCodeLdc1 of constant_value
	| OpCodeLdc2w of constant_value

	| OpCodeLoad of kind * int
	| OpCodeALoad of int

	| OpCodeArrayLoad of kind
	| OpCodeAALoad
	| OpCodeBALoad
	| OpCodeCALoad
	| OpCodeSALoad

	| OpCodeStore of kind * int
	| OpCodeAStore of int

	| OpCodeArrayStore of kind
	| OpCodeAAStore
	| OpCodeBAStore
	| OpCodeCAStore
	| OpCodeSAStore

	| OpCodePop
	| OpCodePop2
	| OpCodeDup
	| OpCodeDupX1
	| OpCodeDupX2
	| OpCodeDup2
	| OpCodeDup2X1
	| OpCodeDup2X2
	| OpCodeSwap

	| OpCodeAdd of kind
	| OpCodeSub of kind
	| OpCodeMult of kind
	| OpCodeDiv of kind
	| OpCodeRem of kind
	| OpCodeNeg of kind

	| OpCodeIShl
	| OpCodeLShl
	| OpCodeIShr
	| OpCodeLShr
	| OpCodeIUShr
	| OpCodeLUShr
	| OpCodeIAnd
	| OpCodeLAnd
	| OpCodeIOr
	| OpCodeLOr
	| OpCodeIXor
	| OpCodeLXor

	| OpCodeIInc of int * int (** index, increment *)

	| OpCodeI2L
	| OpCodeI2F
	| OpCodeI2D
	| OpCodeL2I
	| OpCodeL2F
	| OpCodeL2D
	| OpCodeF2I
	| OpCodeF2L
	| OpCodeF2D
	| OpCodeD2I
	| OpCodeD2L
	| OpCodeD2F
	| OpCodeI2B
	| OpCodeI2C
	| OpCodeI2S

	| OpCodeLCmp
	| OpCodeFCmpL
	| OpCodeFCmpG
	| OpCodeDCmpL
	| OpCodeDCmpG
	| OpCodeIfEq of int
	| OpCodeIfNe of int
	| OpCodeIfLt of int
	| OpCodeIfGe of int
	| OpCodeIfGt of int
	| OpCodeIfLe of int
	| OpCodeICmpEq of int
	| OpCodeICmpNe of int
	| OpCodeICmpLt of int
	| OpCodeICmpGe of int
	| OpCodeICmpGt of int
	| OpCodeICmpLe of int
	| OpCodeACmpEq of int
	| OpCodeACmpNe of int
	| OpCodeGoto of int
	| OpCodeJsr of int
	| OpCodeRet of int

	| OpCodeTableSwitch of int * int32 * int32 * int array
	| OpCodeLookupSwitch of int * (int32 * int) list

	| OpCodeReturn of kind
	| OpCodeAReturn
	| OpCodeReturnVoid

	| OpCodeGetStatic of class_name * string * field_signature
	| OpCodePutStatic of class_name * string * field_signature
	| OpCodeGetField of class_name * string * field_signature
	| OpCodePutField of class_name * string * field_signature
	| OpCodeInvokeVirtual of object_type * string * method_signature
	| OpCodeInvokeNonVirtual of class_name * string * method_signature
	| OpCodeInvokeStatic of class_name * string * method_signature
	| OpCodeInvokeInterface of class_name * string * method_signature * int (** count *)

	| OpCodeNew of class_name
	| OpCodeNewArray of basic_type
	| OpCodeANewArray of object_type
	| OpCodeArrayLength
	| OpCodeThrow
	| OpCodeCheckCast of object_type
	| OpCodeInstanceOf of object_type
	| OpCodeMonitorEnter
	| OpCodeMonitorExit
	| OpCodeAMultiNewArray of object_type * int (** ClassInfo, dims *)
	| OpCodeIfNull of int
	| OpCodeIfNonNull of int
	| OpCodeGotoW of int
	| OpCodeJsrW of int
	| OpCodeBreakpoint
	| OpCodeRetW of int

	| OpCodeInvalid
