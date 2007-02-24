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

(* Last modified by eandre@irisa.fr 2006/05/19 *)


type class_name = string list

type signature =
	| TByte
	| TChar
	| TDouble
	| TFloat
	| TInt
	| TLong
	| TShort
	| TBool
	| TObject of class_name
	| TArray of signature * int option (* Number of dimensions if <> 1 *)
	| TMethod of signature list * signature option

type constant =
	| ConstClass of class_name
	| ConstField of (class_name * string * signature)
	| ConstMethod of (class_name * string * signature)
	| ConstInterfaceMethod of (class_name * string * signature)
	| ConstString of string
	| ConstInt of int32
	| ConstFloat of float
	| ConstLong of int64
	| ConstDouble of float
	| ConstNameAndType of string * signature
	| ConstStringUTF8 of string
	| ConstUnusable

type access_flag =
	| AccPublic
	| AccPrivate
	| AccProtected
	| AccStatic
	| AccFinal
	| AccSynchronized (* Also used as "Special" *)
	| AccVolatile
	| AccTransient
	| AccNative
	| AccInterface
	| AccAbstract
	| AccStrict
	| AccRFU of int (* Four bits reserved for future use *)

type access_flags = access_flag list

type jexception = {
	e_start : int;
	e_end : int;
	e_handler : int;
	e_catch_type : class_name option
}

type array_type =
	| ATBool
	| ATChar
	| ATFloat
	| ATDouble
	| ATByte
	| ATShort
	| ATInt
	| ATLong

(* J'aurais utilisé des polymorphic variants pour généraliser l'emploi de kind. *)
type kind =
	| KInt
	| KLong
	| KFloat
	| KDouble

type opcode =
	| OpNop
	| OpAConstNull
	| OpIConst of int32
	| OpLConst of int64
	| OpFConst of float
	| OpDConst of float
	| OpBIPush of int
	| OpSIPush of int
	| OpLdc1 of constant
	| OpLdc1w of constant
	| OpLdc2w of constant

	| OpLoad of kind * int
	| OpALoad of int

	| OpArrayLoad of kind
	| OpAALoad
	| OpBALoad
	| OpCALoad
	| OpSALoad

	| OpStore of kind * int
	| OpAStore of int

	| OpArrayStore of kind
	| OpAAStore
	| OpBAStore
	| OpCAStore
	| OpSAStore

	| OpPop
	| OpPop2
	| OpDup
	| OpDupX1
	| OpDupX2
	| OpDup2
	| OpDup2X1
	| OpDup2X2
	| OpSwap

	| OpAdd of kind
	| OpSub of kind
	| OpMult of kind
	| OpDiv of kind
	| OpRem of kind
	| OpNeg of kind

	| OpIShl
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

	| OpIInc of int * int (* index, increment *)

	| OpI2L
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
	| OpI2B
	| OpI2C
	| OpI2S

	| OpLCmp
	| OpFCmpL
	| OpFCmpG
	| OpDCmpL
	| OpDCmpG
	| OpIfEq of int
	| OpIfNe of int
	| OpIfLt of int
	| OpIfGe of int
	| OpIfGt of int
	| OpIfLe of int
	| OpICmpEq of int
	| OpICmpNe of int
	| OpICmpLt of int
	| OpICmpGe of int
	| OpICmpGt of int
	| OpICmpLe of int
	| OpACmpEq of int
	| OpACmpNe of int
	| OpGoto of int
	| OpJsr of int
	| OpRet of int

	| OpTableSwitch of int * int32 * int32 * int array
	| OpLookupSwitch of int * (int32 * int) list

	| OpReturn of kind
	| OpAReturn
	| OpReturnVoid

	| OpGetStatic of class_name * string * signature
	| OpPutStatic of class_name * string * signature
	| OpGetField of class_name * string * signature
	| OpPutField of class_name * string * signature
	| OpInvokeVirtual of class_name * string * signature
	| OpInvokeNonVirtual of class_name * string * signature
	| OpInvokeStatic of class_name * string * signature
	| OpInvokeInterface of class_name * string * signature * int (* count *)

	| OpNew of class_name
	| OpNewArray of array_type
	| OpANewArray of class_name
	| OpArrayLength
	| OpThrow
	| OpCheckCast of class_name
	| OpInstanceOf of class_name
	| OpMonitorEnter
	| OpMonitorExit
	| OpAMultiNewArray of class_name * int (* ClassInfo, dims *)
	| OpIfNull of int
	| OpIfNonNull of int
	| OpGotoW of int
	| OpJsrW of int
	| OpBreakpoint
	| OpRetW of int

	| OpInvalid

type opcodes = opcode array

type verification_type = 
	| VTop 
	| VInteger 
	| VFloat
	| VDouble
	| VLong
	| VNull
	| VUninitializedThis
	| VObject of class_name
	| VUninitialized of int (* creation point *)

type attribute =
	| AttributeSourceFile of string
	| AttributeConstant of constant
	| AttributeCode of jcode
	| AttributeLineNumberTable of (int * int) list
	| AttributeUnknown of string * string
	| AttributeStackMap of (int*(verification_type list)*(verification_type list)) list

and jcode = {
	c_max_stack : int;
	c_max_locals : int;
	c_code : opcodes;
	c_exc_tbl : jexception list;
	c_attributes : attribute list;
}

type jfield = {
	f_name : string;
	f_signature : signature;
	f_flags : access_flags;
	f_attributes : attribute list;
}

type jmethod = {
	m_name : string;
	m_signature : signature;
	m_flags : access_flags;	
	m_code : jcode option;
	m_attributes : attribute list;
}

type jclass = {
	j_name : class_name;
	j_super : class_name option;
	j_interfaces : class_name list;
	j_consts : constant array;
	j_flags : access_flags;
	j_fields : jfield list;
	j_methods : jmethod list;
	j_attributes : attribute list;
}
