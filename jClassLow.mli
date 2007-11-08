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

(** Low level representation of a Java class file. *)

(** {2 Basic Elements.} *)

(** Fully qualified ordinary class or interface name (not an array).
    For example: [\["java" ; "lang" ; "Object"\]]. *)
type class_name = string list

(** Numerical types that are not smaller than int. *)
type other_num = [
| `Long
| `Float
| `Double
]

(** JVM basic type (int = short = char = byte = bool). *)
type jvm_basic_type = [
| `Int2Bool
| other_num
]

(** JVM type (int = short = char = byte = bool, all objects have the same type). *)
type jvm_type = [
| jvm_basic_type
| `Object
]

(** JVM array element type (byte = bool, all objects have the same type). *)
type jvm_array_type = [
| `Int
| `Short
| `Char
| `ByteBool
| other_num
| `Object
]

(** JVM return type (byte = bool, all objects have the same type). *)
type jvm_return_type = [
|  jvm_basic_type
| `Object
| `Void
]

(** Java basic type. *)
type java_basic_type = [
| `Int
| `Short
| `Char
| `Byte
| `Bool
| other_num
]

(** Java object type *)
type object_type =
  | TClass of class_name
  | TArray of value_type

(** Java type *)
and value_type =
  | TBasic of java_basic_type
  | TObject of object_type

(** Field signature. *)
type field_descriptor = value_type

(** Method signature. *)
type method_descriptor = value_type list * value_type option

(** Signatures parsed from CONSTANT_NameAndType_info structures. *)
type signature =
  | SValue of field_descriptor
  | SMethod of method_descriptor

(** Constant value. *)
type constant_value =
  | ConstString of string
  | ConstInt of int32
  | ConstFloat of float
  | ConstLong of int64
  | ConstDouble of float
  | ConstClass of object_type (** This is not documented in the JVM spec. *)

(** Constant. *)
type constant =
  | ConstValue of constant_value
  | ConstField of (class_name * string * field_descriptor)
  | ConstMethod of (object_type * string * method_descriptor)
  | ConstInterfaceMethod of (class_name * string * method_descriptor)
  | ConstNameAndType of string * signature
  | ConstStringUTF8 of string
  | ConstUnusable

(** Instruction. *)
type opcode =
	| OpCodeNop
	| OpCodeAConstNull
	| OpCodeIConst of int32
	| OpCodeLConst of int64
	| OpCodeFConst of float
	| OpCodeDConst of float
	| OpCodeBIPush of int
	| OpCodeSIPush of int
	| OpCodeLdc1 of int
	| OpCodeLdc1w of int
	| OpCodeLdc2w of int

	| OpCodeLoad of jvm_basic_type * int
	| OpCodeALoad of int

	| OpCodeArrayLoad of [`Int | other_num]
	| OpCodeAALoad
	| OpCodeBALoad
	| OpCodeCALoad
	| OpCodeSALoad

	| OpCodeStore of jvm_basic_type * int
	| OpCodeAStore of int

	| OpCodeArrayStore of [`Int | other_num]
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

	| OpCodeAdd of jvm_basic_type
	| OpCodeSub of jvm_basic_type
	| OpCodeMult of jvm_basic_type
	| OpCodeDiv of jvm_basic_type
	| OpCodeRem of jvm_basic_type
	| OpCodeNeg of jvm_basic_type

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

	| OpCodeReturn of jvm_basic_type
	| OpCodeAReturn
	| OpCodeReturnVoid

	| OpCodeGetStatic of int
	| OpCodePutStatic of int
	| OpCodeGetField of int
	| OpCodePutField of int
	| OpCodeInvokeVirtual of int
	| OpCodeInvokeNonVirtual of int
	| OpCodeInvokeStatic of int
	| OpCodeInvokeInterface of int * int (** count *)

	| OpCodeNew of int
	| OpCodeNewArray of java_basic_type
	| OpCodeANewArray of int
	| OpCodeArrayLength
	| OpCodeThrow
	| OpCodeCheckCast of int
	| OpCodeInstanceOf of int
	| OpCodeMonitorEnter
	| OpCodeMonitorExit
	| OpCodeAMultiNewArray of int * int (** ClassInfo, dims *)
	| OpCodeIfNull of int
	| OpCodeIfNonNull of int
	| OpCodeGotoW of int
	| OpCodeJsrW of int
	| OpCodeBreakpoint
	| OpCodeRetW of int

	| OpCodeInvalid

type opcodes = opcode array

(** Exception handler. *)
type jexception = {
	e_start : int;
	e_end : int;
	e_handler : int;
	e_catch_type : class_name option
}

(** Stackmap type. *)
type verification_type = 
	| VTop 
	| VInteger 
	| VFloat
	| VDouble
	| VLong
	| VNull
	| VUninitializedThis
	| VObject of object_type
	| VUninitialized of int (** creation point *)

(** {2 Substructures.} *)

type access_flag =
	| AccPublic
	| AccPrivate
	| AccProtected
	| AccStatic
	| AccFinal
	| AccSynchronized (** Also used as "ACC_SUPER" *)
	| AccVolatile
	| AccTransient
	| AccNative
	| AccInterface
	| AccAbstract
	| AccStrict
	| AccRFU of int (** Four bits (RFU 1 .. RFU 4) reserved for future use *)

type access_flags = access_flag list

(* Flags and attributes should be related for fields, methods and classes. *)

type jcode = {
	c_max_stack : int;
	c_max_locals : int;
	c_code : opcodes;
	c_exc_tbl : jexception list;
	c_attributes : attribute list;
}

and attribute =
	| AttributeSourceFile of string
	| AttributeConstant of constant_value
	| AttributeCode of jcode
	| AttributeExceptions of class_name list
	| AttributeInnerClasses of
	    (class_name option * class_name option * string option * access_flags) list
	    (* inner_class_info, outer_class_info, inner_name, inner_class_access_flags *)
	| AttributeSynthetic
	| AttributeLineNumberTable of (int * int) list
	| AttributeLocalVariableTable of (int * int * string * value_type * int) list
	    (* start_pc, length, name, type, index *)
	| AttributeDeprecated
	| AttributeStackMap of (int*(verification_type list)*(verification_type list)) list
	| AttributeUnknown of string * string

type jfield = {
	f_name : string;
	f_signature : field_descriptor;
	f_flags : access_flags (* Should be specialized *);
	f_attributes : attribute list (* Should be specialized *);
}

type jmethod = {
	m_name : string;
	m_signature : method_descriptor;
	m_flags : access_flags (* Should be specialized *);	
	m_code : jcode option; (* Remove that *)
	m_attributes : attribute list (* Should be specialized *);
}

type jclass = {
	j_name : class_name;
	j_super : class_name option (* Remove the option *);
	j_interfaces : class_name list;
	j_consts : constant array;
	j_flags : access_flags (* Should be specialized *);
	j_fields : jfield list;
	j_methods : jmethod list;
	j_attributes : attribute list (* Should be specialized *);
}
