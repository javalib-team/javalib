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

(** Ocaml representation of a Java class file. *)

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
type field_signature = value_type

(** Method signature. *)
type method_signature = value_type list * value_type option

(** Signatures parsed from CONSTANT_NameAndType_info structures. *)
type signature =
  | SValue of field_signature
  | SMethod of method_signature

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
  | ConstField of (class_name * string * field_signature)
  | ConstMethod of (object_type * string * method_signature)
  | ConstInterfaceMethod of (class_name * string * method_signature)
  | ConstNameAndType of string * signature
  | ConstStringUTF8 of string
  | ConstUnusable

(** Instruction. *)
type opcode =

  (* Access to a local variable *)
  | OpLoad of jvm_type * int
  | OpStore of jvm_type * int
  | OpIInc of int * int (** index, increment *)

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

  (* Constant loading *)
  | OpConst of [
      `ANull (** AConstNull  *)
    | `I of int32
    | `L of int64
    | `F of float
    | `D of float
    | `B of int (** BIPush *)
    | `S of int
    ]
  | OpLdc of constant_value

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
  | OpAMultiNewArray of object_type * int (** ClassInfo, dims *)
  | OpCheckCast of object_type
  | OpInstanceOf of object_type
  | OpGetStatic of class_name * string * field_signature
  | OpPutStatic of class_name * string * field_signature
  | OpGetField of class_name * string * field_signature
  | OpPutField of class_name * string * field_signature
  | OpArrayLength
  | OpArrayLoad of jvm_array_type
  | OpArrayStore of jvm_array_type

  (* Method invocation and return *)
  | OpInvoke
      of [
	`Virtual of object_type
      | `Special of class_name
      | `Static of class_name
      | `Interface of class_name
      ]
	* string * method_signature
  | OpReturn of jvm_return_type

  (* Exceptions and threads *)
  | OpThrow
  | OpMonitorEnter
  | OpMonitorExit

  (* Other *)
  | OpNop
  | OpBreakpoint
  | OpInvalid

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
	| AttributeLineNumberTable of (int * int) list
	| AttributeLocalVariableTable of (int * int * string * value_type * int) list
	| AttributeUnknown of string * string
	| AttributeStackMap of (int*(verification_type list)*(verification_type list)) list

type access_flag =
	| AccPublic
	| AccPrivate
	| AccProtected
	| AccStatic
	| AccFinal
	| AccSynchronized (** Also used as "Special" *)
	| AccVolatile
	| AccTransient
	| AccNative
	| AccInterface
	| AccAbstract
	| AccStrict
	| AccRFU of int (** Four bits (RFU 1 .. RFU 4) reserved for future use *)

type access_flags = access_flag list

(* Flags and attributes should be related for fields, methods and classes. *)

type jfield = {
	f_name : string;
	f_signature : field_signature;
	f_flags : access_flags (* Should be specialized *);
	f_attributes : attribute list (* Should be specialized *);
}

type jmethod = {
	m_name : string;
	m_signature : method_signature;
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
