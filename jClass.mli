(*
 *  This file is part of JavaLib
 *  Copyright (c)2007-2008 Université de Rennes 1 / CNRS
 *  Tiphaine Turpin <first.last@irisa.fr>
 *  Laurent Hubert <first.last@irisa.fr>
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

(** High level Ocaml representation of a Java class file. *)

open JBasics

type field_signature = {
  fs_name:string;
  fs_type:value_type;
}

type method_signature = {
  ms_name:string;
  ms_parameters:value_type list;
  ms_return_type : value_type option;
  (* Note : 2 methods in the same class can can differ only by their
     return type in case of bridge methods. *)
}

val clinit_signature : method_signature
val init_signature : method_signature
val main_signature : method_signature


(** {2 Bytecode instructions.} *)
(********************************)

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

  (* Constant loading / it corresponds to instructions *const* and ldc* *)
  | OpConst of [
    | `ANull (** AConstNull  *)
    | `Int of int32
    | `Long of int64
    | `Float of float
    | `Double of float
    | `Byte of int (** BIPush *)
    | `Short of int
    | `String of string
    | `Class of object_type
    ]

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
  | OpGetStatic of class_name * field_signature
  | OpPutStatic of class_name * field_signature
  | OpGetField of class_name * field_signature
  | OpPutField of class_name * field_signature
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

type opcodes = opcode array

(** Visibility modifiers. *)
type access = [
| `Default
| `Public
| `Private
| `Protected
]

(** Generic attributes common to classes, fields and methods. *)
type attributes = {
  synthetic : bool;
  deprecated : bool;
  other : (string * string) list
}

(** {2 Fields of classes and interfaces.} *)
(*******************************)

type field_kind =
  | NotFinal
  | Final
  | Volatile

type class_field = {
  cf_signature : field_signature;
  cf_generic_signature : JSignature.fieldTypeSignature option;
  cf_access: access;
  cf_static : bool;
  cf_synthetic : bool;
  cf_enum : bool;
  cf_kind : field_kind;
  cf_value : constant_value option; (** Only if the field is static final. *)
  cf_transient : bool;
  cf_other_flags : int list;
  cf_attributes : attributes
}

(** Fields of interfaces are implicitly [public], [static] and
    [final].*)
type interface_field = {
  if_signature : field_signature;
  if_generic_signature : JSignature.fieldTypeSignature option;
  if_synthetic : bool;
  if_value : constant_value option; (** a constant_value is not mandatory, especially as it can be initialized by the class initializer <clinit>. *)
  if_other_flags : int list;
  if_attributes : attributes
}

(** {2 Methods of classes and interfaces.} *)
(********************************)

type code = {
  c_max_stack : int;
  c_max_locals : int;
  c_code : opcodes;
  c_exc_tbl : exception_handler list;
  c_line_number_table : (int * int) list option;
  c_local_variable_table : (int * int * string * value_type * int) list option;
  c_stack_map : (int* verification_type list * verification_type list) list option;
  (** This is the MIDP version, not the JSR 202 StackMapTable attribute. *)
  c_attributes : (string * string) list;
}

type implementation =
  | Native
  | Java of code Lazy.t

(* l'attribut final n'a pas vraiment de sens pour une méthode
   statique, mais c'est autorisé dans la spec JVM. *)
type concrete_method = {
  cm_signature : method_signature;
  cm_static : bool;
  cm_final : bool;
  cm_synchronized : bool;
  cm_strict : bool;
  cm_access: access;
  cm_generic_signature : JSignature.methodTypeSignature option;
  cm_bridge: bool;
  cm_varargs : bool;
  cm_synthetic : bool;
  cm_other_flags : int list;
  cm_exceptions : class_name list;
  cm_attributes : attributes;
  cm_implementation : implementation;
}

type abstract_method = {
  am_signature : method_signature;
  am_access: [`Public | `Protected | `Default];
  am_generic_signature : JSignature.methodTypeSignature option;
  am_bridge: bool;
  am_varargs: bool;
  am_synthetic: bool;
  am_other_flags : int list;
  am_exceptions : class_name list;
  am_attributes : attributes;
}

(** {2 Classes and interfaces.} *)
(***************************)

module FieldMap : Map.S with type key = field_signature
module MethodMap : Map.S with type key = method_signature

type jmethod =
    | AbstractMethod of abstract_method
    | ConcreteMethod of concrete_method

type inner_class = {
  ic_class_name : class_name option;
  ic_outer_class_name : class_name option;
  ic_source_name : string option;
  ic_access : access;
  ic_static : bool;
  ic_final : bool;
  ic_synthetic: bool;
  ic_annotation: bool;
  ic_enum: bool;
  ic_other_flags : int list;
  ic_type : [`ConcreteClass | `Abstract | `Interface]
}

type jclass = {
  c_name : class_name;
  c_version : version;
  c_access : [`Public | `Default];
  c_final : bool;
  c_abstract : bool;
  c_super_class : class_name option;
  c_generic_signature : JSignature.classSignature option;
  c_fields : class_field FieldMap.t;
  c_interfaces : class_name list;
  c_consts : constant array; (** needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  c_sourcefile : string option;
  c_deprecated : bool;
  c_enclosing_method : (class_name * method_signature option) option;
  (** introduced with Java 5 for local classes (defined in methods'
      code). The first element is innermost class that encloses the
      declaration of the current class. The second element is the
      method that encose this class definition. cf
      {{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS},
      paragraph 4.8.6.*)
  c_source_debug_extention : string option;
  (** Introduced in Java 5 for debugging purpose (no
      semantics defined)
      ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}). *)
  c_inner_classes : inner_class list;
  c_synthetic: bool;
  c_enum: bool;
  c_other_flags : int list;
  c_other_attributes : (string * string) list;
  c_methods : jmethod MethodMap.t;
}

(** Interfaces cannot be final and can only contains abstract
    methods. Their super class is [java.lang.Object].*)
type jinterface = {
  i_name : class_name;
  i_version : version;
  i_access : [`Public | `Default];
  i_interfaces : class_name list;
  i_generic_signature : JSignature.classSignature option;
  i_consts : constant array; (** needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  i_sourcefile : string option;
  i_deprecated : bool;
  i_source_debug_extention : string option;
  (** Introduced in Java 5 for debugging purpose (no
      semantics defined)
      ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}). *)
  i_inner_classes : inner_class list;
  i_other_attributes : (string * string) list;
  i_initializer : concrete_method option; (* should be static/ signature is <clinit>()V; *)
  i_annotation: bool;
  i_other_flags : int list;
  i_fields : interface_field FieldMap.t;
  i_methods : abstract_method MethodMap.t
}

(* Les polymorphic variants servent juste pour simplifier JProgram en évitant
   les préfixe (faudrait enlever ça dans l'idéal). *)
type interface_or_class = [
  | `Interface of jinterface
  | `Class of jclass
]

(** {2 Access functions.} *)

val get_name : interface_or_class -> class_name
val get_consts : interface_or_class -> constant array
val get_access : interface_or_class -> [`Default | `Public]
val get_sourcefile : interface_or_class -> string option
val is_deprecated : interface_or_class -> bool
val get_inner_classes : interface_or_class -> inner_class list
val get_other_attributes : interface_or_class -> (string * string) list
val get_initializer : interface_or_class -> concrete_method option
val get_other_flags : interface_or_class -> int list

(** The following functions iterate over all methods of a class or interface
    (including the static initializer, if any). *)

val iter_methods : (method_signature -> jmethod -> unit) -> interface_or_class -> unit
val iter_concrete_methods : (method_signature -> concrete_method -> unit) -> interface_or_class -> unit

val iter_fields : (field_signature -> [`ClassField of class_field | `InterfaceField of interface_field] -> unit) -> interface_or_class -> unit
