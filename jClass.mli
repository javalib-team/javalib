(*
 *  This file is part of JavaLib
 *  Copyright (c)2007 Université de Rennes 1 / CNRS
 *  Tiphaine.Turpin@irisa.fr
 *  Laurent.Hubert@irisa.fr
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

open JClassLow

type field_signature = {
  fs_name:string;
  fs_type:value_type;
}

type method_signature = {
  ms_name:string;
  ms_parameters:value_type list;
  ms_return_value:value_type option;
}

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

type field_type =
  | NotFinal
  | Final
  | Volatile

type class_field = {
  cf_access: access;
  cf_static : bool;
  cf_type : field_type;
  cf_value : constant_value option; (** Only if the field is static final. *)
  cf_transient : bool;
  cf_attributes : attributes
}

(** Fields of interfaces are implicitly [public], [static] and
    [final].*)
type interface_field = {
  if_value : constant_value option; (** a constant_value is not mandatory, especially as it can be initialized by the class initializer <clinit>. *)
  if_attributes : attributes
}

(** {2 Methods of classes and interfaces.} *)
(********************************)

type code = {
  c_max_stack : int;
  c_max_locals : int;
  c_code : opcodes;
  c_exc_tbl : jexception list;
  line_number_table : (int * int) list option;
  local_variable_table : (int * int * string * value_type * int) list option;
  stack_map : (int* verification_type list * verification_type list) list option;
  (** This is the MIDP version, not the JSR 202 StackMapTable attribute. *)
  c_attributes : (string * string) list;
}

type implementation =
  | Native
  | Java of code

(* l'attribut final n'a pas vraiment de sens pour une méthode
   statique, mais c'est autorisé dans la spec JVM. *)
type concrete_method = {
  nm_static : bool;
  nm_final : bool;
  nm_synchronized : bool;
  nm_strict : bool;
  nm_access: access;
  nm_exceptions : class_name list;
  nm_attributes : attributes;
  implementation : implementation
}

type abstract_method = {
  am_access: [`Public | `Protected | `Default];
  am_exceptions : class_name list;
  am_attributes : attributes
}

(* Contrainte supplémentaire : une méthode d'initialisation (d'instance)
   est forcément concrète, et ne peut pas être statique, finale ou
   synchronisée :

   "A specific instance initialization method (§3.9) may have at most one
   of its ACC_PRIVATE, ACC_PROTECTED, and ACC_PUBLIC flags set and may also
   have its ACC_STRICT flag set, but may not have any of the other flags
   in Table 4.5 set."

   L: => on peut typer differement les méthodes d'initialisation (class
   et instance), mais ça risque d'allourdir les choses.
*)


(** {2 Classes and interfaces.} *)
(***************************)

module FieldMap : Map.S with type key = field_signature
module MethodMap : Map.S with type key = method_signature

type abstract_class_method =
    | AbstractMethod of abstract_method
    | ConcreteMethod of concrete_method

(** Abstract classes cannot be final and may contain abstract methods.*)
type abstract_class = {
  ac_super_class : class_name option;
  ac_fields : class_field FieldMap.t;
  ac_methods : abstract_class_method MethodMap.t
}

(** Concrete classes cannot contains abstract methods.*)
type normal_class = {
  nc_final : bool;
  nc_super_class : class_name option;
  nc_fields : class_field FieldMap.t;
  nc_methods : concrete_method MethodMap.t
}

(** Interfaces cannot be final and can only contains abstract
    methods. Their super class is [java.lang.Object].*)
type interface = {
  (* super-class must be java.lang.Object. *)
  i_initializer : concrete_method option; (* should be static/ signature is <clinit>()V; *)
  i_fields : interface_field FieldMap.t;
  i_methods : abstract_method MethodMap.t
}

type inner_class = {
  ic_class_name : class_name option;
  ic_outer_class_name : class_name option;
  ic_source_name : string option;
  ic_access : access;
  ic_static : bool;
  ic_final : bool;
  ic_type : [`ConcreteClass|`Abstract|`Interface]
}

type class_file_type =
  | ConcreteClass of normal_class
  | AbstractClass of abstract_class
  | Interface of interface


type class_file = {
  name : class_name;
  c_access : [`Public | `Default];
  interfaces : class_name list;
  c_consts : constant array; (** needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  sourcefile : string option;
  c_deprecated : bool;
  inner_classes : inner_class list;
  c_other_attributes : (string * string) list;
  class_file_type : class_file_type
}
