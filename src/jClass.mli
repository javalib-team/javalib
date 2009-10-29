(*
 * This file is part of JavaLib
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
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

(** High level Ocaml representation of a Java class file. *)

open JBasics
open JCode

(** {2 Fields access and attributes.} *)

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
  (** correspond to the attribute, not to the flag (cf. JVM Spec 1.5
      §4.2, §4.6, §4.7 and §4.8.7) *)
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
  cf_class_signature : class_field_signature;
  cf_generic_signature : JSignature.fieldTypeSignature option;
  cf_access: access;
  cf_static : bool;
  cf_synthetic : bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.6 and §4.8.7) *)
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
  if_class_signature : class_field_signature;
  if_generic_signature : JSignature.fieldTypeSignature option;
  if_synthetic : bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.6 and §4.8.7) *)
  if_value : constant_value option;
  (** a constant_value is not mandatory, especially as it can be
      initialized by the class initializer <clinit>. *)
  if_other_flags : int list;
  if_attributes : attributes
}

type any_field = | InterfaceField of interface_field | ClassField of class_field

(** {2 Methods of classes and interfaces.} *)
(********************************)

type 'a implementation =
  | Native
  | Java of 'a Lazy.t

(* l'attribut final n'a pas vraiment de sens pour une méthode
   statique, mais c'est autorisé dans la spec JVM. *)
type 'a concrete_method = {
  cm_signature : method_signature;
  cm_class_method_signature : class_method_signature;
  cm_static : bool;
  cm_final : bool;
  cm_synchronized : bool;
  cm_strict : bool;
  cm_access: access;
  cm_generic_signature : JSignature.methodTypeSignature option;
  cm_bridge: bool;
  cm_varargs : bool;
  cm_synthetic : bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.7 and §4.8.7) *)
  cm_other_flags : int list;
  cm_exceptions : class_name list;
  cm_attributes : attributes;
  cm_implementation : 'a implementation;
}

type abstract_method = {
  am_signature : method_signature;
  am_class_method_signature : class_method_signature;
  am_access: [`Public | `Protected | `Default];
  am_generic_signature : JSignature.methodTypeSignature option;
  am_bridge: bool;
  am_varargs: bool;
  am_synthetic: bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.7 and §4.8.7) *)
  am_other_flags : int list;
  am_exceptions : class_name list;
  am_attributes : attributes;
}


type 'a jmethod =
    | AbstractMethod of abstract_method
    | ConcreteMethod of 'a concrete_method

(** {2 Classes and interfaces.} *)
(***************************)

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

type 'a jclass = {
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
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.2 and §4.8.7) *)
  c_enum: bool;
  c_other_flags : int list;
  c_other_attributes : (string * string) list;
  c_methods : 'a jmethod MethodMap.t;
}

(** Interfaces cannot be final and can only contains abstract
    methods. Their super class is [java.lang.Object].*)
type 'a jinterface = {
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
  i_initializer : 'a concrete_method option; (* should be static/ signature is <clinit>()V; *)
  i_annotation: bool;
  i_other_flags : int list;
  i_fields : interface_field FieldMap.t;
  i_methods : abstract_method MethodMap.t
}

type 'a interface_or_class =
  | JInterface of 'a jinterface
  | JClass of 'a jclass

(** {2 Classes access functions.} *)

val get_name : 'a interface_or_class -> class_name
val get_consts : 'a interface_or_class -> constant array
val get_access : 'a interface_or_class -> [`Default | `Public]
val get_sourcefile : 'a interface_or_class -> string option
val is_deprecated : 'a interface_or_class -> bool
val get_inner_classes : 'a interface_or_class -> inner_class list
val get_other_attributes : 'a interface_or_class -> (string * string) list
val get_initializer : 'a interface_or_class -> 'a concrete_method option
val get_other_flags : 'a interface_or_class -> int list

(** {2 Methods access functions.} *)

val get_method_signature : 'a jmethod -> method_signature
val get_class_method_signature : 'a jmethod -> class_method_signature
val is_static_method : 'a jmethod -> bool
val is_final_method : 'a jmethod -> bool
val is_synchronized_method : 'a jmethod -> bool
val get_method : 'a interface_or_class -> method_signature -> 'a jmethod
val get_methods : 'a interface_or_class -> 'a jmethod MethodMap.t
val get_concrete_methods : 'a interface_or_class -> 'a concrete_method MethodMap.t
val defines_method : 'a interface_or_class -> method_signature -> bool

(** {2 Fields access functions.} *)

val get_field_signature : any_field -> field_signature
val get_class_field_signature : any_field -> class_field_signature
val get_field : 'a interface_or_class -> field_signature -> any_field
val get_fields : 'a interface_or_class -> any_field FieldMap.t
val defines_field : 'a interface_or_class -> field_signature -> bool

(** {2 Iterators.} *)

(** The following functions iterate over all methods of a class or interface
    (including the static initializer, if any). *)

val iter_methods : (method_signature -> 'a jmethod -> unit) -> 'a interface_or_class -> unit
val iter_concrete_methods : (method_signature -> 'a concrete_method -> unit) -> 'a interface_or_class -> unit

val iter_fields : (field_signature -> any_field -> unit) -> 'a interface_or_class -> unit

(** {2 Transforming code representation.} *)

val map_concrete_method :
  ('a -> 'b) -> 'a concrete_method -> 'b concrete_method

val map_class : ('a -> 'b) -> 'a jclass -> 'b jclass
val map_interface : ('a -> 'b) -> 'a jinterface -> 'b jinterface
val map_interface_or_class :
  ('a -> 'b) -> 'a interface_or_class -> 'b interface_or_class

val map_class_context :
  ('a concrete_method -> 'a -> 'b) -> 'a jclass -> 'b jclass
val map_interface_context :
  ('a concrete_method -> 'a -> 'b) -> 'a jinterface -> 'b jinterface
val map_interface_or_class_context :
  ('a concrete_method -> 'a -> 'b) -> 'a interface_or_class -> 'b interface_or_class
