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

(** Defines high level Ocaml representation of a java byte-code program. *)

open JBasics
open JClass

(** {2 Navigable hierarchy} *)

(** To be able to navigate more easily in byte-code program, class
    names are replaced by pointers to class_file structures ( for
    super_class, implemented interfaces and sub-classes or
    sub-interfaces).
*)

module ClassIndexMap : Map.S with type key = class_name
module MethodIndexMap : Map.S with type key = method_signature
module FieldIndexMap : Map.S with type key = field_signature

module ClassMap : Ptmap.S
module MethodMap : Ptmap.S
module FieldMap : Ptmap.S

type field_signature_index = int
type field_signature_index_table =
    { mutable fsi_map : field_signature_index FieldIndexMap.t;
      mutable fs_map : field_signature FieldMap.t;
      mutable fsi_next : field_signature_index }
type method_signature_index = int
type method_signature_index_table =
    { mutable msi_map : method_signature_index MethodIndexMap.t;
      mutable ms_map : method_signature MethodMap.t;
      mutable msi_next : method_signature_index }
type class_name_index = int
type class_name_index_table =
    { mutable cni_map : class_name_index ClassIndexMap.t;
      mutable cn_map : class_name ClassMap.t;
      mutable cni_next : class_name_index }

type dictionary = { msi_table : method_signature_index_table;
		    cni_table : class_name_index_table;
		    fsi_table : field_signature_index_table;
		    get_fs_index : FieldIndexMap.key -> field_signature_index;
		    get_ms_index : MethodIndexMap.key -> method_signature_index;
		    get_cn_index : ClassIndexMap.key -> class_name_index;
		    retrieve_fs : field_signature_index -> field_signature;
		    retrieve_ms : method_signature_index -> method_signature;
		    retrieve_cn : class_name_index -> class_name }

val clinit_index : int
val init_index : int

val java_lang_object_index : int

val make_dictionary : unit -> dictionary

module ClassMethSet : Set.S with type elt = int * int
module ClassMethMap : Map.S with type key = int * int

type concrete_method = {
  mutable cm_has_been_parsed : bool;
  cm_index : method_signature_index;
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
  mutable cm_overridden_in : class_file list; (* TODO: is useful ? *)
}

and abstract_method = {
  am_index : method_signature_index;
  am_signature : method_signature;
  am_access: [`Public | `Protected | `Default];
  am_generic_signature : JSignature.methodTypeSignature option;
  am_bridge: bool;
  am_varargs: bool;
  am_synthetic: bool;
  am_other_flags : int list;
  am_exceptions : class_name list;
  am_attributes : attributes;
  mutable am_overridden_in : interface_or_class list; (* TODO: is useful ? *)
}

and jmethod =
  | AbstractMethod of abstract_method
  | ConcreteMethod of concrete_method

and class_file = {
  c_name : class_name;
  c_index : class_name_index;
  c_version : version;
  c_access : [`Public | `Default];
  c_generic_signature : JSignature.classSignature option;
  c_final : bool;
  c_abstract : bool;
  c_synthetic: bool;
  c_enum: bool;
  c_other_flags : int list;
  c_super_class : class_file option;
  c_fields : class_field FieldMap.t;
  c_interfaces : interface_file ClassMap.t;
  c_consts : constant array;
  (** needed at least for unparsed/unknown attributes that might refer
      to the constant pool. *)
  c_sourcefile : string option;
  c_deprecated : bool;
  c_enclosing_method : (class_name * method_signature option) option;
  (** introduced with Java 5 for local classes (defined in methods'
      code). The first element is innermost class that encloses the
      declaration of the current class. The second element is the
      method that enlcoses this class definition. cf
      {{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS},
      paragraph 4.8.6.*)
  c_source_debug_extention : string option;
  (** Introduced in Java 5 for debugging purpose (no
      semantics defined)
      ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}). *)
  c_inner_classes : inner_class list;
  c_other_attributes : (string * string) list;
  c_methods : jmethod MethodMap.t;
  mutable c_resolve_methods : (class_file * jmethod) MethodMap.t;
  mutable c_may_be_instanciated : bool;
  mutable c_children : class_file ClassMap.t; (* a set would be more appropriate*)
}

and interface_file = {
  i_name : class_name;
  i_index : class_name_index;
  i_version : version;
  i_access : [`Public | `Default];
  i_generic_signature : JSignature.classSignature option;
  i_annotation: bool;
  i_other_flags : int list;
  i_interfaces : interface_file ClassMap.t;
  i_consts : constant array;
  (** needed at least for unparsed/unknown attributes that might refer
      to the constant pool. *)
  i_sourcefile : string option;
  i_deprecated : bool;
  i_source_debug_extention : string option;
  (** Introduced in Java 5 for debugging purpose (no
      semantics defined)
      ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}). *)
  i_inner_classes : inner_class list;
  i_other_attributes : (string * string) list;
  i_super : class_file;
  (** must be java.lang.Object. But note that interfaces are not
      considered as children of java.lang.Object.*)
  i_initializer : concrete_method option; (** should be static/ signature is <clinit>()V; *)
  i_fields : interface_field FieldMap.t;
  i_methods : abstract_method MethodMap.t;
  mutable i_children_interface : interface_file ClassMap.t; (* a set would be more appropriate*)
  mutable i_children_class : class_file ClassMap.t; (* a set would be more appropriate*)
}
and  interface_or_class = [
| `Interface of interface_file
| `Class of class_file
]

val get_name : interface_or_class -> class_name
val get_index : interface_or_class -> class_name_index
val get_interfaces : interface_or_class -> interface_file ClassMap.t

val equal : interface_or_class -> interface_or_class -> bool

val is_static_method : jmethod -> bool
val get_method_signature : jmethod -> method_signature

(** [to_class c] return the same class but in the representation of
    JClass, i.e. without pointers for classes.*)
val to_class : interface_or_class -> JClass.interface_or_class

(** {2 The [program] structure} *)

(** A program is a record containing a map of class files identified by
    an id, and a dictionary containing functions to retrieve classes and
    methods ids from their names. *)
type program = { classes : interface_or_class ClassMap.t;
		 static_lookup : class_name_index -> method_signature_index ->
							  int -> ClassMethSet.t;
		 dictionary : dictionary }

val ccm2pcm : dictionary -> JClass.concrete_method -> concrete_method
val cam2pam : dictionary -> JClass.abstract_method -> abstract_method
val declare_method : interface_or_class -> method_signature_index -> unit

(** [Class_not_found c] is raised when trying to add a class when its
    super class or one of its implemented interfaces is not in the
    program structure.*)
exception Class_not_found of class_name

(** @raise Sys_error if the file could not be opened. *)
val load_program : string -> program
val store_program : string -> program -> unit

(** {2 Iterators}*)

val iter : (interface_or_class -> unit) -> program -> unit
val fold : ('b -> interface_or_class -> 'b) -> 'b -> program -> 'b


(** {2 Access functions to fields and methods}*)

type any_field = | InterfaceField of interface_field | ClassField of class_field

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception IncompatibleClassChangeError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception NoSuchMethodError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception NoSuchFieldError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception NoClassDefFoundError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception AbstractMethodError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception IllegalAccessError


(** [get_interface_or_class p cn] returns the class named [cn] in
    program [p], if any.
    @raise Not_found if [p] does not contain a class named [cn].
*)
val get_interface_or_class : program -> class_name -> interface_or_class
(* val get_class : t -> class_name -> class_file *)
(* val get_interface : t -> class_name -> interface_file *)

(** [get_method c ms] returns the method with signature [ms] in class
    [c], if any.
    @raise Not_found if [c] does not contain a method with signature [ms].
*)
val get_method : interface_or_class -> method_signature_index -> jmethod
val get_methods : interface_or_class -> method_signature_index list
val defines_method : method_signature_index -> interface_or_class -> bool

(** [get_field c fs] returns the field with signature [fs] in class
    [c], if any.
    @raise Not_found if [c] does not contain a field with signature [fs].
*)
val get_field : interface_or_class -> field_signature_index -> any_field
val get_fields : interface_or_class -> field_signature_index list
val defines_field : field_signature_index -> interface_or_class -> bool


(** {2 Access to the hierarchy} *)

(** [extends_class p cn1 cn2] returns [true] if [cn2] is a super-class
    of [cn1]. An class extends itself.
    @raise Not_found if [cn1] or [cn2] cannot be found in [p]. *)
val extends_class : class_file -> class_file -> bool

(** [extends_interface p in1 in2] returns true if [in2] is a
    super-interface of [in1]. An interface extends itself.
    @raise Not_found if [in1] cannot be found in [p]. *)
val extends_interface : interface_file -> interface_file -> bool

(** [implements p cn1 in2] returns true if [in2] is a
    super-interface of [cn1].
    @raise Not_found if [cn1] cannot be found in [p]. *)
val implements : class_file -> interface_file -> bool

(** [super_class p cn] returns the super class of cn.
    @raise Not_found if [cn] is not in [p] or if [cn] has no super
    class. *)
val super_class : interface_or_class -> class_file option

(** [implemented_interfaces p cn] returns the interfaces implemented
    by [cn], super-classes of [cn], or extended by those
    super-interfaces. *)
val implemented_interfaces : class_file -> interface_file list

(** [super_interfaces p iname] returns the explicit and implicit
    super-interfaces of [iname].*)
val super_interfaces : interface_file -> interface_file list

val firstCommonSuperClass : class_file -> class_file -> class_file

(* Mainly for test purposes... *)
module ClassnameSet : Set.S with type elt = JBasics.class_name
module MethodSet : Set.S with type elt = JClass.method_signature

val get_loaded_classes : program -> ClassnameSet.t

val get_loaded_methods : program -> MethodSet.t

val get_instantiated_classes : program -> ClassnameSet.t
