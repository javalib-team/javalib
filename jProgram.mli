(*
 *  This file is part of JavaLib
 *  Copyright (c)2007 Université de Rennes 1 / CNRS
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

(** High level Ocaml representation of a java byte-code program. *)

open JBasics
open JClass

(** {2 Navigable hierarchy} *)

(** To be able to navigate more easily in byte-code program, we
    replace class names with pointers to class_file structures (only
    for super_class and implemented interfaces).
*)

module ClassMap : Map.S with type key = class_name


type class_file = {
  c_name : class_name;
  c_access : [`Public | `Default];
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
  c_inner_classes : inner_class list;
  c_other_attributes : (string * string) list;
  c_methods : jmethod MethodMap.t;
  mutable c_children : class_file ClassMap.t;
}

and interface_file = {
  i_name : class_name;
  i_access : [`Public | `Default];
  i_annotation: bool;
  i_other_flags : int list;
  i_interfaces : interface_file ClassMap.t;
  i_consts : constant array;
  (** needed at least for unparsed/unknown attributes that might refer
      to the constant pool. *)
  i_sourcefile : string option;
  i_deprecated : bool;
  i_inner_classes : inner_class list;
  i_other_attributes : (string * string) list;
  i_super : class_file;
  (** must be java.lang.Object. But note that interfaces are not
      considered as children of java.lang.Object.*)
  i_initializer : concrete_method option; (** should be static/ signature is <clinit>()V; *)
  i_fields : interface_field FieldMap.t;
  i_methods : abstract_method MethodMap.t;
  mutable i_children_interface : interface_file ClassMap.t;
  mutable i_children_class : class_file ClassMap.t;
}


type interface_or_class = [
| `Interface of interface_file
| `Class of class_file
]

val get_name : interface_or_class -> class_name
val get_interfaces : interface_or_class -> interface_file ClassMap.t

(** [to_class c] return the same class but in the representation of
    JClass, i.e. without pointers for classes.*)
val to_class : interface_or_class -> JClass.interface_or_class

(** {2 The [program] structure} *)

(** A program is a set of class files identified by their name and
    organized as a map. *)
type program  (* should we use an hash map ? *)

type t = program

(** [Class_not_found c] is raised when trying to add a class when its
    super class or one of its implemented interfaces is not in the
    program structure.*)
exception Class_not_found of class_name

(** [parse_program classpath names] parses a list of [.jar] and
    [.class] files, looking for them in the classpath (a list of
    directories and [.jar] files separated with ':') . *)
val parse_program : string -> string list -> t
val add_file :
  JFile.class_path -> JClass.interface_or_class -> program -> program

(** @raise Sys_error if the file could not be opened. *)
val load_program : string -> t
val store_program : string -> t -> unit

(** {2 Iterators}*)

val iter : (interface_or_class -> unit) -> program -> unit
val fold : ('b -> interface_or_class -> 'b) -> 'b -> program -> 'b


(** {2 `Access functions to fields and methods}*)

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


(** {b Warning : lookup and resolve functions do not take in account
    visibility yet}! *)

(** [get_class p cn] returns the class named [cn] in program [p], if
    any.
    @raise Not_found if [p] does not contain a class named [cn].
*)
val get_interface_or_class : t -> class_name -> interface_or_class
(* val get_class : t -> class_name -> class_file *)
(* val get_interface : t -> class_name -> interface_file *)

(** [get_method c ms] returns the method with signature [ms] in class
    [c], if any.
    @raise Not_found if [c] does not contain a method with signature [ms].
*)
val get_method : interface_or_class -> method_signature -> jmethod
val get_methods : interface_or_class -> method_signature list

(** [get_field c fs] returns the field with signature [fs] in class
    [c], if any.
    @raise Not_found if [c] does not contain a field with signature [fs].
*)
val get_field : interface_or_class -> field_signature -> any_field
val get_fields : interface_or_class -> field_signature list

(** [get_class p cn] returns the class named [cn] in program [p], if
    any.
    @raise NoClassDefFoundError if [p] does not contain a class named
    [cn].
*)
val resolve_class : program -> class_name -> interface_or_class

(** [resolve_method ms c] return the class or interface that defines
    the method [ms], if any.  The caller is responsible to check that
    the class and the method defined in the class are visible from the
    current class.
    @raise NoSuchMethodError if the method is not found
*)
val resolve_method : method_signature -> class_file -> interface_or_class

(** [resolve_interface_method ms c] return the interface that defines
    the method [ms], or [java.lang.Object] if no interface defines
    [ms] but [Object] does.  The caller is responsible to check that
    the interface and the method defined in the interface are visible
    from the current class.
    @raise NoSuchMethodError if the method is not found.
    @raise IncompatibleClassChangeError if [c] is not an interface.
*)
val resolve_interface_method : method_signature -> interface_file -> interface_or_class

(** [resolve_all_interface_methods ms c] return the list of interfaces
    of [c] that defines the method [ms].  The list is ordered by
    increasing distant in the inheritance hierarchy.  The caller is
    responsible to check that the interface and the method defined in
    the interface are visible from the current class.
*)
val resolve_all_interface_methods : method_signature -> interface_file -> interface_file list


(** [resolve_field fs c] returns the class or interface that defines
    the field [fs], if any.
    @raise NoSuchFieldError if the field is not found
    @see <http://java.sun.com/docs/books/jvms/second_edition/html/ConstantPool.doc.html#71685> Field Resolution
*)
val resolve_field : field_signature -> interface_or_class -> interface_or_class

(** [lookup_virtual_method ms c] return the class that defines the
    method [ms], if any.  The caller is responsible to check that the
    class and the method defined in the class are visible from the
    current class.
    @raise AbstractMethodError if the method is not found or if the
    method is abstract.
*)
val lookup_virtual_method : method_signature -> class_file -> class_file

(** [lookup_interface_method ms c] return the class that defines the
    method [ms], if any. The caller is responsible to check that the
    class returned is visible from the current class. As the method is
    supposed to have been declared in a interface (and
    [resolve_interface_method] can ensure that), the method is
    necessarily [public].
    @raise AbstractMethodError if the method is not found or if the
    method is abstract.
*)
val lookup_interface_method : method_signature -> class_file -> class_file


(** [overriding_methods ms c] looks for the methods that overrides and
    implements [ms] in the children of [c].

    @raise Not_found if [ms] cannot be found in [c]
*)
val overridden_by_methods : method_signature -> interface_or_class -> class_file list


(** [overridden_methods ms c] looks for the classes that define
    methods that are overridden by [(c,ms)] (in the parents of
    [c]). The result list is ordered such that [c1] is before [c2] iff
    [c1] extends [c2].

    @raise Not_found if [ms] cannot be found in [c]
*)
val overrides_methods : method_signature -> class_file -> class_file list

(** [implements_methods ms c] looks for the interfaces that defines
    methods [ms] in the direct interfaces of [c] and recursively in
    their super-interfaces. If [i1] and [i2] defines [ms] and [i1]
    extends [i2], then [i1] is before [i2] in the result list.

    @raise Not_found if [ms] cannot be found in [c]
*)
val implements_methods : method_signature -> class_file -> interface_file list

(** {2 `Access to the hierarchy} *)

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
