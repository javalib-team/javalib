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

(** High level Ocaml representation of a java byte-code program. *)

open JClassLow
open JClass

(** {2 Navigable hierarchy} *)

(** To be able to navigate more easily in byte-code program, we
    replace class names with pointers to class_file structures (only
    for super_class and implemented interfaces).
*)

module ClassMap : Map.S with type key = class_name

type abstract_class = {
  ac_super_class : class_file option;
  ac_fields : class_field FieldMap.t;
  ac_methods : abstract_class_method MethodMap.t
}

and concrete_class = {
  cc_final : bool;
  cc_super_class : class_file option;
  cc_fields : class_field FieldMap.t;
  cc_methods : concrete_method MethodMap.t
}

and interface = {
  i_super : class_file; (** must be java.lang.Object. *)
  i_fields : interface_field FieldMap.t;
  i_methods : abstract_method MethodMap.t
}

and class_file_type =
    | ConcreteClass of concrete_class
    | AbstractClass of abstract_class
    | Interface of interface

and class_file = {
  name : class_name;
  c_access : [`Public | `Default];
  interfaces : class_file ClassMap.t;
  sourcefile : string option;
  c_deprecated : bool;
  inner_classes : inner_class list;
  c_other_attributes : (string * string) list;
  class_file_type : class_file_type
}

(** {2 The [program] structure} *)

(** A program is a set of class files identified by their name and
    organized as a map. *)
type program  (* should we use an hash map ? *)

type t = program

(** [Class_not_found c] is raised when trying to add a class when its
    super class or one of its implemented interfaces is not in the
    program structure.*)
exception Class_not_found of class_name

(** [parse_program classpath names] parses a list of directories,
    [.jar] files and [.class] files, looking for it in the classpath
    (a list of directories separated with ':') . *)
val parse_program : string -> string list -> t
val add_classFile : JClass.class_file -> t -> t

val load_program : string -> t
val store_program : string -> t -> unit

(** {2 Iterators}*)

val iter : (class_file -> unit) -> program -> unit
val fold : ('b -> class_file -> 'b) -> 'b -> program -> 'b


(** {2 Access functions to fields and methods}*)

type any_method = abstract_class_method
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


(** {b Warning : lookup and resolve functions do not take in account
    visibility yet}! *)

(** [get_class p cn] returns the class named [cn] in program [p], if
    any.
    @raise Not_found if [c] does not contain a method with signature [ms].
*)
val get_class : t -> class_name -> class_file

(** [get_method c ms] returns the method with signature [ms] in class
    [c], if any.
    @raise Not_found if [c] does not contain a method with signature [ms].
*)
val get_method : class_file -> method_signature -> any_method

(** [get_field c fs] returns the field with signature [fs] in class
    [c], if any.
    @raise Not_found if [c] does not contain a field with signature [fs].
*)
val get_field : class_file -> field_signature -> any_field

(** [get_class p cn] returns the class named [cn] in program [p], if
    any.
    @raise NoClassDefFoundError if [p] does not contain a class named
    [cn].
*)
val resolve_class : program -> class_name -> class_file

(** [resolve_method ms c] return the class or interface that defines
    the method [ms], if any.  The caller is responsible to check that
    the class and the method defined in the class are visible from the
    current class.
    @raise NoSuchMethodError if the method is not found
*)
val resolve_method : method_signature -> class_file -> class_file

(** [resolve_interface_method ms c] return the interface that defines
    the method [ms], if any.  The caller is responsible to check that
    the interface and the method defined in the interface are visible
    from the current class.
    @raise NoSuchMethodError if the method is not found.
    @raise IncompatibleClassChangeError if [c] is not an interface.
*)
val resolve_interface_method : method_signature -> class_file -> class_file

(** [resolve_field fs c] returns the class or interface that defines
    the field [fs], if any.
    @raise NoSuchFieldError if the field is not found
    @see <http://java.sun.com/docs/books/jvms/second_edition/html/ConstantPool.doc.html#71685> Field Resolution
*)
val resolve_field : field_signature -> class_file -> class_file

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


(** {2 Access to the hierarchy} *)

type className
type interfaceName

(** [classOrInterfaceName_of_ident p cn] returns a className or
    interfaceName, depending on whether [cn] corresponds to a class or an
    interface in [p].
    @raise Not_found if [cn] is not defined in [p]*)
val classOrInterfaceName_of_ident : t -> class_name ->
   [`Class of className | `Interface of interfaceName]

(** [extends_class p cn1 cn2] returns [true] if [cn2] is a super-class
    of [cn1].
    @raise Not_found if [cn1] or [cn2] cannot be found in [p]. *)
val extends_class : t ->  className -> className -> bool

(** [extends_interface p in1 in2] returns true if [in2] is a
    super-interface of [in1].
    @raise Not_found if [in1] cannot be found in [p]. *)
val extends_interface : t ->  interfaceName -> interfaceName -> bool

(** [implements p cn1 in2] returns true if [in2] is a
    super-interface of [cn1].
    @raise Not_found if [cn1] cannot be found in [p]. *)
val implements : t ->  className -> interfaceName -> bool


(** [super_class p cn] returns the super class of cn.
    @raise Not_found if [cn] is not in [p] or if [cn] has no super
    class. *)
val super_class : t -> className -> className

(** [implemented_interfaces p cn] returns the interfaces implemented
    by [cn], super-classes of [cn], or extended by those
    super-interfaces. *)
val implemented_interfaces : t -> className -> interfaceName list

(** [super_interfaces p iname] returns the explicit and implicit
    super-interfaces of [iname].*)
val super_interfaces : t -> interfaceName -> interfaceName list

(* val firstCommonSuperClass : t -> className -> className -> className *)
