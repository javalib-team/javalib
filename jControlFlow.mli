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

(** Allows some navigation in the control flow graph of a program. *)

open JBasics
open JClass
open JProgram

(** {2 Access to instructions}*)

(** Manipulation of program pointers *)
module PP : sig
  type t
  exception NoCode of (class_name * method_signature)
  val get_class : t -> interface_or_class
  val get_meth : t -> concrete_method
  val get_pc : t -> int

  val get_pp : interface_or_class -> concrete_method -> int -> t

  (** [get_first_pp p cn ms] gets a pointer to the first instruction of
      the method [ms] of the class [cn].

      @raise Not_found if [cn] is not a class of [p], or [ms] is not a
      method of [cn].

      @raise NoCode if the method [ms] has no associated code.*)
  val get_first_pp : program -> class_name -> method_signature -> t
  val get_first_pp_wp : interface_or_class -> method_signature_index -> t
  val goto_absolute : t -> int -> t
  val goto_relative : t -> int -> t

  val to_string : t -> string
  val pprint : Format.formatter -> t -> unit

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

(** The type of program point identifier. *)
type pp = PP.t

val get_opcode : pp -> JClass.opcode
val next_instruction : pp -> pp
val normal_successors : pp -> pp list
val handlers : program -> pp -> JBasics.exception_handler list
val exceptional_successors : program -> pp -> pp list



(** {2 Lookup and resolve procedure} *)

(** {b Warning : lookup and resolve functions do not take in account
    visibility yet}! *)


(** [get_class p cn] returns the class named [cn] in program [p], if
    any.
    @raise NoClassDefFoundError if [p] does not contain a class named
    [cn].
*)
val resolve_class : program -> class_name -> interface_or_class

(** [resolve_method ms c] returns the class or interface that defines
    the method [ms], if any.  The caller is responsible to check that
    the class and the method defined in the class are visible from the
    current class.
    @raise NoSuchMethodError if the method is not found
*)
val resolve_method : method_signature_index -> class_file -> interface_or_class
val resolve_method' : method_signature_index -> class_file -> class_file
  (** only look for the method in the superclasses. *)

(** [mplements_method c ms] returns [true] iff the class has a method with the
    signature [ms] and which is not abstract. (Note: The method can be native.)
*)
val implements_method : class_file -> method_signature_index -> bool

(** [resolve_interface_method ms c] return the interface that defines
    the method [ms], or [java.lang.Object] if no interface defines
    [ms] but [Object] does.  The caller is responsible to check that
    the interface and the method defined in the interface are visible
    from the current class.
    @raise NoSuchMethodError if the method is not found.
    @raise IncompatibleClassChangeError if [c] is not an interface.
*)
val resolve_interface_method : method_signature_index -> interface_file -> interface_or_class

(** [resolve_interface_methods' ms i] looks for the methods [ms] in [i]
    and recursively in its interfaces, stopping at every first
    occurence in each hirarchy. It returns the list of interfaces that
    defines [ms]. *)
val resolve_interface_method' :
  ?acc:interface_file list -> method_signature_index -> interface_or_class -> interface_file list

(** [resolve_all_interface_methods ms c] return the list of interfaces
    of [c] that defines the method [ms].  The list is ordered by
    increasing distant in the inheritance hierarchy.  The caller is
    responsible to check that the interface and the method defined in
    the interface are visible from the current class.
*)
val resolve_all_interface_methods : method_signature_index -> interface_file -> interface_file list


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
val lookup_virtual_method : method_signature_index -> class_file -> class_file

(** [lookup_interface_method ms c] return the class that defines the
    method [ms], if any. The caller is responsible to check that the
    class returned is visible from the current class. As the method is
    supposed to have been declared in a interface (and
    [resolve_interface_method] can ensure that), the method is
    necessarily [public].
    @raise AbstractMethodError if the method is not found or if the
    method is abstract.
*)
val lookup_interface_method : method_signature_index -> class_file -> class_file


(** [overriding_methods ms c] looks for the methods that overrides and
    implements [ms] in the children of [c].

    @raise Not_found if [ms] cannot be found in [c]
    @raise Invalid_argument if [ms] is an initialization method
*)
val overridden_by_methods : method_signature_index -> interface_or_class -> class_file list


(** [overridden_methods ms c] looks for the classes that define
    methods that are overridden by [(c,ms)] (in the parents of
    [c]). The result list is ordered such that [c1] is before [c2] iff
    [c1] extends [c2].

    @raise Not_found if [ms] cannot be found in [c]
*)
val overrides_methods : method_signature_index -> class_file -> class_file list

(** [implements_methods ms c] looks for the interfaces that defines
    methods [ms] in the direct interfaces of [c] and recursively in
    their super-interfaces. If [i1] and [i2] defines [ms] and [i1]
    extends [i2], then [i1] is before [i2] in the result list.

    @raise Not_found if [ms] cannot be found in [c]
*)
val implements_methods : method_signature_index -> class_file -> interface_file list



(** static_lookup_* functions return the highest methods that can be
    called, i.e. the method actually called is known to implement or
    override the result.*)
val static_lookup_interface :
  program -> class_name -> method_signature -> interface_or_class list
val static_lookup_special :
  program -> PP.t -> class_name -> method_signature -> class_file
val static_lookup_virtual :
  program -> object_type -> method_signature -> interface_or_class list
