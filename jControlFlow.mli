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

open JBasics
open JClass
open JProgram

(** {2 Access to instructions}*)

(** manipulation of program pointers *)
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
  val get_first_pp_wp : interface_or_class -> method_signature -> t
  val goto_absolute : t -> int -> t
  val goto_relative : t -> int -> t
end

(** The type of program point identifier. *)
type pp = PP.t

val get_opcode : pp -> JClass.opcode
val next_instruction : pp -> pp
val normal_successors : pp -> pp list
val handlers : pp -> JBasics.exception_handler list
val exceptional_successors : pp -> pp list

val static_lookup_static :
  program -> class_name -> method_signature -> class_file list
val static_lookup_interface :
  program -> class_name -> method_signature -> class_file list
val static_lookup_special :
  program -> PP.t -> class_name -> method_signature -> class_file list
val static_lookup_virtual :
  program -> object_type -> method_signature -> class_file list
