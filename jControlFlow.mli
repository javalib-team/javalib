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

open JBasics
open JClass
open JProgram

(** {2 Access to instructions}*)

(** manipulation of program pointers *)
module PP :
  sig
    type t
    exception NoCode of (class_name * method_signature)
    val to_className : t -> JBasics.class_name
    val to_class : t -> JProgram.class_file
    val to_hardpp : t -> JProgram.class_file * JClass.code * int
    val to_softpp :
      t -> (JBasics.class_name * JClass.method_signature) * int

    (** [get_first_pp p cn ms] gets a pointer to the first instruction of
	the method [ms] of the class [cn].

	@raise Not_found if [cn] is not a class of [p], or [ms] is not a
	method of [cn].

	@raise NoCode if the method [ms] has no associated code.*)
    val get_first_pp :
      JProgram.t -> JBasics.class_name * JClass.method_signature -> t
    val get_first_pp_wp : class_file * method_signature -> t
    val goto_absolute : t -> int -> t
    val goto_relative : t -> int -> t
  end

(** The type of program point identifier. *)
type pp = PP.t

val get_opcode : pp -> JClass.opcode
val next_instruction : PP.t -> PP.t
val normal_successors : pp -> PP.t list
val handlers : PP.t -> JBasics.jexception list
val exceptional_successors : PP.t -> PP.t list
