(*
 *  This file is part of JavaLib
 *  Copyright (c)2007 Université de Rennes 1
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

(** Unparsing. *)

(** Unparses a class to a file. See {!JInstruction.code2opcodes} for more
    information *)
val unparse_class : 'a IO.output -> JClass.jclass -> unit

(**/**)

(* For statistics: *)

val unparse_stackmap_attribute :
  'a IO.output ->
  JClass.constant DynArray.t ->
  (int * JClass.verification_type list * JClass.verification_type list) list ->
  unit

val unparse_constant_pool :
  'a IO.output ->
  JClass.constant DynArray.t ->
  unit
