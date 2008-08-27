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

(** Unparsing (compilation) of either high or low level class
    representations into class files. *)

(** Provided constants are kept unchanged. Missing constant are added at
    the end of the constant pool if needed. *)

(** Unparses a class to a file. See {!JInstruction.code2opcodes} for more
    information *)
val unparse_class_low_level : 'a IO.output -> JClassLow.jclass -> unit

(** Unparses a class to a file. See {!JInstruction.code2opcodes} for more
    information *)
val unparse_class : 'a IO.output -> JClass.interface_or_class -> unit

(** Unparses an attribute to a couple [(name, content)] where [name]
    is the code-name of the attribute and [content] as encoded in a
    [.class] file. *)
val unparse_attribute_to_strings : JBasics.constant DynArray.t -> JClassLow.attribute -> string * string

(**/**)

(* For statistics: *)

val unparse_stackmap_attribute :
  JBasics.constant DynArray.t ->
  (int * JBasics.verification_type list * JBasics.verification_type list) list ->
  (string * string)

val unparse_constant_pool :
  'a IO.output ->
  JBasics.constant DynArray.t ->
  unit
