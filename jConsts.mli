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

(** Access to the constant pool. *)

(** You should not need this module for normal classfiles, as the
    parsing/unparsing functions take care of the constant pool. *)

(** Low level access to the constant pool. This is usefull for adding
    a user-defined attribute that refers to the constant pool. *)
val constant_to_int : JClassLow.constant DynArray.t -> JClassLow.constant -> int

(**/**)

type error_msg =
    Invalid_data
  | Invalid_constant of int
  | Invalid_access_flags of int
  | Custom of string

exception Error of string

val error : string -> 'a

val get_constant : JClassLow.constant array -> int -> JClassLow.constant

val get_constant_value : JClassLow.constant array -> int -> JClassLow.constant_value

val get_object_type : JClassLow.constant array -> int -> JClassLow.object_type
val get_class : JClassLow.constant array -> int -> JClassLow.class_name

val get_string : JClassLow.constant array -> IO.input -> string
val get_string' : JClassLow.constant array -> int -> string

val get_field : JClassLow.constant array -> int ->
  JClassLow.class_name * string * JClassLow.field_descriptor

val get_method : JClassLow.constant array -> int ->
  JClassLow.object_type * string * JClassLow.method_descriptor

val get_interface_method : JClassLow.constant array -> int ->
  JClassLow.class_name * string * JClassLow.method_descriptor

(* This should go somewhere else. *)

val write_ui8 : 'a IO.output -> int -> unit
val write_i8 : 'a IO.output -> int -> unit
val write_constant :
  'a IO.output -> JClassLow.constant DynArray.t -> JClassLow.constant -> unit
val write_string_with_length :
  ('a IO.output -> int -> 'b) -> 'a IO.output -> string -> unit
val write_with_length :
  ('a IO.output -> int -> 'b) ->
  'a IO.output -> (string IO.output -> 'c) -> unit
val write_with_size :
  ('a -> int -> 'b) -> 'a -> ('c -> unit) -> 'c list -> unit
