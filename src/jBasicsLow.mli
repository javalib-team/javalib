(*
 * This file is part of Javalib
 * Copyright (c)2004 Nicolas Cannasse
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008 Laurent Hubert (CNRS)
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1, with the special exception on linking described in file
 * LICENSE.
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

(** Low level manipulations on basic types. *)

open JBasics

(** Method handle type. *)
type method_handle_kind = [
| `GetField
| `GetStatic
| `PutField
| `PutStatic
| `InvokeVirtual
| `InvokeStatic
| `InvokeSpecial
| `NewInvokeSpecial
| `InvokeInterface
]

type ldc_value = [
  | `Int of int32
  | `Float of float
  | `String of jstr
  | `Class of object_type
  | `MethodType of method_descriptor
  | `MethodHandle of method_handle
]
   
type ioc_method = [
  | `Class of object_type * method_signature
  | `Interface of class_name * method_signature
]

(** {2 Constant Pool.}  *)

(** Converting to constant pool elements: *)

val bootstrap_argument_to_const : bootstrap_argument -> constant
val method_handle_to_const : method_handle -> method_handle_kind * constant
val constant_attribute_to_const : JClass.constant_attribute -> constant

(** Getting a constant from the constant pool: *)

val get_constant : constant array -> int -> constant
val get_constant_ldc_value : constant array -> int -> ldc_value
val get_object_type : constant array -> int -> object_type
val get_class : constant array -> int -> class_name
val get_string : constant array -> int -> string
val get_field : constant array -> int ->
  class_name * field_signature
val get_method : constant array -> int ->
  object_type * method_signature
val get_interface_method : constant array -> int ->
  class_name * method_signature
val get_method_or_interface_method : constant array -> int -> ioc_method
val get_method_handle : constant array -> int -> method_handle
val get_bootstrap_argument : constant array -> int -> bootstrap_argument
val get_constant_attribute : constant array -> int -> JClass.constant_attribute

(** Same thing, reading the index in a channel: *)

val get_class_ui16 : constant array -> JLib.IO.input -> class_name
val get_string_ui16 : constant array -> JLib.IO.input -> string
val get_method_handle_ui16 : constant array -> JLib.IO.input -> method_handle
val get_bootstrap_argument_ui16 : constant array -> JLib.IO.input -> bootstrap_argument

(** Getting an index for a constant: *)

(** Return the index of a constant, adding it to the constant pool if necessary. *)
val constant_to_int : constant JLib.DynArray.t -> constant -> int
val ldc_value_to_int : constant JLib.DynArray.t -> ldc_value -> int
val object_type_to_int : constant JLib.DynArray.t -> object_type -> int
val class_to_int : constant JLib.DynArray.t -> class_name -> int
val field_to_int : constant JLib.DynArray.t ->
  class_name * field_signature -> int
val method_to_int : constant JLib.DynArray.t ->
  object_type * method_signature -> int
val interface_method_to_int : constant JLib.DynArray.t ->
  class_name * method_signature -> int
val string_to_int : constant JLib.DynArray.t -> string -> int
val method_handle_kind_to_int : method_handle_kind -> int
val name_and_type_to_int : constant JLib.DynArray.t -> string * descriptor -> int

(** Return the index of a bootstrap method in the bootstrap method table, adding it if necessary. *)
val bootstrap_method_to_int : bootstrap_method JLib.DynArray.t -> bootstrap_method -> int

(** Same thing, but writes the index to a channel. *)

val write_constant :
  'a JLib.IO.output -> constant JLib.DynArray.t -> constant -> unit
val write_object_type :
  'a JLib.IO.output -> constant JLib.DynArray.t -> object_type -> unit
val write_class :
  'a JLib.IO.output -> constant JLib.DynArray.t -> class_name -> unit
val write_string :
  'a JLib.IO.output -> constant JLib.DynArray.t -> string -> unit
val write_name_and_type :
  'a JLib.IO.output -> constant JLib.DynArray.t -> string * descriptor -> unit
val write_bootstrap_argument :
  'a JLib.IO.output -> constant JLib.DynArray.t -> bootstrap_argument -> unit
val write_constant_attribute :
  'a JLib.IO.output -> constant JLib.DynArray.t -> JClass.constant_attribute -> unit

(** {2 Usefull writing functions. } *)

(** @raise Overflow if the integer does not belong to [0x0;0xFF].  *)
val write_ui8 : 'a JLib.IO.output -> int -> unit

(** @raise Overflow if the integer does not belong to [-0x80;0x7F].  *)
val write_i8 : 'a JLib.IO.output -> int -> unit

val write_string_with_length :
  ('a JLib.IO.output -> int -> unit) -> 'a JLib.IO.output -> string -> unit
val write_with_length :
  ('a JLib.IO.output -> int -> unit) ->
  'a JLib.IO.output -> (string JLib.IO.output -> unit) -> unit
val write_with_size :
  ('a JLib.IO.output -> int -> unit) -> 'a JLib.IO.output -> ('c -> unit) -> 'c list -> unit
