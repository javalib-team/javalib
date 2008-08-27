(*
 *  This file is part of JavaLib
 *  Copyright (c)2004 Nicolas Cannasse
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

(** Basic elements of class files. *)

(** {2 Types and descriptors.} *)

(** Fully qualified ordinary class or interface name (not an array).
    For example: [\["java" ; "lang" ; "Object"\]]. *)
type class_name = string list

(** [java.lang.Object] *)
val java_lang_object: class_name

(** Verison number of the class file. Extract of the specification: An
    implementation of Java 1.k sould support class file formats of
    versions in the range of 45.0 through 44+k.0 inclusive. E.g. Java
    1.6 implementations support class file formats of versions up to
    50.0. *)
type version = {major :int; minor:int;}

(** Numerical types that are not smaller than int. *)
type other_num = [
| `Long
| `Float
| `Double
]

(** JVM basic type (int = short = char = byte = bool). *)
type jvm_basic_type = [
| `Int2Bool
| other_num
]

(** JVM type (int = short = char = byte = bool, all objects have the same type). *)
type jvm_type = [
| jvm_basic_type
| `Object
]

(** JVM array element type (byte = bool, all objects have the same type). *)
type jvm_array_type = [
| `Int
| `Short
| `Char
| `ByteBool
| other_num
| `Object
]

(** JVM return type (byte = bool, all objects have the same type). *)
type jvm_return_type = [
|  jvm_basic_type
| `Object
| `Void
]

(** Java basic type. *)
type java_basic_type = [
| `Int
| `Short
| `Char
| `Byte
| `Bool
| other_num
]

(** Java object type *)
type object_type =
  | TClass of class_name
  | TArray of value_type

(** Java type *)
and value_type =
  | TBasic of java_basic_type
  | TObject of object_type

(** Field descriptor. *)
type field_descriptor = value_type

(** Method descriptor. *)
type method_descriptor = value_type list * value_type option

(** Signatures parsed from CONSTANT_NameAndType_info structures. *)
type descriptor =
  | SValue of field_descriptor
  | SMethod of method_descriptor

(** {2 Exception handlers.} *)

(** Exception handler. *)
type exception_handler = {
	e_start : int;
	e_end : int;
	e_handler : int;
	e_catch_type : class_name option
}

(** {2 Constant pool.} *)

(** You should not need this for normal usage, as the parsing/unparsing functions
    take care of the constant pool. This is typically usefull for user-defined
    attributes that refer to the constant pool. *)

(** Constant value. *)
type constant_value =
  | ConstString of string
  | ConstInt of int32
  | ConstFloat of float
  | ConstLong of int64
  | ConstDouble of float
  | ConstClass of object_type

(** Constant. *)
type constant =
  | ConstValue of constant_value
  | ConstField of (class_name * string * field_descriptor)
  | ConstMethod of (object_type * string * method_descriptor)
  | ConstInterfaceMethod of (class_name * string * method_descriptor)
  | ConstNameAndType of string * descriptor
  | ConstStringUTF8 of string
  | ConstUnusable

(** Getting a constant from the constant pool: *)

val get_constant : constant array -> int -> constant
val get_constant_value : constant array -> int -> constant_value
val get_object_type : constant array -> int -> object_type
val get_class : constant array -> int -> class_name
val get_string : constant array -> int -> string
val get_field : constant array -> int ->
  class_name * string * field_descriptor
val get_method : constant array -> int ->
  object_type * string * method_descriptor
val get_interface_method : constant array -> int ->
  class_name * string * method_descriptor

(** Same thing, reading the index in a channel: *)

val get_class_ui16 : constant array -> IO.input -> class_name
val get_string_ui16 : constant array -> IO.input -> string

(** Getting an index for a constant: *)

(** Return the index of a constant, adding it to the constant pool if necessary. *)
val constant_to_int : constant DynArray.t -> constant -> int
val value_to_int : constant DynArray.t -> constant_value -> int
val object_type_to_int : constant DynArray.t -> object_type -> int
val class_to_int : constant DynArray.t -> class_name -> int
val field_to_int : constant DynArray.t ->
  class_name * string * field_descriptor -> int
val method_to_int : constant DynArray.t ->
  object_type * string * method_descriptor -> int
val string_to_int : constant DynArray.t -> string -> int

(** Same thing, bu writes the index to a channel. *)

val write_constant :
  'a IO.output -> constant DynArray.t -> constant -> unit
val write_value :
  'a IO.output -> constant DynArray.t -> constant_value -> unit
val write_object_type :
  'a IO.output -> constant DynArray.t -> object_type -> unit
val write_class :
  'a IO.output -> constant DynArray.t -> class_name -> unit
val write_string :
  'a IO.output -> constant DynArray.t -> string -> unit
val write_name_and_type :
  'a IO.output -> constant DynArray.t -> string * descriptor -> unit

(** {2 Stackmaps}  *)

(** Stackmap type. *)
type verification_type =
  | VTop
  | VInteger
  | VFloat
  | VDouble
  | VLong
  | VNull
  | VUninitializedThis
  | VObject of object_type
  | VUninitialized of int (** creation point *)

(** {2 Errors}  *)

(** The library may throw the following exceptions, in addition to [Invalid_argument],
    and other exceptions defined in {! JProgram} and {! JControlFlow}.
    Any other exception (in particular, an [Assert_failure])
    should be interpreted as a bug in [javalib]. *)

(** Indicates that a class name could not be found in a given classpath. *)
exception No_class_found of string

(** Indicates the argument of a parsing/unparsing function does not
    satisfy a structural constraint of class files. *)
exception Class_structure_error of string


(**/**)

(** {2 Usefull writing functions. } *)

val write_ui8 : 'a IO.output -> int -> unit
val write_i8 : 'a IO.output -> int -> unit
val write_string_with_length :
  ('a IO.output -> int -> unit) -> 'a IO.output -> string -> unit
val write_with_length :
  ('a IO.output -> int -> unit) ->
  'a IO.output -> (string IO.output -> unit) -> unit
val write_with_size :
  ('a IO.output -> int -> unit) -> 'a IO.output -> ('c -> unit) -> 'c list -> unit
