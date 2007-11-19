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

open IO
open IO.BigEndian
open ExtList
open ExtString

(** {2 Basic Elements.} *)

(** Fully qualified ordinary class or interface name (not an array).
    For example: [\["java" ; "lang" ; "Object"\]]. *)
type class_name = string list

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

(** Field signature. *)
type field_descriptor = value_type

(** Method signature. *)
type method_descriptor = value_type list * value_type option

(** Signatures parsed from CONSTANT_NameAndType_info structures. *)
type signature =
  | SValue of field_descriptor
  | SMethod of method_descriptor

(** Constant value. *)
type constant_value =
  | ConstString of string
  | ConstInt of int32
  | ConstFloat of float
  | ConstLong of int64
  | ConstDouble of float
  | ConstClass of object_type (** This is not documented in the JVM spec. *)

(** Constant. *)
type constant =
  | ConstValue of constant_value
  | ConstField of (class_name * string * field_descriptor)
  | ConstMethod of (object_type * string * method_descriptor)
  | ConstInterfaceMethod of (class_name * string * method_descriptor)
  | ConstNameAndType of string * signature
  | ConstStringUTF8 of string
  | ConstUnusable

(** Exception handler. *)
type jexception = {
	e_start : int;
	e_end : int;
	e_handler : int;
	e_catch_type : class_name option
}

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

type error_msg =
	| Invalid_data
	| Invalid_constant of int
	| Invalid_access_flags of int
	| Custom of string

exception Error of string

exception Illegal_value of string * string

exception Invalid_opcode of int

exception No_class_found of string

exception Invalid_class of string

exception Parse_error of string

exception Class_structure_error of string

exception Bad_allignement_of_low_level_bytecode

exception IncorrectConstantPool

let error msg = raise (Error msg)

let get_constant c n =
	if n < 0 || n >= Array.length c then error ("Invalid constant index " ^ string_of_int n);
	match c.(n) with
	| ConstUnusable -> error ("Unusable constant index " ^ string_of_int n);
	| x -> x

let get_constant_value c n =
  match get_constant c n with
    | ConstValue v -> v
    | _ -> error "Invalie constant value index"

let get_object_type consts i =
	match get_constant consts i with
	  | ConstValue (ConstClass n) -> n
	  | _ -> error "Invalid class index"

let get_class consts i =
  match get_object_type consts i with
    | TClass c -> c
    | _ -> raise (Illegal_value ("array type descriptor", "class index"))

let get_field consts i =
  match get_constant consts i with
    | ConstField (c, f, s) -> c, f, s
    | _ -> raise (Illegal_value ("", "field index"))

let get_method consts i =
	match get_constant consts i with
	| ConstMethod (c, m, s) -> c, m, s
	| _ -> raise (Illegal_value ("", "method index"))

let get_interface_method consts i =
  match get_constant consts i with
    | ConstInterfaceMethod (c, m, s) -> c, m, s
    | _ -> raise (Illegal_value ("", "interface method index"))

let get_string' consts i =
  match get_constant consts i with
    | ConstStringUTF8 s -> s
    | c -> raise (Illegal_value (string_of_int i, "string index"))

let get_string consts ch = get_string' consts (read_ui16 ch)

let constant_to_int cp c =
  if c = ConstUnusable
  then invalid_arg "constant_to_int";
  try
    DynArray.index_of (fun c' -> 0 = compare c c') cp (* [nan <> nan], where as [0 = compare nan nan] *)
  with
      Not_found ->
	if DynArray.length cp = 0
	then DynArray.add cp ConstUnusable;
	if not (DynArray.get cp 0 = ConstUnusable)
	then raise IncorrectConstantPool;
	let i = DynArray.length cp in
	  DynArray.add cp c;
	  (match c with
	     | ConstValue (ConstLong _ | ConstDouble _) ->
		 DynArray.add cp ConstUnusable
	     | _ -> ());
	  i

(* Usefull functions *)
(*********************)

(* write_byte doesn't check anything *)
let write_ui8 ch n =
  if n < 0 || n > 0xFF then raise (Overflow "write_ui8");
  write_byte ch n

let write_i8 ch n =
  if n < -0x80 || n > 0x7F then raise (Overflow "write_i8");
  if n < 0 then
    write_ui8 ch (0x100 + n)
  else
    write_ui8 ch n

let write_constant ch cp c =
  write_ui16 ch (constant_to_int cp c)

let write_string_with_length length ch s =
  length ch (String.length s);
  nwrite ch s

let write_with_length length ch write =
  let ch' = output_string () in
    write ch';
    write_string_with_length length ch (close_out ch')

let write_with_size size ch write l =
  size ch (List.length l);
  List.iter write l
