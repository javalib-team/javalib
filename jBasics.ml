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

open IO
open IO.BigEndian
open ExtList
open ExtString

(* {2 Basic Elements.} *)

(* Fully qualified ordinary class or interface name (not an array).
    For example: [\["java" ; "lang" ; "Object"\]]. *)
type class_name = string list

let java_lang_object = ["java";"lang";"Object"]

type version = {major :int; minor:int;}

(* Numerical types that are not smaller than int. *)
type other_num = [
| `Long
| `Float
| `Double
]

(* JVM basic type (int = short = char = byte = bool). *)
type jvm_basic_type = [
| `Int2Bool
| other_num
]

(* JVM type (int = short = char = byte = bool, all objects have the same type). *)
type jvm_type = [
| jvm_basic_type
| `Object
]

(* JVM array element type (byte = bool, all objects have the same type). *)
type jvm_array_type = [
| `Int
| `Short
| `Char
| `ByteBool
| other_num
| `Object
]

(* JVM return type (byte = bool, all objects have the same type). *)
type jvm_return_type = [
|  jvm_basic_type
| `Object
| `Void
]

(* Java basic type. *)
type java_basic_type = [
| `Int
| `Short
| `Char
| `Byte
| `Bool
| other_num
]

(* Java object type *)
type object_type =
  | TClass of class_name
  | TArray of value_type

(* Java type *)
and value_type =
  | TBasic of java_basic_type
  | TObject of object_type

(* Field descriptor. *)
type field_descriptor = value_type

(* Method descriptor. *)
type method_descriptor = value_type list * value_type option

(* Signatures parsed from CONSTANT_NameAndType_info structures. *)
type descriptor =
  | SValue of field_descriptor
  | SMethod of method_descriptor

(* Constant value. *)
type constant_value =
  | ConstString of string
  | ConstInt of int32
  | ConstFloat of float
  | ConstLong of int64
  | ConstDouble of float
  | ConstClass of object_type

(* Constant. *)
type constant =
  | ConstValue of constant_value
  | ConstField of (class_name * string * field_descriptor)
  | ConstMethod of (object_type * string * method_descriptor)
  | ConstInterfaceMethod of (class_name * string * method_descriptor)
  | ConstNameAndType of string * descriptor
  | ConstStringUTF8 of string
  | ConstUnusable

(* Exception handler. *)
type exception_handler = {
	e_start : int;
	e_end : int;
	e_handler : int;
	e_catch_type : class_name option
}

(* Stackmap type. *)
type verification_type =
	| VTop
	| VInteger
	| VFloat
	| VDouble
	| VLong
	| VNull
	| VUninitializedThis
	| VObject of object_type
	| VUninitialized of int (* creation point *)

exception No_class_found of string

exception Class_structure_error of string

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

let write_string_with_length length ch s =
  length ch (String.length s);
  nwrite ch s

let write_with_length length ch write =
  let ch' = output_string () in
    write ch';
    write_string_with_length length ch (close_out ch')

let write_with_size (size:'a IO.output -> int -> unit) ch write l =
  size ch (List.length l);
  List.iter write l

(* Constant pool *)
(*****************)

let get_constant c n =
	if n < 0 || n >= Array.length c then raise (Class_structure_error ("Illegal constant index:" ^ string_of_int n));
	match c.(n) with
	| ConstUnusable -> raise (Class_structure_error ("Illegal constant: unusable"))
	| x -> x

let get_constant_value c n =
  match get_constant c n with
    | ConstValue v -> v
    | _ -> raise (Class_structure_error ("Illegal constant value index (does not refer to constant value)"))

let get_object_type consts i =
	match get_constant consts i with
	  | ConstValue (ConstClass n) -> n
	  | _ -> raise (Class_structure_error ("Illegal class index (does not refer to a constant class)"))

let get_class consts i =
  match get_object_type consts i with
    | TClass c -> c
    | _ -> raise (Class_structure_error ("Illegal class index: refers to an array type descriptor"))

let get_field consts i =
  match get_constant consts i with
    | ConstField (c, f, s) -> c, f, s
    | _ -> raise (Class_structure_error ("Illegal field index (does not refer to a constant field)"))

let get_method consts i =
	match get_constant consts i with
	| ConstMethod (c, m, s) -> c, m, s
	| _ -> raise (Class_structure_error ("Illegal method index (does not refer to a constant method)"))

let get_interface_method consts i =
  match get_constant consts i with
    | ConstInterfaceMethod (c, m, s) -> c, m, s
    | _ -> raise (Class_structure_error ("Illegal interface method index (does not refer to a constant interface method)"))

let get_string consts i =
  match get_constant consts i with
    | ConstStringUTF8 s -> s
    | _ -> raise (Class_structure_error ("Illegal string index (does not refer to a constant string)"))

let get_class_ui16 consts ch = get_class consts (read_ui16 ch)
let get_string_ui16 consts ch = get_string consts (read_ui16 ch)

let constant_to_int cp c =
  if c = ConstUnusable
  then raise (Class_structure_error ("Illegal constant: unusable"));
  try
    DynArray.index_of (fun c' -> 0 = compare c c') cp (* [nan <> nan], where as [0 = compare nan nan] *)
  with
      Not_found ->
	if DynArray.length cp = 0
	then DynArray.add cp ConstUnusable;
	if not (DynArray.get cp 0 = ConstUnusable)
	then raise (Class_structure_error "unparsing with an incorrect constant pool");
	let i = DynArray.length cp in
	  DynArray.add cp c;
	  (match c with
	     | ConstValue (ConstLong _ | ConstDouble _) ->
		 DynArray.add cp ConstUnusable
	     | _ -> ());
	  i

let value_to_int cp v = constant_to_int cp (ConstValue v)
let object_type_to_int cp ot = value_to_int cp (ConstClass ot)
let field_to_int cp v = constant_to_int cp (ConstField v)
let method_to_int cp v = constant_to_int cp (ConstMethod v)
let class_to_int cp v = object_type_to_int cp (TClass v)
let string_to_int cp v = constant_to_int cp (ConstStringUTF8 v)
let name_and_type_to_int cp (n, s) = constant_to_int cp (ConstNameAndType (n, s))

let write_constant ch cp c = write_ui16 ch (constant_to_int cp c)
let write_value ch cp c = write_ui16 ch (value_to_int cp c)
let write_object_type ch cp c = write_ui16 ch (object_type_to_int cp c)
let write_class ch cp c = write_ui16 ch (class_to_int cp c)
let write_string ch cp c = write_ui16 ch (string_to_int cp c)
let write_name_and_type ch cp c = write_ui16 ch (name_and_type_to_int cp c)
