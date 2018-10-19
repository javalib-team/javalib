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

open JLib
open IO
open IO.BigEndian
open JBasics

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
  nwrite_string ch s

let write_with_length length ch write =
  let ch' = output_string () in
    write ch';
    write_string_with_length length ch (close_out ch')

let write_with_size (size:'a output -> int -> unit) ch write l =
  size ch (List.length l);
  List.iter write l

(* Constant pool *)
(*****************)

let get_constant c n =
  if n < 0 || n >= Array.length c then raise (Class_structure_error ("Illegal constant index:" ^ string_of_int n));
  match c.(n) with
    | ConstUnusable -> raise (Class_structure_error ("Illegal constant: unusable"))
    | x -> x

(* for ldc and ldc_w since Java 7 *)
let get_constant_ldc_value c n =
  match get_constant c n with
  | ConstInt i -> `Int i
  | ConstFloat f -> `Float f
  | ConstString s -> `String s
  | ConstClass cl -> `Class cl
  | ConstMethodType mt -> `MethodType mt
  | ConstMethodHandle mh -> `MethodHandle mh
  | _ -> raise (Class_structure_error ("Illegal constant value index (does not refer to constant value or a method type or a method handle)"))
  
let get_object_type consts i =
  match get_constant consts i with
    | ConstClass n -> n
    | _ -> raise (Class_structure_error ("Illegal class index (does not refer to a constant class)"))

let get_class consts i =
  match get_object_type consts i with
    | TClass c -> c
    | _ -> raise (Class_structure_error ("Illegal class index: refers to an array type descriptor"))

let get_field consts i =
  match get_constant consts i with
    | ConstField cnfs -> cnfs
    | _ -> raise (Class_structure_error ("Illegal field index (does not refer to a constant field)"))

let get_method consts i =
  match get_constant consts i with
    | ConstMethod cms -> cms
    | _ -> raise (Class_structure_error ("Illegal method index (does not refer to a constant method)"))

let get_interface_method consts i =
  match get_constant consts i with
    | ConstInterfaceMethod cms -> cms
    | _ -> raise (Class_structure_error ("Illegal interface method index (does not refer to a constant interface method)"))

let get_method_or_interface_method consts i =
  match get_constant consts i with
  | ConstMethod cms -> `Class cms
  | ConstInterfaceMethod cms -> `Interface cms
  | _ -> raise (Class_structure_error ("Illegal class or interface method index (does not refer to a constant class or interface method)"))
                                         
let get_method_handle consts i =
  match get_constant consts i with
  | ConstMethodHandle mh -> mh
  | _ -> raise (Class_structure_error ("Illegal method handle index"))

let get_bootstrap_argument consts i =
  match get_constant consts i with
  | ConstString s -> `String s
  | ConstClass cl -> `Class cl
  | ConstInt i -> `Int i
  | ConstLong i -> `Long i
  | ConstFloat f -> `Float f
  | ConstDouble f -> `Double f
  | ConstMethodType mt -> `MethodType mt
  | ConstMethodHandle mh -> `MethodHandle mh
  | _ -> raise (Class_structure_error ("Illegal bootstrap argument"))

let get_constant_attribute consts i =
  match get_constant consts i with
  | ConstLong i -> `Long i
  | ConstFloat f -> `Float f
  | ConstDouble f -> `Double f
  | ConstInt i -> `Int i
  | ConstString s -> `String s
  | _ -> raise (Class_structure_error ("Illegal constant attribute"))

let bootstrap_argument_to_const arg =
  match arg with
  | `String s ->  ConstString s
  | `Class cl ->  ConstClass cl
  | `Int i -> ConstInt i
  | `Long i -> ConstLong i
  | `Float f -> ConstFloat f
  | `Double f -> ConstDouble f
  | `MethodHandle mh -> ConstMethodHandle mh
  | `MethodType ms -> ConstMethodType ms

let method_handle_to_const mh =
  match mh with
  | `GetField f -> (`GetField, ConstField f)
  | `GetStatic f -> (`GetStatic, ConstField f)
  | `PutField f -> (`PutField, ConstField f)
  | `PutStatic f -> (`PutStatic, ConstField f)
  | `InvokeVirtual v -> (`InvokeVirtual, ConstMethod v)
  | `NewInvokeSpecial v -> (`NewInvokeSpecial, ConstMethod v)
  | `InvokeStatic (`Method v) -> (`InvokeStatic, ConstMethod v)
  | `InvokeStatic (`InterfaceMethod v) -> (`InvokeStatic, ConstInterfaceMethod v)
  | `InvokeSpecial (`Method v) -> (`InvokeSpecial, ConstMethod v)
  | `InvokeSpecial (`InterfaceMethod v) -> (`InvokeSpecial, ConstInterfaceMethod v)
  | `InvokeInterface v -> (`InvokeInterface, ConstInterfaceMethod v)

let constant_attribute_to_const attr =
  match attr with
  | `Long i -> ConstLong i
  | `Float f -> ConstFloat f
  | `Double f -> ConstDouble f
  | `Int i -> ConstInt i
  | `String s -> ConstString s

let get_string consts i =
  match get_constant consts i with
    | ConstStringUTF8 s -> s
    | _ -> raise (Class_structure_error ("Illegal string index (does not refer to a constant string)"))

let get_class_ui16 consts ch = get_class consts (read_ui16 ch)
let get_string_ui16 consts ch = get_string consts (read_ui16 ch)
let get_method_handle_ui16 consts ch = get_method_handle consts (read_ui16 ch)
let get_bootstrap_argument_ui16 consts ch = get_bootstrap_argument consts (read_ui16 ch)

let constant_to_int cp c =
  if c = ConstUnusable
  then raise (Class_structure_error ("Illegal constant: unusable"));
  try
    DynArray.index_of (fun c' -> 0 = compare c c') cp (* [nan <> nan], where as [0 = compare nan nan] *)
  with
      Not_found ->
	if DynArray.length cp = 0
	then DynArray.add cp ConstUnusable;
	if not (DynArray.unsafe_get cp 0 = ConstUnusable)
	then raise (Class_structure_error "unparsing with an incorrect constant pool");
	let i = DynArray.length cp in
	  DynArray.add cp c;
	  (match c with
	     | ConstLong _ | ConstDouble _ ->
		 DynArray.add cp ConstUnusable
	     | _ -> ());
	  i

let bootstrap_method_to_int bm_table bm =
  try
    DynArray.index_of (fun bm' -> 0 = compare bm bm') bm_table
  with
    Not_found ->
    let i = DynArray.length bm_table in
    DynArray.add bm_table bm;
    i

let method_handle_kind_to_int = function
  | `GetField -> 1
  | `GetStatic -> 2
  | `PutField ->3
  | `PutStatic -> 4
  | `InvokeVirtual -> 5
  | `InvokeStatic -> 6
  | `InvokeSpecial -> 7
  | `NewInvokeSpecial -> 8
  | `InvokeInterface -> 9

let ldc_value_to_int cp = function
  | `Int i -> constant_to_int cp (ConstInt i)
  | `Float f -> constant_to_int cp (ConstFloat f)
  | `String s -> constant_to_int cp (ConstString s)
  | `Class cl -> constant_to_int cp (ConstClass cl)
  | `MethodType mt -> constant_to_int cp (ConstMethodType mt)
  | `MethodHandle mh -> constant_to_int cp (ConstMethodHandle mh)

let object_type_to_int cp ot = constant_to_int cp (ConstClass ot)
let field_to_int cp v = constant_to_int cp (ConstField v)
let method_to_int cp v = constant_to_int cp (ConstMethod v)
let interface_method_to_int cp v = constant_to_int cp (ConstInterfaceMethod v)
let class_to_int cp v = object_type_to_int cp (TClass v)
let string_to_int cp v = constant_to_int cp (ConstStringUTF8 v)
let name_and_type_to_int cp (n, s) = constant_to_int cp (ConstNameAndType (n, s))
let bootstrap_argument_to_int cp a = constant_to_int cp (bootstrap_argument_to_const a)

let write_constant ch cp c = write_ui16 ch (constant_to_int cp c)
let write_object_type ch cp c = write_ui16 ch (object_type_to_int cp c)
let write_class ch cp c = write_ui16 ch (class_to_int cp c)
let write_string ch cp c = write_ui16 ch (string_to_int cp c)
let write_name_and_type ch cp c = write_ui16 ch (name_and_type_to_int cp c)
let write_bootstrap_argument ch cp a = write_ui16 ch (bootstrap_argument_to_int cp a)
let write_constant_attribute ch cp a =
  write_ui16 ch (constant_to_int cp (constant_attribute_to_const a))
