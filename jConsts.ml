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

open JClass
open IO
open IO.BigEndian
open ExtList
open ExtString

type error_msg =
	| Invalid_data
	| Invalid_constant of int
	| Invalid_access_flags of int
	| Custom of string

exception Error of string

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
    | _ -> failwith "array type descriptor not allowed here"

let get_field consts i =
	match get_constant consts i with
	| ConstField (c, f, s) -> c, f, s
	| _ -> error "Invalid field index"
	
let get_method consts i =
	match get_constant consts i with
	| ConstMethod (c, m, s) -> c, m, s
	| _ -> error "Invalid method index"
	
let get_interface_method consts i =
	match get_constant consts i with
	| ConstInterfaceMethod (c, m, s) -> c, m, s
	| _ -> error "Invalid interface method index"
	
let get_string' consts i =
	match get_constant consts i with
	| ConstStringUTF8 s -> s
	| c ->
	    let s = IO.output_string () in
	      JDump.dump_constant s c;
	      error ("Invalid string " ^ IO.close_out s ^ " at index " ^ string_of_int i)
let get_string consts ch = get_string' consts (read_ui16 ch)

let constant_to_int cp c =
  if c = ConstUnusable
  then invalid_arg "constant_to_int";
  try
    DynArray.index_of (( = ) c) cp
  with
      Not_found ->
	if DynArray.length cp = 0
	then DynArray.add cp ConstUnusable;
	assert (DynArray.get cp 0 = ConstUnusable);
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
