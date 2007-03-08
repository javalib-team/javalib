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

let get_class consts ch =
	match get_constant consts (read_ui16 ch) with
	| ConstClass n -> n
	| _ -> error "Invalid class index"
	
let get_field consts ch =
	match get_constant consts (read_ui16 ch) with
	| ConstField (c, f, s) -> c, f, s
	| _ -> error "Invalid field index"
	
let get_method consts ch =
	match get_constant consts (read_ui16 ch) with
	| ConstMethod (c, m, s) -> c, m, s
	| _ -> error "Invalid method index"
	
let get_interface_method consts ch =
	match get_constant consts (read_ui16 ch) with
	| ConstInterfaceMethod (c, m, s) -> c, m, s
	| _ -> error "Invalid interface method index"
	
let get_string consts ch =
  let i = read_ui16 ch in
	match get_constant consts i with
	| ConstStringUTF8 s -> s
	| c ->
	    let s = IO.output_string () in
	      JDump.dump_constant s c;
	      error ("Invalid string " ^ IO.close_out s ^ " at index " ^ string_of_int i)

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
	     | ConstLong _ | ConstDouble _ ->
		 DynArray.add cp ConstUnusable
	     | _ -> ());
	  i
