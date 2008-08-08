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

open JBasics

let class_name = String.concat "."

let sprintf = Printf.sprintf

let basic_type = function
  | `Bool -> "bool"
  | `Char -> "char"
  | `Float -> "float"
  | `Double -> "double"
  | `Byte -> "byte"
  | `Short -> "short"
  | `Int -> "int"
  | `Long -> "long"

let rec object_value_signature = function
	| TClass cl -> class_name cl
	| TArray s -> value_signature s ^"[]"

and value_signature = function
  | TBasic b -> basic_type b
  | TObject o -> object_value_signature o


let type2shortstring t = 
  let bt2ss = function
    | `Long -> "J"
    | `Float -> "F"
    | `Double -> "D"
    | `Int -> "I"
    | `Short -> "S"
    | `Char -> "C"
    | `Byte -> "B"
    | `Bool -> "Z"
  in
  let rec ot2ss = function
    | TClass cn -> "L"^class_name cn^";"
    | TArray t -> "["^ vt2ss t
  and vt2ss = function
    | TBasic t -> bt2ss t
    | TObject t -> ot2ss t
  in vt2ss t

let rettype2shortstring = function
  | None -> "V"
  | Some t -> type2shortstring t

let method_signature name (sl,sr) =
		(match sr with
		| None -> "void"
		| Some s -> value_signature s) ^ " " ^name^ "(" ^ String.concat "," (List.map value_signature sl) ^ ")"

let signature name = function
  | SValue v -> value_signature v ^ " " ^name
  | SMethod m -> method_signature name m

let jvm_basic_type = function
	| `Int
	| `Int2Bool -> 'i'
	| `Long -> 'l'
	| `Float -> 'f'
	| `Double -> 'd'

let java_basic_type = function
  | `Int -> 'i'
  | `Long -> 'l'
  | `Float -> 'f'
  | `Double -> 'd'
  | `Short -> 's'
  | `Char -> 'c'
  | `Byte
  | `Bool -> 'b'

let dump_constant_value ch = function
  | ConstString s -> IO.printf ch "string '%s'" s
  | ConstInt i -> IO.printf ch "int %ld" i
  | ConstFloat f -> IO.printf ch "float %f" f
  | ConstLong i -> IO.printf ch "long %Ld" i
  | ConstDouble f -> IO.printf ch "double %f" f
  | ConstClass cl -> IO.printf ch "class %s" (object_value_signature cl)

let dump_constant ch = function
  | ConstValue v -> dump_constant_value ch v
  | ConstField (cl,f,sign) -> IO.printf ch "field : %s %s::%s" (value_signature sign) (class_name cl) f
  | ConstMethod (cl,f,sign) -> IO.printf ch "method : %s"(method_signature (object_value_signature cl ^ "::" ^ f) sign)
  | ConstInterfaceMethod (cl,f,sign) -> IO.printf ch "interface-method : %s"(method_signature (class_name cl ^ "::" ^ f) sign)
  | ConstNameAndType (s,sign) -> IO.printf ch "name-and-type : %s" (signature s sign)
  | ConstStringUTF8 s -> IO.printf ch "utf8 %s" s
  | ConstUnusable -> IO.printf ch "unusable"

let dump_constantpool ch =
  Array.iteri
    (fun i c ->
      IO.printf ch "    %d  " i;
      dump_constant ch c;
      IO.write ch '\n')


let dump_stackmap ch (offset,locals,stack) =
	let dump_verif_info = function
		| VTop -> "Top"
		| VInteger -> "Integer"
		| VFloat -> "Float"
		| VDouble -> "Double"
		| VLong -> "Long"
		| VNull -> "Null"
		| VUninitializedThis -> "UninitializedThis"
		| VObject c -> sprintf "Object %s" (object_value_signature c)
		| VUninitialized off -> sprintf "Uninitialized %d" off
	in
	IO.printf ch "\n      offset=%d,\n      locals=[" offset;
	List.iter (fun t -> IO.printf ch "\n        %s" (dump_verif_info t)) locals;
	IO.printf ch "],\n      stack=[";
	List.iter (fun t -> IO.printf ch "\n        %s" (dump_verif_info t)) stack

let dump_exc ch _cl exc =
  IO.printf ch "\n      [%d-%d] -> %d (" exc.e_start exc.e_end exc.e_handler;
  (match exc.e_catch_type with
     | None -> IO.printf ch "<finally>"
     | Some cl -> IO.printf ch "class %s" (class_name cl));
  IO.printf ch ")"


