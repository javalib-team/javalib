(*
 * This file is part of Javalib
 * Copyright (c)2004 Nicolas Cannasse
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
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

open Batteries
open JBasics

let replace_dot =
  let replace_char c = if c='.' then '/' else c in
  String.map replace_char

let class_name ?(jvm=false) cn =
  let cname = cn_name cn in
    if jvm then
      replace_dot cname
    else cname

let sprintf = Printf.sprintf

let basic_type ?(jvm=false) bt =
  match bt with
    | `Bool ->
	if jvm then "Z" else "bool"
    | `Byte ->
	if jvm then "B" else "byte"
    | `Char ->
	if jvm then "C" else "char"
    | `Double ->
	if jvm then "D" else "double"
    | `Float ->
	if jvm then "F" else "float"
    | `Int ->
	if jvm then "I" else "int"
    | `Long ->
	if jvm then "J" else "long"
    | `Short ->
	if jvm then "S" else "short"


let rec object_value_signature ?(jvm=false) ot =
  match ot with
    | TClass cn ->
	let cn = class_name ~jvm:jvm cn in
	  if jvm then "L" ^cn^";"
	  else cn
    | TArray vt ->
	if jvm then
	  "[" ^ (value_signature ~jvm:true vt)
	else (value_signature vt) ^ "[]"

and value_signature ?(jvm=false) vt =
  match vt with
    | TBasic bt -> basic_type ~jvm:jvm bt
    | TObject ot -> object_value_signature ~jvm:jvm ot

let type2shortstring = value_signature ~jvm:true

let rettype2shortstring ?(jvm=true) = function
    None ->
	if jvm then "V"
	else "void"
    | Some v -> value_signature ~jvm:jvm v

let arraytype2shortstring = function
  | `Long -> "J"
  | `Float -> "F"
  | `Double -> "D"
  | `Int -> "I"
  | `Short -> "S"
  | `Char -> "C"
  | `ByteBool -> "B"
  | `Object -> "A"

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

let jvm_array_type = function
  | `Int -> 'i'
  | `Long -> 'l'
  | `Float -> 'f'
  | `Double -> 'd'
  | `Short -> 's'
  | `Char -> 'c'
  | `ByteBool -> 'b'
  | `Object -> 'a'

let method_handle_kind = function
  | `GetField -> "getfield"
  | `GetStatic -> "getstatic"
  | `PutField -> "putfield"
  | `PutStatic -> "putstatic"
  | `InvokeVirtual -> "invokevirtual"
  | `InvokeStatic -> "invokestatic"
  | `InvokeSpecial -> "invokespecial"
  | `NewInvokeSpecial -> "newinvokespecial"
  | `InvokeInterface -> "invokeinterface"

let dump_constant_value ch = function
  | ConstString s -> BatPrintf.fprintf ch "string '%s'" (jstr_pp s)
  | ConstInt i -> BatPrintf.fprintf ch "int %ld" i
  | ConstFloat f -> BatPrintf.fprintf ch "float %f" f
  | ConstLong i -> BatPrintf.fprintf ch "long %Ld" i
  | ConstDouble f -> BatPrintf.fprintf ch "double %f" f
  | ConstClass cl -> BatPrintf.fprintf ch "class %s" (object_value_signature cl)

let rec dump_constant ch = function
  | ConstValue v -> dump_constant_value ch v
  | ConstField (cn,fs) ->
      let fn = fs_name fs
      and ft = fs_type fs
      in
        BatPrintf.fprintf ch "field : %s %s::%s" (value_signature ft) (class_name cn) fn
  | ConstMethod (cl,ms) ->
      let mn = ms_name ms
      and md = ms_args ms, ms_rtype ms
      in
        BatPrintf.fprintf ch "method : %s"
          (method_signature (object_value_signature cl ^ "::" ^ mn) md)
  | ConstInterfaceMethod (cn,ms) ->
      let mn = ms_name ms
      and md = ms_args ms, ms_rtype ms
      in
        BatPrintf.fprintf ch "interface-method : %s"
          (method_signature (class_name cn ^ "::" ^ mn) md)
  | ConstNameAndType (s,sign) -> BatPrintf.fprintf ch "name-and-type : %s" (signature s sign)
  | ConstStringUTF8 s -> BatPrintf.fprintf ch "utf8 %s" s
  | ConstUnusable -> BatPrintf.fprintf ch "unusable"
  | ConstMethodType ms ->
      BatPrintf.fprintf ch "method-type : %s"
        (method_signature "" ms)
  | ConstMethodHandle (hk, c) ->
      BatPrintf.fprintf ch "method-handle : %s" (method_handle_kind hk);
      (dump_constant ch c)
  | ConstInvokeDynamic (bmi, ms) ->
      BatPrintf.fprintf ch "invole-dynamic : %d %s"
        bmi
        (ms_name ms)


let dump_constantpool ch =
  Array.iteri
    (fun i c ->
      BatPrintf.fprintf ch "    %d  " i;
      dump_constant ch c;
      IO.write ch '\n')


let dump_verification_type = function
  | VTop -> "Top"
  | VInteger -> "Integer"
  | VFloat -> "Float"
  | VDouble -> "Double"
  | VLong -> "Long"
  | VNull -> "Null"
  | VUninitializedThis -> "UninitializedThis"
  | VObject c -> sprintf "Object %s" (object_value_signature c)
  | VUninitialized off -> sprintf "Uninitialized %d" off

let dump_stackmap ch (offset,locals,stack) =
  BatPrintf.fprintf ch "\n      offset=%d,\n      locals=[" offset;
  List.iter (fun t -> BatPrintf.fprintf ch "\n        %s" (dump_verification_type t)) locals;
  BatPrintf.fprintf ch "],\n      stack=[";
  List.iter (fun t -> BatPrintf.fprintf ch "\n        %s" (dump_verification_type t)) stack

open JCode

let dump_exc ch _cl exc =
  BatPrintf.fprintf ch "\n      [%d-%d] -> %d (" exc.e_start exc.e_end exc.e_handler;
  (match exc.e_catch_type with
     | None -> BatPrintf.fprintf ch "<finally>"
     | Some cl -> BatPrintf.fprintf ch "class %s" (class_name cl));
  BatPrintf.fprintf ch ")"
