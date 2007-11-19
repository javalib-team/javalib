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

open JClassLow
open JBasics
open IO
open IO.BigEndian
open JCode

(* Classfile version *)
let version_major = 49
and version_minor = 0

(* Signature and classname encoding *)
(************************************)

let encode_class_name = function
  | t :: q ->
      List.fold_left
	(fun s x -> s ^ "/" ^ x)
	t
	q
  | [] -> invalid_arg "encode_class_name"

let unparse_basic_type = function
  | `Byte -> "B"
  | `Char -> "C"
  | `Double -> "D"
  | `Float -> "F"
  | `Int -> "I"
  | `Long -> "J"
  | `Short -> "S"
  | `Bool -> "Z"

let rec unparse_object_type = function
  | TClass c ->
      "L" ^ encode_class_name c ^ ";"
  | TArray s ->
      "[" ^ unparse_value_signature s

and unparse_value_signature = function
  | TBasic b -> unparse_basic_type b
  | TObject o -> unparse_object_type o

let rec unparse_method_signature (sigs, s) =
      List.fold_left
	(fun desc s ->
	   desc ^ unparse_value_signature s)
	"("
	sigs
      ^ ")"
      ^ (match s with
	   | Some s -> unparse_value_signature s
	   | None -> "V")

let rec unparse_signature = function
  | SValue v -> unparse_value_signature v
  | SMethod m -> unparse_method_signature m

(* Unparse an type that must be an object.Therefore, there is no
   L ; around the classname (if this is a class). *)
let unparse_objectType = function
  | TClass c ->
      encode_class_name c
  | TArray _ as s -> unparse_object_type s

(* Constant pool unparsing *)
(***************************)

let unparse_constant_value ch consts = function
  | ConstString s ->
      write_ui8 ch 8;
      write_constant ch consts (ConstStringUTF8 s)
  | ConstInt i ->
      write_ui8 ch 3;
      write_real_i32 ch i
  | ConstFloat f ->
      write_ui8 ch 4;
      write_real_i32 ch (Int32.bits_of_float f)
  | ConstLong l ->
      write_ui8 ch 5;
      write_i64 ch l
  | ConstDouble d ->
      write_ui8 ch 6;
      write_double ch d
  | ConstClass c ->
      write_ui8 ch 7;
      write_constant ch consts (ConstStringUTF8 (unparse_objectType c))

let unparse_constant ch consts =
  let write_constant = write_constant ch consts in
    function
      | ConstValue v -> unparse_constant_value ch consts v
      | ConstField (c, s, signature) ->
	  write_ui8 ch 9;
	  write_constant (ConstValue (ConstClass (TClass c)));
	  write_constant (ConstNameAndType (s, SValue signature))
      | ConstMethod (c, s, signature) ->
	  write_ui8 ch 10;
	  write_constant (ConstValue (ConstClass c));
	  write_constant (ConstNameAndType (s, SMethod signature))
      | ConstInterfaceMethod (c, s, signature) ->
	  write_ui8 ch 11;
	  write_constant (ConstValue (ConstClass (TClass c)));
	  write_constant (ConstNameAndType (s, SMethod signature))
      | ConstNameAndType (s, signature) ->
	  write_ui8 ch 12;
	  write_constant (ConstStringUTF8 s);
	  write_constant (ConstStringUTF8 (unparse_signature signature))
      | ConstStringUTF8 s ->
	  write_ui8 ch 1;
	  write_string_with_length write_ui16 ch s
      | ConstUnusable -> ()

let unparse_constant_pool ch consts =
  let ch'' = output_string ()
  and i = ref 0 in
    while ! i < DynArray.length consts do
      unparse_constant ch'' consts (DynArray.get consts ! i);
      incr i
    done;
    write_ui16 ch (DynArray.length consts);
    nwrite ch (close_out ch'')

(* Acess (and other) flags unparsing *)
(*************************************)

let unparse_flags flags =
  let all_flags = [
    AccPublic; AccPrivate; AccProtected; AccStatic; AccFinal; AccSynchronized;
    AccVolatile; AccTransient; AccNative; AccInterface; AccAbstract; AccStrict;
    AccRFU 1; AccRFU 2; AccRFU 3; AccRFU 4 ]
  and fl = ref 0
  and fbit = ref 0 in
    List.iter
      (fun f ->
	 if List.mem f flags
	 then fl := ! fl lor (1 lsl ! fbit);
	 incr fbit)
      all_flags;
    !fl

(* Attributes unparsing *)
(************************)

let unparse_stackmap_attribute consts stackmap =
  let ch = output_string ()
  in
    write_with_size write_ui16 ch
      (function pc, lt, st ->
	let unparse_list =
	  write_with_size write_ui16 ch
	    (function
	      | VTop  -> write_ui8 ch 0
	      | VInteger  -> write_ui8 ch 1
	      | VFloat -> write_ui8 ch 2
	      | VDouble -> write_ui8 ch 3
	      | VLong -> write_ui8 ch 4
	      | VNull -> write_ui8 ch 5
	      | VUninitializedThis -> write_ui8 ch 6
	      | VObject o ->
		  write_ui8 ch 7 ; write_constant ch consts(ConstValue (ConstClass o))
	      | VUninitialized pc -> write_ui8 ch 8 ; write_ui16 ch pc)
	in
	  write_ui16 ch pc;
	  unparse_list lt;
	  unparse_list st)
      stackmap;
    ("StackMap",close_out ch)

let rec unparse_attribute_to_strings consts =
  let ch = output_string () in
    function
      | AttributeSourceFile s ->
	  write_constant ch consts (ConstStringUTF8 s);
	  ("SourceFile",close_out ch)
      | AttributeConstant c ->
	  write_constant ch consts (ConstValue c);
	  ("ConstantValue",close_out ch)
      | AttributeExceptions l ->
	  write_with_size write_ui16 ch
	    (function c ->
	      write_constant ch consts (ConstValue (ConstClass (TClass c))))
	    l;
	  ("Exceptions",close_out ch)
      | AttributeInnerClasses l ->
	  write_with_size write_ui16 ch
	    (function inner, outer, inner_name, flags ->
	      (match inner with
		| None -> write_ui16 ch 0
		| Some inner ->
		    write_constant ch consts (ConstValue (ConstClass (TClass inner))));
	      (match outer with
		| None -> write_ui16 ch 0
		| Some outer ->
		    write_constant ch consts (ConstValue (ConstClass (TClass outer))));
	      (match inner_name with
		| None -> write_ui16 ch 0
		| Some inner_name ->
		    write_constant ch consts (ConstStringUTF8 inner_name));
	      write_ui16 ch (unparse_flags flags))
	    l;
	  ("InnerClasses",close_out ch)
      | AttributeSynthetic ->
	  ("Synthetic",close_out ch)
      | AttributeLineNumberTable l ->
	  write_with_size write_ui16 ch
	    (function pc, line ->
	      write_ui16 ch pc;
	      write_ui16 ch line)
	    l;
	  ("LineNumberTable",close_out ch)
      | AttributeLocalVariableTable l ->
	  write_with_size write_ui16 ch
	    (function start_pc, length, name, signature, index ->
	      write_ui16 ch start_pc;
	      write_ui16 ch length;
	      write_constant ch consts (ConstStringUTF8 name);
	      write_constant ch consts
		(ConstStringUTF8 (unparse_value_signature signature));
	      write_ui16 ch index)
	    l;
	  ("LocalVariableTable",close_out ch)
      | AttributeDeprecated ->
	  ("Deprecated",close_out ch)
      | AttributeStackMap s ->
	  ignore (close_out ch);
	  unparse_stackmap_attribute consts s
      | AttributeUnknown (name, contents) ->
	  (name,contents)
      | AttributeCode code ->
	  write_ui16 ch code.c_max_stack;
	  write_ui16 ch code.c_max_locals;
	  write_with_length write_i32 ch
	    (function ch ->
	      JCode.unparse_code ch code.c_code);
	  write_with_size write_ui16 ch
	    (function e ->
	      write_ui16 ch e.e_start;
	      write_ui16 ch e.e_end;
	      write_ui16 ch e.e_handler;
	      match e.e_catch_type with
		| Some cl -> write_constant ch consts (ConstValue (ConstClass (TClass cl)))
		| None -> write_ui16 ch 0)
	    code.c_exc_tbl;
	  write_with_size write_ui16 ch
	    (unparse_attribute ch consts)
	    code.c_attributes;
	  ("Code",close_out ch)

and unparse_attribute ch consts attr =
  let (name,content) = unparse_attribute_to_strings consts attr
  in
    write_constant ch consts (ConstStringUTF8 name);
    write_string_with_length write_i32 ch content

(* Fields, methods and classes *)
(*******************************)

let unparse_field ch consts field =
  write_ui16 ch (unparse_flags field.f_flags);
  write_constant ch consts (ConstStringUTF8 field.f_name);
  write_constant ch consts
    (ConstStringUTF8 (unparse_value_signature field.f_signature));
  write_with_size write_ui16 ch
    (unparse_attribute ch consts)
    field.f_attributes

let unparse_method ch consts methode =
  if not
    (match
       methode.m_code,
       List.filter
	 (function AttributeCode _ -> true | _ -> false)
	 methode.m_attributes
     with
       | Some c, [AttributeCode c'] -> c == c' (* = is false because of nan. *)
       | None, [] -> true
       | _, l -> false)
  then
    raise
      (Class_structure_error "duplicate code or different versions in m_code and m_attributes");
  write_ui16 ch (unparse_flags methode.m_flags);
  write_constant ch consts (ConstStringUTF8 methode.m_name);
  write_constant ch consts
    (ConstStringUTF8 (unparse_method_signature methode.m_signature));
  write_with_size write_ui16 ch
    (unparse_attribute ch consts)
    methode.m_attributes

let unparse_class_low_level ch c =
  write_real_i32 ch 0xCAFEBABEl;
  write_ui16 ch version_minor;
  write_ui16 ch version_major;
  let ch' = output_string ()
  and consts = DynArray.of_array c.j_consts in
    write_ui16 ch' (unparse_flags c.j_flags);
    write_constant ch' consts (ConstValue (ConstClass (TClass c.j_name)));
    write_ui16 ch'
      (match c.j_super with
	 | Some super -> constant_to_int consts (ConstValue (ConstClass (TClass super)))
	 | None -> 0);
    let unparse unparse = write_with_size write_ui16 ch' unparse in
      unparse
	(function int ->
	   write_constant ch' consts (ConstValue (ConstClass (TClass int))))
	c.j_interfaces;
      unparse (unparse_field ch' consts) c.j_fields;
      unparse (unparse_method ch' consts) c.j_methods;
      unparse (unparse_attribute ch' consts) c.j_attributes;
      unparse_constant_pool ch consts;
      nwrite ch (close_out ch')

let unparse_class ch c =
  unparse_class_low_level ch (JHigh2Low.high2low c)
