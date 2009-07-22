(*
 * This file is part of JavaLib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008 Laurent Hubert (CNRS)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
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

open IO
open IO.BigEndian
open JBasics
open JClassLow
open JCode
open JUnparseSignature


(* Constant pool unparsing *)
(***************************)

let unparse_constant_value ch consts = function
  | ConstString s ->
      write_ui8 ch 8;
      (write_string ch consts s)
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
      write_string ch consts (unparse_objectType c)

let unparse_constant ch consts =
  function
    | ConstValue v -> unparse_constant_value ch consts v
    | ConstField (c, s, signature) ->
	write_ui8 ch 9;
	write_class ch consts c;
	write_name_and_type ch consts (s, SValue signature)
    | ConstMethod (c, s, signature) ->
	write_ui8 ch 10;
	write_object_type ch consts c;
	write_name_and_type ch consts (s, SMethod signature)
    | ConstInterfaceMethod (c, s, signature) ->
	write_ui8 ch 11;
	write_class ch consts c;
	write_name_and_type ch consts (s, SMethod signature)
    | ConstNameAndType (s, signature) ->
	write_ui8 ch 12;
	write_string ch consts s;
	write_string ch consts (unparse_descriptor signature)
    | ConstStringUTF8 s ->
	write_ui8 ch 1;
	write_string_with_length write_ui16 ch s
    | ConstUnusable -> ()

let unparse_constant_pool ch consts =
  let ch'' = output_string ()
  and i = ref 0 in
    while ! i < DynArray.length consts do
      unparse_constant ch'' consts (DynArray.unsafe_get consts ! i);
      incr i
    done;
    write_ui16 ch (DynArray.length consts);
    nwrite ch (close_out ch'')

(* Acess (and other) flags unparsing *)
(*************************************)
let class_flags =
  [`AccPublic; `AccRFU 0x2; `AccRFU 0x4; `AccRFU 0x8;
   `AccFinal; `AccSuper; `AccRFU 0x40; `AccRFU 0x80;
   `AccRFU 0x100; `AccInterface; `AccAbstract; `AccRFU 0x800;
   `AccSynthetic; `AccAnnotation; `AccEnum; `AccRFU 0x8000]
let innerclass_flags =
  [`AccPublic; `AccPrivate; `AccProtected; `AccStatic;
   `AccFinal; `AccRFU 0x20; `AccRFU 0x40; `AccRFU 0x80;
   `AccRFU 0x100; `AccInterface; `AccAbstract; `AccRFU 0x800;
   `AccSynthetic; `AccAnnotation; `AccEnum; `AccRFU 0x8000]
let field_flags =
  [`AccPublic; `AccPrivate; `AccProtected; `AccStatic;
   `AccFinal; `AccRFU 0x20; `AccVolatile; `AccTransient;
   `AccRFU 0x100; `AccRFU 0x200; `AccRFU 0x400; `AccRFU 0x800;
   `AccSynthetic; `AccRFU 0x2000; `AccEnum; `AccRFU 0x8000]
let method_flags =
  [`AccPublic; `AccPrivate; `AccProtected; `AccStatic;
   `AccFinal; `AccSynchronized; `AccBridge; `AccVarArgs;
   `AccNative; `AccRFU 0x200; `AccAbstract; `AccStrict;
   `AccSynthetic; `AccRFU 0x2000; `AccRFU 0x4000; `AccRFU 0x8000]

let unparse_flags all_flags flags =
  let fl = ref 0
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
		  write_ui8 ch 7 ; write_object_type ch consts o
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
      | AttributeSignature s ->
	  write_string ch consts s;
	  ("Signature",close_out ch)
      | AttributeEnclosingMethod (cn,mso) ->
	  write_class ch consts cn;
	  (match mso with
	     | Some (n,t) ->
                 write_name_and_type ch consts (n,t)
	     | None ->
		 write_ui16 ch 0);
	  ("EnclosingMethod", close_out ch)
      | AttributeSourceDebugExtension s ->
	  ("SourceDebugExtension", s)
      | AttributeSourceFile s ->
	  write_string ch consts s;
	  ("SourceFile",close_out ch)
      | AttributeConstant c ->
	  write_value ch consts c;
	  ("ConstantValue",close_out ch)
      | AttributeExceptions l ->
	  write_with_size write_ui16 ch
	    (function c -> write_class ch consts c)
	    l;
	  ("Exceptions",close_out ch)
      | AttributeInnerClasses l ->
	  write_with_size write_ui16 ch
	    (function inner, outer, inner_name, flags ->
	       (match inner with
		  | None -> write_ui16 ch 0
		  | Some inner -> write_class ch consts inner);
	       (match outer with
		  | None -> write_ui16 ch 0
		  | Some outer -> write_class ch consts outer);
	       (match inner_name with
		  | None -> write_ui16 ch 0
		  | Some inner_name ->
		      write_string ch consts inner_name);
	       write_ui16 ch (unparse_flags innerclass_flags flags))
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
	       write_string ch consts name;
	       write_string ch consts (unparse_value_type signature);
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
	  let code = Lazy.force code in
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
		   | Some cl -> write_class ch consts cl
		   | None -> write_ui16 ch 0)
	      code.c_exc_tbl;
	    write_with_size write_ui16 ch
	      (unparse_attribute ch consts)
	      code.c_attributes;
	    ("Code",close_out ch)

and unparse_attribute ch consts attr =
  let (name,content) = unparse_attribute_to_strings consts attr
  in
    write_string ch consts name;
    write_string_with_length write_i32 ch content

(* Fields, methods and classes *)
(*******************************)

let unparse_field ch consts field =
  write_ui16 ch (unparse_flags field_flags field.f_flags);
  write_string ch consts field.f_name;
  write_string ch consts (unparse_value_type field.f_descriptor);
  write_with_size write_ui16 ch
    (unparse_attribute ch consts)
    field.f_attributes

let unparse_method ch consts methode =
  if
    List.length
      (List.filter
	 (function AttributeCode _ -> true | _ -> false)
	 methode.m_attributes)
    > 1
  then
    raise
      (Class_structure_error "duplicate code or different versions in m_code and m_attributes");
  write_ui16 ch (unparse_flags method_flags methode.m_flags);
  write_string ch consts methode.m_name;
  write_string ch consts (unparse_method_descriptor methode.m_descriptor);
  write_with_size write_ui16 ch
    (unparse_attribute ch consts)
    methode.m_attributes

let unparse_class_low_level ch c =
  write_real_i32 ch 0xCAFEBABEl;
  write_ui16 ch c.j_version.minor;
  write_ui16 ch c.j_version.major;
  let ch' = output_string ()
  and consts = DynArray.of_array c.j_consts in
    write_ui16 ch' (unparse_flags class_flags c.j_flags);
    write_class ch' consts c.j_name;
    write_ui16 ch'
      (match c.j_super with
	 | Some super -> constant_to_int consts (ConstValue (ConstClass (TClass super)))
	 | None -> 0);
    let unparse unparse = write_with_size write_ui16 ch' unparse in
      unparse
	(function int -> write_class ch' consts int)
	c.j_interfaces;
      unparse (unparse_field ch' consts) c.j_fields;
      unparse (unparse_method ch' consts) c.j_methods;
      unparse (unparse_attribute ch' consts) c.j_attributes;
      unparse_constant_pool ch consts;
      nwrite ch (close_out ch')

let unparse_class ch c =
  unparse_class_low_level ch (JHigh2Low.high2low c)
