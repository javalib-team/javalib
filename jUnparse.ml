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
open JConsts
open IO
open IO.BigEndian

(* Classfile version *)
let version_major = 49
and version_minor = 0

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

(* Instructions unparsing *)
(**************************)

module OpcodeMap =
  Map.Make(struct type t = opcode let compare = compare end)

let simple_table =
  List.fold_left
    (fun m (offset, opcodes) ->
       fst
	 (Array.fold_left
	    (fun (m, code) op ->
	       OpcodeMap.add op code m, succ code)
	    (m, offset)
	    opcodes))
    OpcodeMap.empty
    [
      050, [|OpAALoad;
	     OpBALoad;
	     OpCALoad;
	     OpSALoad
	   |];
      083, [|OpAAStore;
	     OpBAStore;
	     OpCAStore;
	     OpSAStore
	   |];
      087, [|OpPop;
	     OpPop2;
	     OpDup;
	     OpDupX1;
	     OpDupX2;
	     OpDup2;
	     OpDup2X1;
	     OpDup2X2;
	     OpSwap
	   |];
      120, [|OpIShl;
	     OpLShl;
	     OpIShr;
	     OpLShr;
	     OpIUShr;
	     OpLUShr;
	     OpIAnd;
	     OpLAnd;
	     OpIOr;
	     OpLOr;
	     OpIXor;
	     OpLXor
	   |];
      133, [|OpI2L;
	     OpI2F;
	     OpI2D;
	     OpL2I;
	     OpL2F;
	     OpL2D;
	     OpF2I;
	     OpF2L;
	     OpF2D;
	     OpD2I;
	     OpD2L;
	     OpD2F;
	     OpI2B;
	     OpI2C;
	     OpI2S
	   |];
      148, [|OpLCmp;
	     OpFCmpL;
	     OpFCmpG;
	     OpDCmpL;
	     OpDCmpG
	   |];
      000, [|OpNop;
	     OpAConstNull
	   |];
      176, [|OpAReturn;
	     OpReturnVoid
	   |];
      190, [|OpArrayLength;
	     OpThrow
	   |];
      194, [|OpMonitorEnter;
	     OpMonitorExit
	   |];
      202, [|OpBreakpoint|]
    ]

(* Instruction without arguments *)
let simple ch op =
  try
    write_ui8 ch
      (OpcodeMap.find op simple_table)
  with
      Not_found -> invalid_arg "simple"

let int_of_kind = function
  | KInt -> 0
  | KLong -> 1
  | KFloat -> 2
  | KDouble -> 3

(* Instructions with a kind argument added to the base opcode. *)
let kind ch inst =
  let kind, opcode = match inst with
    | OpArrayLoad k -> k, 46
    | OpArrayStore k -> k, 79
    | OpAdd i -> i, 96
    | OpSub i -> i, 100
    | OpMult i -> i, 104
    | OpDiv i -> i, 108
    | OpRem i -> i, 112
    | OpNeg i -> i, 116
    | OpReturn k -> k, 172
    | _ -> invalid_arg "kind"
  in
    write_ui8 ch (opcode + int_of_kind kind)

let unparse_local_instruction ch opcode value =
  if value <= 0xFF
  then (
    write_ui8 ch opcode;
    write_ui8 ch value
  ) else (
    write_ui8 ch 196;
    write_ui8 ch opcode;
    write_ui16 ch value
  )

(* Instructions xload, xstore (but not xaload, xastore) *)
let ilfda_loadstore ch instr =
  let value = match instr with
    | OpLoad (_, value)
    | OpALoad value
    | OpStore (_, value)
    | OpAStore value -> value
    | _ -> invalid_arg "ilfd_loadstore"
  in
    if value < 4
    then
      write_ui8 ch
	(value +
	   match instr with
	     | OpLoad (kind, _) -> 26 + 4 * int_of_kind kind
	     | OpALoad _ -> 42
	     | OpStore (kind, _) -> 59 + 4 * int_of_kind kind
	     | OpAStore _ -> 75
	     | _ -> assert false)
    else
      unparse_local_instruction ch
	(match instr with
	   | OpLoad (kind, _) -> 21 +  int_of_kind kind
	   | OpALoad _ -> 25
	   | OpStore (kind, _) -> 54 +  int_of_kind kind
	   | OpAStore _ -> 58
	   | _ -> assert false)
	value

(* Instructions with one 16 bits signed argument *)
let i16 ch inst =
  let i, opcode = match inst with
    | OpSIPush i -> i, 17
    | OpIfEq i -> i, 153
    | OpIfNe i -> i, 154
    | OpIfLt i -> i, 155
    | OpIfGe i -> i, 156
    | OpIfGt i -> i, 157
    | OpIfLe i -> i, 158
    | OpICmpEq i -> i, 159
    | OpICmpNe i -> i, 160
    | OpICmpLt i -> i, 161
    | OpICmpGe i -> i, 162
    | OpICmpGt i -> i, 163
    | OpICmpLe i -> i, 164
    | OpACmpEq i -> i, 165
    | OpACmpNe i -> i, 166
    | OpGoto i -> i, 167
    | OpJsr i -> i, 168
    | OpIfNull i -> i, 198
    | OpIfNonNull i -> i, 199
    | _ -> invalid_arg "i16"
  in
    write_ui8 ch opcode;
    write_i16 ch i

(* Instructions with one 16 bits unsigned argument *)
let ui16 ch inst =
  let i, opcode = match inst with
    | OpRetW i -> i, 209
    | _ -> invalid_arg "ui16"
  in
    write_ui8 ch opcode;
    write_ui16 ch i

(* Instructions with one class_name argument *)
let class_name consts ch inst =
  let c, opcode = match inst with
    | OpNew i -> (TObject i), 187
    | OpANewArray i -> i, 189
    | OpCheckCast i -> i, 192
    | OpInstanceOf i -> i, 193
    | _ -> invalid_arg "class_name"
  in
    write_ui8 ch opcode;
    write_constant ch consts (ConstClass c)

(* Instruction with a (class_name * string * signature) argument *)
let field_or_method consts ch inst =
  let arg, opcode = match inst with
    | OpGetStatic (c, n, s) -> ConstField (c, n, s), 178
    | OpPutStatic (c, n, s) -> ConstField (c, n, s), 179
    | OpGetField (c, n, s) -> ConstField (c, n, s), 180
    | OpPutField (c, n, s) -> ConstField (c, n, s), 181
    | OpInvokeVirtual (c, n, s) -> ConstMethod (c, n, s), 182
    | OpInvokeNonVirtual (c, n, s) -> ConstMethod (c, n, s), 183
    | OpInvokeStatic (c, n, s) -> ConstMethod (c, n, s), 184
    | _ -> invalid_arg "field_or_method"
  in
      write_ui8 ch opcode;
      write_constant ch consts arg

let array_type = [|
  ATBool;
  ATChar;
  ATFloat;
  ATDouble;
  ATByte;
  ATShort;
  ATInt;
  ATLong
|]

let padding ch count =
  flush ch;
  for i = 1 + (count () - 1) mod 4 to 3 do
    write_ui8 ch 0
  done

(* Everything else *)
let other consts count ch = function
  | OpIConst n -> write_ui8 ch (3 + Int32.to_int n)
  | OpLConst n -> write_ui8 ch (9 + Int64.to_int n)
  | OpFConst n -> write_ui8 ch (11 + int_of_float n)
  | OpDConst n -> write_ui8 ch (14 + int_of_float n)
  | OpBIPush n ->
      write_ui8 ch 16;
      write_ui8 ch n
  | OpLdc1 n ->
      let index = constant_to_int consts n in
	if
	  index <= 0xFF
	then (
	  write_ui8 ch 18;
	  write_ui8 ch index;
	) else (
	  write_ui8 ch 19;
	  write_ui16 ch index;
	)
  | OpLdc2w n ->
      write_ui8 ch 20;
      write_constant ch consts n
  | OpIInc (index, incr) ->
      if
	index <= 0xFF && - 0x80 <= incr && incr <= 0x7F
      then (
	write_ui8 ch 132;
	write_ui8 ch index;
	write_i8 ch incr
      ) else (
	write_ui8 ch 196;
	write_ui8 ch 132;
	write_ui16 ch index;
	write_i16 ch incr
      )
  | OpRet pc ->
      if
	pc <= 0xFF
      then (
	write_ui8 ch 169;
	write_ui8 ch pc
      ) else (
	write_ui8 ch 196;
	write_ui8 ch 169;
	write_ui16 ch pc
      )
  | OpTableSwitch (def, low, high, tbl) ->
      write_ui8 ch 170;
      padding ch count;
      write_i32 ch def;
      write_real_i32 ch low;
      write_real_i32 ch high;
      Array.iter (write_i32 ch) tbl
  | OpLookupSwitch (def, tbl) ->
      write_ui8 ch 171;
      padding ch count;
      write_i32 ch def;
      write_with_size write_i32 ch
	(function v, j ->
	   write_real_i32 ch v;
	   write_i32 ch j)
	tbl
  | OpInvokeInterface (c, m, s, nargs) ->
      write_ui8 ch 185;
      write_constant ch consts (ConstInterfaceMethod (c, m, s));
      write_ui8 ch nargs;
      write_ui8 ch 0
  | OpNewArray at ->
      write_ui8 ch 188;
      write_ui8 ch (4 + ExtArray.Array.findi (( = ) at) array_type)
  | OpAMultiNewArray (c, dims) ->
      write_ui8 ch 197;
      write_constant ch consts (ConstClass c);
      write_ui8 ch dims
  | OpGotoW i ->
      write_ui8 ch 200;
      write_i32 ch i
  | OpJsrW i ->
      write_ui8 ch 201;
      write_i32 ch i
  | OpInvalid -> ()
  | _ -> invalid_arg "other"

let unparse_instruction ch consts count inst =
  try
    List.iter
      (function unparse ->
	 try
	   unparse ch inst;
	   raise Exit
	 with
	     Invalid_argument _ -> ())
      [
	simple;
	kind;
	ilfda_loadstore;
	i16;
	ui16;
	class_name consts;
	field_or_method consts;
	other consts count
      ];
    assert false
  with
      Exit -> ()

(* Signature and classname encoding *)
(************************************)

let encode_class_name = function
  | t :: q ->
      List.fold_left
	(fun s x -> s ^ "/" ^ x)
	t
	q
  | [] -> invalid_arg "encode_class_name"

let rec unparse_signature = function
  | TByte -> "B"
  | TChar -> "C"
  | TDouble -> "D"
  | TFloat -> "F"
  | TInt -> "I"
  | TLong -> "J"
  | TShort -> "S"
  | TBool -> "Z"
  | TObject c ->
      "L" ^ encode_class_name c ^ ";"
  | TArray (s, size) ->
      let desc = ref "" in
	for i = 1 to
	  match size with Some s -> s | None -> 1
	do
	  desc := ! desc ^ "["
	done;
	! desc ^ unparse_signature s
  | TMethod (sigs, s) ->
      List.fold_left
	(fun desc s ->
	   desc ^ unparse_signature s)
	"("
	sigs
      ^ ")"
      ^ (match s with
	   | Some s -> unparse_signature s
	   | None -> "V")


let unparse_objectType = function
  | TObject c ->
      encode_class_name c
  | TArray _ as s -> unparse_signature s
  | _ -> invalid_arg "unparse_signature"

(* Constant pool unparsing *)
(***************************)

let unparse_constant ch consts =
  let write_constant = write_constant ch consts in
    function
      | ConstClass c ->
	  write_ui8 ch 7;
	  write_constant (ConstStringUTF8 (unparse_objectType c))
      | ConstField (c, s, signature) ->
	  write_ui8 ch 9;
	  write_constant (ConstClass (TObject c));
	  write_constant (ConstNameAndType (s, signature))
      | ConstMethod (c, s, signature) ->
	  write_ui8 ch 10;
	  write_constant (ConstClass (TObject c));
	  write_constant (ConstNameAndType (s, signature))
      | ConstInterfaceMethod (c, s, signature) ->
	  write_ui8 ch 11;
	  write_constant (ConstClass (TObject c));
	  write_constant (ConstNameAndType (s, signature))
      | ConstString s ->
	  write_ui8 ch 8;
	  write_constant (ConstStringUTF8 s)
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

let unparse_stackmap_attribute ch consts stackmap =
  write_constant ch consts (ConstStringUTF8 "StackMap");
  write_with_length write_i32 ch
    (function ch ->
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
		       write_ui8 ch 7 ; write_constant ch consts(ConstClass o)
		   | VUninitialized pc -> write_ui8 ch 8 ; write_ui16 ch pc)
	    in
	      write_ui16 ch pc;
	      unparse_list lt;
	      unparse_list st)
	 stackmap)

let unparse_attribute_not_code ch consts =
  let write_utf8 name =
    write_constant ch consts (ConstStringUTF8 name) in
    function
      | AttributeSourceFile s ->
	  write_utf8 "SourceFile";
	  write_with_length write_i32 ch
	    (function ch ->
	       write_constant ch consts (ConstStringUTF8 s))
      | AttributeConstant c ->
	  write_utf8 "ConstantValue";
	  write_with_length write_i32 ch
	    (function ch ->
	   write_constant ch consts c)
      | AttributeLineNumberTable l ->
	  write_utf8 "LineNumberTable";
	  write_with_length write_i32 ch
	    (function ch ->
	       write_with_size write_ui16 ch
		 (function pc, line ->
		    write_ui16 ch pc;
		    write_ui16 ch line)
		 l)
      | AttributeStackMap s -> unparse_stackmap_attribute ch consts s
      | AttributeUnknown (name, contents) ->
	  write_utf8 name;
	  write_string_with_length write_i32 ch contents
      | AttributeCode code -> invalid_arg "code"

let rec nops code i =
  try 
    match code.(i) with
      | OpInvalid -> nops code (succ i) + 1
      | _ -> 0
  with
    | Invalid_argument "index out of bounds" -> 0

(* Length of an instruction (as read in the classfile) :
   - 0 for OpInvalid
   - 1 + number of following OpInvalids otherwise. *)
let length code i =
  match code.(i) with
    | OpInvalid ->
	0
    | _ -> 1 + nops code (succ i)

let unparse_code_attribute ch consts code =
  write_constant ch consts (ConstStringUTF8 "Code");
  write_with_length write_i32 ch
    (function ch ->
       write_ui16 ch code.c_max_stack;
       write_ui16 ch code.c_max_locals;
       write_with_length write_i32 ch
	 (function ch ->
	    let ch', count = pos_out ch in
	      Array.iteri
		(fun i instr ->
		   try
		     let ch', count' = pos_out ch' in
		       unparse_instruction ch' consts count instr;
		       let nops = length code.c_code i - count' () in
			 if nops < 0
			 then failwith ("Instruction " ^ string_of_int (- nops) ^ " byte(s) too long");
			 for i = 1 to nops do
			   unparse_instruction ch' consts count OpNop
			 done
		   with e ->
		     prerr_endline ("Error when unparsing " ^ JDump.opcode instr);
		     raise e)
		code.c_code);
       write_with_size write_ui16 ch
	 (function e ->
	    write_ui16 ch e.e_start;
	    write_ui16 ch e.e_end;
	    write_ui16 ch e.e_handler;
	    match e.e_catch_type with
	      | Some cl -> write_constant ch consts (ConstClass (TObject cl))
	      | None -> write_ui16 ch 0)
	 code.c_exc_tbl;
       write_with_size write_ui16 ch
	 (unparse_attribute_not_code ch consts)
	 code.c_attributes)

let unparse_attribute ch consts = function
  | AttributeCode code -> unparse_code_attribute ch consts code
  | attr -> unparse_attribute_not_code ch consts attr

(* Fields, methods and classes *)
(*******************************)

let unparse_field ch consts field =
  write_ui16 ch (unparse_flags field.f_flags);
  write_constant ch consts (ConstStringUTF8 field.f_name);
  write_constant ch consts
    (ConstStringUTF8 (unparse_signature field.f_signature));
  write_with_size write_ui16 ch
    (unparse_attribute ch consts)
    field.f_attributes

let unparse_method ch consts methode =
  assert
    (match
       methode.m_code,
       List.filter
	 (function AttributeCode _ -> true | _ -> false)
	 methode.m_attributes
     with
       | Some c, [AttributeCode c'] -> c == c' (* = is false because of nan. *)
       | None, [] -> true
       | _, l -> false);
  write_ui16 ch (unparse_flags methode.m_flags);
  write_constant ch consts (ConstStringUTF8 methode.m_name);
  write_constant ch consts
    (ConstStringUTF8 (unparse_signature methode.m_signature));
  write_with_size write_ui16 ch
    (unparse_attribute ch consts)
    methode.m_attributes

let unparse_class ch c =
  write_real_i32 ch 0xCAFEBABEl;
  write_ui16 ch version_minor;
  write_ui16 ch version_major;
  let ch' = output_string ()
  and consts = DynArray.of_array c.j_consts in
    write_ui16 ch' (unparse_flags c.j_flags);
    write_constant ch' consts (ConstClass (TObject c.j_name));
    write_ui16 ch'
      (match c.j_super with
	 | Some super -> constant_to_int consts (ConstClass (TObject super))
	 | None -> 0);
    let unparse unparse = write_with_size write_ui16 ch' unparse in
      unparse
	(function int ->
	   write_constant ch' consts (ConstClass (TObject int)))
	c.j_interfaces;
      unparse (unparse_field ch' consts) c.j_fields;
      unparse (unparse_method ch' consts) c.j_methods;
      unparse (unparse_attribute ch' consts) c.j_attributes;
      unparse_constant_pool ch consts;
      nwrite ch (close_out ch')
