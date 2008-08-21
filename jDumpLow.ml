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
open JClassLow
open JDumpBasics

let sprintf = Printf.sprintf

let opcode = function
  | OpNop -> "nop"
  | OpAConstNull -> "aconstnull"
  | OpIConst i -> sprintf "iconst %ld" i
  | OpLConst i -> sprintf "lconst %Ld" i
  | OpFConst f -> sprintf "fconst %f" f
  | OpDConst f -> sprintf "dconst %f" f
  | OpBIPush i -> sprintf "biconst %d" i
  | OpSIPush i -> sprintf "siconst %d" i
  | OpLdc1 i -> sprintf "ldc1 %d" i
  | OpLdc1w i -> sprintf "ldc1w %d" i
  | OpLdc2w i -> sprintf "ldc2w %d" i

  | OpLoad (t,i) -> sprintf "%cload %d" (jvm_basic_type t) i
  | OpALoad i -> sprintf "aload %d" i

  | OpArrayLoad t -> sprintf "%caload" (jvm_basic_type t)
  | OpAALoad -> "aaload"
  | OpBALoad -> "baload"
  | OpCALoad -> "caload"
  | OpSALoad -> "saload"

  | OpStore (t,i) -> sprintf "%cstore %d" (jvm_basic_type t) i
  | OpAStore i -> sprintf "astore %d" i

  | OpArrayStore t -> sprintf "%castore" (jvm_basic_type t)
  | OpAAStore -> "aastore"
  | OpBAStore -> "bastore"
  | OpCAStore -> "castore"
  | OpSAStore -> "sastore"

  | OpPop -> "pop"
  | OpPop2 -> "pop2"
  | OpDup -> "dup"
  | OpDupX1 -> "dupX1"
  | OpDupX2 -> "dupX2"
  | OpDup2 -> "dup2"
  | OpDup2X1 -> "dup2X1"
  | OpDup2X2 -> "dup2X2"
  | OpSwap -> "swap"

  | OpAdd k -> sprintf "%cadd" (jvm_basic_type k)
  | OpSub k -> sprintf "%csub" (jvm_basic_type k)
  | OpMult k -> sprintf "%cmult" (jvm_basic_type k)
  | OpDiv k -> sprintf "%cdiv" (jvm_basic_type k)
  | OpRem k -> sprintf "%crem" (jvm_basic_type k)
  | OpNeg k -> sprintf "%cneg" (jvm_basic_type k)

  | OpIShl -> "ishl"
  | OpLShl -> "lshl"
  | OpIShr -> "ishr"
  | OpLShr -> "lshr"
  | OpIUShr -> "iushr"
  | OpLUShr -> "lushr"
  | OpIAnd -> "iand"
  | OpLAnd -> "land"
  | OpIOr -> "ior"
  | OpLOr -> "lor"
  | OpIXor -> "ixor"
  | OpLXor -> "lxor"

  | OpIInc (a,b) -> sprintf "iinc %d %d" a b

  | OpI2L -> "i2l"
  | OpI2F -> "i2f"
  | OpI2D -> "i2d"
  | OpL2I -> "l2i"
  | OpL2F -> "l2f"
  | OpL2D -> "l2d"
  | OpF2I -> "f2i"
  | OpF2L -> "f2l"
  | OpF2D -> "f2d"
  | OpD2I -> "d2i"
  | OpD2L -> "d2l"
  | OpD2F -> "d2f"
  | OpI2B -> "i2b"
  | OpI2C -> "i2c"
  | OpI2S -> "i2s"

  | OpLCmp -> "lcmp"
  | OpFCmpL -> "fcmpl"
  | OpFCmpG -> "fcmpg"
  | OpDCmpL -> "dcmpl"
  | OpDCmpG -> "dcmpg"
  | OpIfEq i -> sprintf "ifEq %d" i
  | OpIfNe i -> sprintf "ifNe %d" i
  | OpIfLt i -> sprintf "ifLt %d" i
  | OpIfGe i -> sprintf "ifGe %d" i
  | OpIfGt i -> sprintf "ifGt %d" i
  | OpIfLe i -> sprintf "ifLe %d" i
  | OpICmpEq i -> sprintf "ifcmpeq %d" i
  | OpICmpNe i -> sprintf "ifcmpne %d" i
  | OpICmpLt i -> sprintf "ifcmplt %d" i
  | OpICmpGe i -> sprintf "ifcmpge %d" i
  | OpICmpGt i -> sprintf "ifcmpgt %d" i
  | OpICmpLe i -> sprintf "ifcmple %d" i
  | OpACmpEq i -> sprintf "ifacmpeq %d" i
  | OpACmpNe i -> sprintf "ifacmpne %d" i
  | OpGoto i -> sprintf "goto %d" i
  | OpJsr i -> sprintf "goto %d" i
  | OpRet i -> sprintf "goto %d" i

  | OpTableSwitch (def,min,max,tbl) ->
      (* "tableswitch ([_:_] -> [_,_,_,...],default:_)" *)
      let inst = "tableswitch (["^ Int32.to_string min ^":"^ Int32.to_string max ^"] -> ["
      and table = String.concat "," (Array.to_list (Array.map string_of_int tbl))
      in inst^table^"],default:"^ string_of_int def^")"

  | OpLookupSwitch (default,jumps) ->
      let inst =
	List.fold_left
	  (fun s (int,offset) -> s ^ Int32.to_string int ^"->" ^ string_of_int offset^ " | ")
	  "lookupswitch "
	  jumps
      in inst ^ "_ ->" ^string_of_int default

  | OpReturn k -> sprintf "%creturn" (jvm_basic_type k)
  | OpAReturn -> "areturn"
  | OpReturnVoid -> "return"

  | OpGetStatic i -> sprintf "getstatic %d" i
  | OpPutStatic i -> sprintf "putstatic %d" i
  | OpGetField i -> sprintf "getfield %d" i
  | OpPutField i -> sprintf "putfield %d" i
  | OpInvokeVirtual i -> sprintf "invokevirtual %d" i
  | OpInvokeNonVirtual i -> sprintf "invokespecial %d" i
  | OpInvokeStatic i -> sprintf "invokestatic %d" i
  | OpInvokeInterface (i,n) -> sprintf "invokeinterface %d %d" i n

  | OpNew i -> sprintf "new %d" i
  | OpNewArray k -> sprintf "%cnewarray" (java_basic_type k)
  | OpANewArray i -> sprintf "anewarray %d" i
  | OpArrayLength -> "arraylenth"
  | OpThrow -> "throw"
  | OpCheckCast i -> sprintf "checkcast %d" i
  | OpInstanceOf i -> sprintf "instanceof %d" i
  | OpMonitorEnter -> "monitorenter"
  | OpMonitorExit -> "monitorexit"
  | OpAMultiNewArray (c,n) -> sprintf "amultinewarray type:%d dims:%d" c n
  | OpIfNull i -> sprintf "ifnull %d" i
  | OpIfNonNull i -> sprintf "ifnonnull %d" i
  | OpGotoW i -> sprintf "gotow %d" i
  | OpJsrW i -> sprintf "jsrw %d" i
  | OpBreakpoint -> "breakpoint"
  | OpRetW i -> sprintf "retw %d" i

  | OpInvalid -> "invalid"



let rec dump_code ch cl code =
	IO.printf ch "max_stack = %d , max_locals = %d\n" code.JClassLow.c_max_stack code.JClassLow.c_max_locals;
	Array.iteri (fun i c ->
		match c with
		  | OpInvalid -> (); (* IO.printf ch "__\n" *)
		  | _ -> IO.printf ch "      %.4i (%.4X) %s\n" i i (opcode c)
	) code.JClassLow.c_code;
	IO.printf ch "    exceptions"; List.iter (dump_exc ch cl) code.JClassLow.c_exc_tbl;
	List.iter (dump_attrib ch cl) code.JClassLow.c_attributes

and dump_attrib ch cl = function
	| AttributeSourceFile s ->
	    IO.printf ch "    source = %s\n" s
	| AttributeSignature s ->
	    IO.printf ch "    signature = %s\n" s
	| AttributeEnclosingMethod (cn,mso) ->
	    IO.printf ch "    enclosing method : class = %s, method = " (JDumpBasics.class_name cn);
	    (match mso with
	       | None -> IO.printf ch "None"
	       | Some (mn,ms) -> IO.printf ch "%s" (JDumpBasics.signature mn ms));
	    IO.printf ch "\n"
	| AttributeConstant c ->
	    IO.printf ch "    const "; dump_constant_value ch c; IO.printf ch "\n";
	| AttributeCode code ->
	    dump_code ch cl (Lazy.force code) (* IO.printf ch "    unexpected code attribute" *)
	| AttributeExceptions l ->
	    IO.printf ch "    exceptions";
	    List.iter (fun cn -> IO.nwrite ch (class_name cn^" ")) l;
	    IO.printf ch "\n"
	| AttributeInnerClasses _ ->
	    IO.printf ch "    inner-classes\n"
	| AttributeSynthetic ->
	    IO.printf ch "    synthetic\n"
	| AttributeLineNumberTable _lines ->
	    IO.printf ch "    line-numbers\n"
	| AttributeLocalVariableTable variables ->
	    IO.printf ch "    local-variables\n";
	    List.iter
	      (function start_pc, length, name, signature, index ->
		 IO.printf ch "      from %d to %d, %s %s at %d\n"
		   start_pc
		   (start_pc + length)
		   (value_signature signature)
		   name
		   index)
	      variables
	| AttributeDeprecated ->
	    IO.printf ch "    deprecated\n"
	| AttributeStackMap stackmap_frames ->
	    IO.printf ch "    stackmap = ["; List.iter (dump_stackmap ch) stackmap_frames; IO.printf ch "]\n";
	| AttributeUnknown (s,_) ->
	    IO.printf ch "    ?%s\n" s

let access_flags = function
	| [] -> ""
	| flags ->
		String.concat " " (List.map (function
			| `AccPublic -> "public"
			| `AccPrivate -> "private"
			| `AccProtected -> "protected"
			| `AccStatic -> "static"
			| `AccFinal -> "final"
			| `AccSynchronized -> "synchronized"
			| `AccVolatile -> "volatile"
			| `AccTransient -> "transient"
			| `AccNative -> "native"
			| `AccInterface -> "interface"
			| `AccAbstract -> "abstract"
			| `AccStrict -> "strict"
			| `AccEnum -> "enum"
			| `AccAnnotation -> "annotation"
			| `AccVarArgs -> "VarArgs"
			| `AccBridge -> "bridge"
			| `AccSuper -> "`AccSuper"
			| `AccSynthetic -> "synthetic"
			| `AccRFU i -> Printf.sprintf "rfu 0x%X" i
		) flags) ^ " "

let dump_field ch cl f =
	IO.printf ch "  %s%s %s\n" (access_flags f.f_flags) (value_signature f.f_descriptor) f.f_name;
	List.iter (dump_attrib ch cl) f.f_attributes

let dump_method ch cl m =
	IO.printf ch "  %s%s\n" (access_flags m.m_flags) (method_signature m.m_name m.m_descriptor);
	List.iter (dump_attrib ch cl) m.m_attributes;
	IO.write ch '\n'

let dump_super ch = function
  | None -> ()
  | Some c -> IO.printf ch "  extends %s\n" (class_name c)

let dump ch cl =
	IO.printf ch "%sclass %s\n" (access_flags cl.j_flags) (class_name cl.j_name);
	dump_super ch cl.j_super;
	if cl.j_interfaces <> [] then IO.printf ch "  implements %s\n" (String.concat " " (List.map class_name cl.j_interfaces));
	IO.printf ch "{\n\n";
	IO.printf ch "/* **** CONSTANTS ****\n";
(* Put this in the dump method for high level class files
	Array.iteri (fun i c ->
		IO.printf ch "    %d  " i;
		dump_constant ch c;
		IO.write ch '\n'
	) cl.j_consts;
*)
	IO.printf ch "// ****************** */\n\n";
	List.iter (dump_field ch cl) cl.j_fields;
	IO.printf ch "\n";
	List.iter (dump_method ch cl) cl.j_methods;
	IO.printf ch "}\n";
