(*
 *  This file is part of JavaLib
 *  Copyright (c)2004 Nicolas Cannasse
 *  Copyright (c)2007 Université de Rennes 1 / CNRS
 *  Tiphaine.Turpin@irisa.fr
 *  Laurent.Hubert@irisa.fr
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
  | OpCodeNop -> "nop"
  | OpCodeAConstNull -> "aconstnull"
  | OpCodeIConst i -> sprintf "iconst %ld" i
  | OpCodeLConst i -> sprintf "lconst %Ld" i
  | OpCodeFConst f -> sprintf "fconst %f" f
  | OpCodeDConst f -> sprintf "dconst %f" f
  | OpCodeBIPush i -> sprintf "biconst %d" i
  | OpCodeSIPush i -> sprintf "siconst %d" i
  | OpCodeLdc1 i -> sprintf "ldc1 %d" i
  | OpCodeLdc1w i -> sprintf "ldc1w %d" i
  | OpCodeLdc2w i -> sprintf "ldc2w %d" i

  | OpCodeLoad (t,i) -> sprintf "%cload %d" (jvm_basic_type t) i
  | OpCodeALoad i -> sprintf "aload %d" i

  | OpCodeArrayLoad t -> sprintf "%caload" (jvm_basic_type t)
  | OpCodeAALoad -> "aaload"
  | OpCodeBALoad -> "baload"
  | OpCodeCALoad -> "caload"
  | OpCodeSALoad -> "saload"

  | OpCodeStore (t,i) -> sprintf "%cstore %d" (jvm_basic_type t) i
  | OpCodeAStore i -> sprintf "astore %d" i

  | OpCodeArrayStore t -> sprintf "%castore" (jvm_basic_type t)
  | OpCodeAAStore -> "aastore"
  | OpCodeBAStore -> "bastore"
  | OpCodeCAStore -> "castore"
  | OpCodeSAStore -> "sastore"

  | OpCodePop -> "pop"
  | OpCodePop2 -> "pop2"
  | OpCodeDup -> "dup"
  | OpCodeDupX1 -> "dupX1"
  | OpCodeDupX2 -> "dupX2"
  | OpCodeDup2 -> "dup2"
  | OpCodeDup2X1 -> "dup2X1"
  | OpCodeDup2X2 -> "dup2X2"
  | OpCodeSwap -> "swap"

  | OpCodeAdd k -> sprintf "%cadd" (jvm_basic_type k)
  | OpCodeSub k -> sprintf "%csub" (jvm_basic_type k)
  | OpCodeMult k -> sprintf "%cmult" (jvm_basic_type k)
  | OpCodeDiv k -> sprintf "%cdiv" (jvm_basic_type k)
  | OpCodeRem k -> sprintf "%crem" (jvm_basic_type k)
  | OpCodeNeg k -> sprintf "%cneg" (jvm_basic_type k)

  | OpCodeIShl -> "ishl"
  | OpCodeLShl -> "lshl"
  | OpCodeIShr -> "ishr"
  | OpCodeLShr -> "lshr"
  | OpCodeIUShr -> "iushr"
  | OpCodeLUShr -> "lushr"
  | OpCodeIAnd -> "iand"
  | OpCodeLAnd -> "land"
  | OpCodeIOr -> "ior"
  | OpCodeLOr -> "lor"
  | OpCodeIXor -> "ixor"
  | OpCodeLXor -> "lxor"

  | OpCodeIInc (a,b) -> sprintf "iinc %d %d" a b

  | OpCodeI2L -> "i2l"
  | OpCodeI2F -> "i2f"
  | OpCodeI2D -> "i2d"
  | OpCodeL2I -> "l2i"
  | OpCodeL2F -> "l2f"
  | OpCodeL2D -> "l2d"
  | OpCodeF2I -> "f2i"
  | OpCodeF2L -> "f2l"
  | OpCodeF2D -> "f2d"
  | OpCodeD2I -> "d2i"
  | OpCodeD2L -> "d2l"
  | OpCodeD2F -> "d2f"
  | OpCodeI2B -> "i2b"
  | OpCodeI2C -> "i2c"
  | OpCodeI2S -> "i2s"

  | OpCodeLCmp -> "lcmp"
  | OpCodeFCmpL -> "fcmpl"
  | OpCodeFCmpG -> "fcmpg"
  | OpCodeDCmpL -> "dcmpl"
  | OpCodeDCmpG -> "dcmpg"
  | OpCodeIfEq i -> sprintf "ifEq %d" i
  | OpCodeIfNe i -> sprintf "ifNe %d" i
  | OpCodeIfLt i -> sprintf "ifLt %d" i
  | OpCodeIfGe i -> sprintf "ifGe %d" i
  | OpCodeIfGt i -> sprintf "ifGt %d" i
  | OpCodeIfLe i -> sprintf "ifLe %d" i
  | OpCodeICmpEq i -> sprintf "ifcmpeq %d" i
  | OpCodeICmpNe i -> sprintf "ifcmpne %d" i
  | OpCodeICmpLt i -> sprintf "ifcmplt %d" i
  | OpCodeICmpGe i -> sprintf "ifcmpge %d" i
  | OpCodeICmpGt i -> sprintf "ifcmpgt %d" i
  | OpCodeICmpLe i -> sprintf "ifcmple %d" i
  | OpCodeACmpEq i -> sprintf "ifacmpeq %d" i
  | OpCodeACmpNe i -> sprintf "ifacmpne %d" i
  | OpCodeGoto i -> sprintf "goto %d" i
  | OpCodeJsr i -> sprintf "goto %d" i
  | OpCodeRet i -> sprintf "goto %d" i

  | OpCodeTableSwitch _ -> "tableswitch <...>"
  | OpCodeLookupSwitch _ -> "lookupswitch <...>"

  | OpCodeReturn k -> sprintf "%creturn" (jvm_basic_type k)
  | OpCodeAReturn -> "areturn"
  | OpCodeReturnVoid -> "return"

  | OpCodeGetStatic i -> sprintf "getstatic %d" i
  | OpCodePutStatic i -> sprintf "putstatic %d" i
  | OpCodeGetField i -> sprintf "getfield %d" i
  | OpCodePutField i -> sprintf "putfield %d" i
  | OpCodeInvokeVirtual i -> sprintf "invokevirtual %d" i
  | OpCodeInvokeNonVirtual i -> sprintf "invokespecial %d" i
  | OpCodeInvokeStatic i -> sprintf "invokestatic %d" i
  | OpCodeInvokeInterface (i,n) -> sprintf "invokeinterface %d %d" i n

  | OpCodeNew i -> sprintf "new %d" i
  | OpCodeNewArray k -> sprintf "%cnewarray" (java_basic_type k)
  | OpCodeANewArray i -> sprintf "anewarray %d" i
  | OpCodeArrayLength -> "arraylenth"
  | OpCodeThrow -> "throw"
  | OpCodeCheckCast i -> sprintf "checkcast %d" i
  | OpCodeInstanceOf i -> sprintf "instanceof %d" i
  | OpCodeMonitorEnter -> "monitorenter"
  | OpCodeMonitorExit -> "monitorexit"
  | OpCodeAMultiNewArray (c,n) -> sprintf "amultinewarray type:%d dims:%d" c n
  | OpCodeIfNull i -> sprintf "ifnull %d" i
  | OpCodeIfNonNull i -> sprintf "ifnonnull %d" i
  | OpCodeGotoW i -> sprintf "gotow %d" i
  | OpCodeJsrW i -> sprintf "jsrw %d" i
  | OpCodeBreakpoint -> "breakpoint"
  | OpCodeRetW i -> sprintf "retw %d" i

  | OpCodeInvalid -> "invalid"



let rec dump_code ch cl code =
	IO.printf ch "max_stack = %d , max_locals = %d\n" code.JClassLow.c_max_stack code.JClassLow.c_max_locals;
	Array.iteri (fun i c ->
		match c with
		  | OpCodeInvalid -> (); (* IO.printf ch "__\n" *)
		  | _ -> IO.printf ch "      %.4i (%.4X) %s\n" i i (opcode c)
	) code.JClassLow.c_code;
	IO.printf ch "    exceptions"; List.iter (dump_exc ch cl) code.JClassLow.c_exc_tbl;
	List.iter (dump_attrib ch cl) code.JClassLow.c_attributes

and dump_attrib ch cl = function
	| AttributeSourceFile s ->
	    IO.printf ch "    source = %s\n" s
	| AttributeConstant c ->
	    IO.printf ch "    const "; dump_constant_value ch c; IO.printf ch "\n";
	| AttributeCode code ->
	    dump_code ch cl code (* IO.printf ch "    unexpected code attribute" *)
	| AttributeExceptions l ->
	    IO.printf ch "    exceptions";
	    List.iter (fun cn -> IO.nwrite ch (class_name cn^" ")) l;
	    IO.printf ch "\n"
	| AttributeInnerClasses _ ->
	    IO.printf ch "    inner-classes\n"
	| AttributeSynthetic ->
	    IO.printf ch "    synthetic\n"
	| AttributeLineNumberTable lines ->
	    IO.printf ch "    line-numbers\n"
	| AttributeLocalVariableTable variables ->
	    IO.printf ch "    local-variables\n";
	    List.iter
	      (function start_pc, length, name, signature, index ->
		 IO.printf ch "      from %d to %d, %s at %d\n"
		   start_pc
		   (start_pc + length)
		   (value_signature name signature)
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
			| AccPublic -> "public"
			| AccPrivate -> "private"
			| AccProtected -> "protected"
			| AccStatic -> "static"
			| AccFinal -> "final"
			| AccSynchronized -> "synchronized"
			| AccVolatile -> "volatile"
			| AccTransient -> "transient"
			| AccNative -> "native"
			| AccInterface -> "interface"
			| AccAbstract -> "abstract"
			| AccStrict -> "strict"
			| AccRFU i -> "rfu" ^ string_of_int i
		) flags) ^ " "

let dump_field ch cl f =
	IO.printf ch "  %s%s\n" (access_flags f.f_flags) (value_signature f.f_name f.f_signature);
	List.iter (dump_attrib ch cl) f.f_attributes

let dump_method ch cl m =
	IO.printf ch "  %s%s\n" (access_flags m.m_flags) (method_signature m.m_name m.m_signature);
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
