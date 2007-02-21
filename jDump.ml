(*
 *  This file is part of JavaLib
 *  Copyright (c)2004 Nicolas Cannasse
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

(* last modified by eandre@irisa.fr 2006/06/08 *)


open JClass

let class_name = String.concat "."

let sprintf = Printf.sprintf

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

let rec signature name = function
	| TByte -> "byte " ^ name
	| TChar -> "char " ^ name
	| TDouble -> "double " ^ name
	| TFloat -> "float " ^ name
	| TInt -> "int " ^ name
	| TLong -> "long " ^ name
	| TShort -> "short " ^ name
	| TBool -> "bool " ^ name
	| TObject cl -> class_name cl ^ " " ^ name
	| TArray (s,size) ->
		signature name s ^ "[" ^ (match size with None -> "" | Some n -> string_of_int n) ^ "]"
	| TMethod (sl,sr) ->
		(match sr with
		| None -> "void " ^ name
		| Some s -> signature name s) ^ "(" ^ String.concat "," (List.map (signature "") sl) ^ ")"

let kind = function
	| KInt -> 'i'
	| KLong -> 'l'
	| KFloat -> 'f'
	| KDouble -> 'd'

let array_type = function
	| ATBool -> "bool"
	| ATChar -> "char"
	| ATFloat -> "float"
	| ATDouble -> "double"
	| ATByte -> "byte"
	| ATShort -> "short"
	| ATInt -> "int"
	| ATLong -> "long"

let opcode = function
	| OpNop -> "nop"
	| OpAConstNull -> "aconstnull"
	| OpIConst i -> sprintf "iconst %ld" i
	| OpLConst i -> sprintf "lconst %Ld" i
	| OpFConst f -> sprintf "fconst %f" f
	| OpDConst f -> sprintf "dconst %f" f
	| OpBIPush n -> sprintf "bipush %d" n
	| OpSIPush a -> sprintf "sipush %d " a
	    (* modified by eandre@irisa.fr 2006/05/02 *)
	| OpLdc1 n -> sprintf "ldc1 %d" n
	| OpLdc1w n -> sprintf "ldc1w %d" n
	| OpLdc2w n -> sprintf "ldc2w %d" n

	| OpLoad (k,n) -> sprintf "%cload %d" (kind k) n
	| OpALoad n -> sprintf "aload %d" n

	| OpArrayLoad k -> sprintf "%caload" (kind k)
	| OpAALoad -> "aaload"
	| OpBALoad -> "baload"
	| OpCALoad -> "caload"
	| OpSALoad -> "saload"

	| OpStore (k,n) -> sprintf "%cstore %d" (kind k) n
	| OpAStore n -> sprintf "astore %d" n

	| OpArrayStore k -> sprintf "%castore" (kind k)
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

	| OpAdd k -> sprintf "%cadd" (kind k)
	| OpSub k -> sprintf "%csub" (kind k)
	| OpMult k -> sprintf "%cmult" (kind k)
	| OpDiv k -> sprintf "%cdiv" (kind k)
	| OpRem k -> sprintf "%crem" (kind k)
	| OpNeg k -> sprintf "%cneg" (kind k)

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
	| OpIfEq n -> sprintf "ifeq %d" n
	| OpIfNe n -> sprintf "ifne %d" n
	| OpIfLt n -> sprintf "iflt %d" n
	| OpIfGe n -> sprintf "ifge %d" n
	| OpIfGt n -> sprintf "ifgt %d" n
	| OpIfLe n -> sprintf "ifle %d" n
	| OpICmpEq n -> sprintf "ifcmpeq %d" n
	| OpICmpNe n -> sprintf "ifcmpne %d" n
	| OpICmpLt n -> sprintf "ifcmplt %d" n
	| OpICmpGe n -> sprintf "ifcmpge %d" n
	| OpICmpGt n -> sprintf "ifcmpgt %d" n
	| OpICmpLe n -> sprintf "ifcmpme %d" n
	| OpACmpEq n -> sprintf "ifacmpeq %d" n
	| OpACmpNe n -> sprintf "ifacmpne %d" n
	| OpGoto n -> sprintf "goto %d" n
	| OpJsr n -> sprintf "jsr %d" n
	| OpRet n -> sprintf "ret %d" n

	| OpTableSwitch (def,min,max,tbl) -> "tableswitch <...>"
	| OpLookupSwitch (def,pairs) -> "lookupswitch <...>"

	| OpReturn k -> sprintf "%creturn" (kind k)
	| OpAReturn -> "areturn"
	| OpReturnVoid -> "return"

	| OpGetStatic n -> sprintf "getstatic %d" n
	| OpPutStatic n -> sprintf "putstatic %d" n
	| OpPutField n -> sprintf "putfield %d" n
	| OpGetField n -> sprintf "getfield %d" n
	| OpInvokeVirtual n -> sprintf "invokevirtual %d" n
	| OpInvokeNonVirtual n -> sprintf "invokenonvirtual %d" n
	| OpInvokeStatic n -> sprintf "invokestatic %d" n
	| OpInvokeInterface (a,b) -> sprintf "invokeinterface %d %d" a b

	| OpNew n -> sprintf "new %d" n
	| OpNewArray t -> sprintf "newarray %s" (array_type t)
	| OpANewArray n -> sprintf "anewarray %d" n
	| OpArrayLength -> "arraylength"
	    (* Modified by eandre@irisa.fr 2006/06/08 *)
	| OpThrow -> "athrow"
	| OpCheckCast n -> sprintf "checkcast %d" n
	| OpInstanceOf n -> sprintf "instanceof %d" n
	| OpMonitorEnter -> "monitorenter"
	| OpMonitorExit -> "monitorexit"
	    (* Modified by eandre@irisa.fr 2006/05/19
	       because there was a (big) error *)
	| OpWide -> "wide"
	| OpAMultiNewArray (a,b) -> sprintf "amultinewarray %d %d" a b
	| OpIfNull n -> sprintf "ifnull %d" n
	| OpIfNonNull n -> sprintf "ifnonnull %d" n
	| OpGotoW n -> sprintf "gotow %d" n
	| OpJsrW n -> sprintf "jrsw %d" n
	| OpBreakpoint -> "breakpoint"
	| OpRetW n -> sprintf "retw %d" n

	| OpInvalid -> "invalid"

let dump_constant2 ch kind (cl,f,sign) =
	IO.printf ch "%s : %s" kind (signature (class_name cl ^ "::" ^ f) sign)

let dump_constant ch = function
	| ConstClass cl -> IO.printf ch "class %s" (class_name cl)
	| ConstField x -> dump_constant2 ch "field" x
	| ConstMethod x -> dump_constant2 ch "method" x
	| ConstInterfaceMethod x -> dump_constant2 ch "interface-method" x
	| ConstString s -> IO.printf ch "string '%s'" s
	| ConstInt i -> IO.printf ch "int %ld" i
	| ConstFloat f -> IO.printf ch "float %f" f
	| ConstLong i -> IO.printf ch "long %Ld" i
	| ConstDouble f -> IO.printf ch "double %f" f
	| ConstNameAndType (s,sign) -> IO.printf ch "name-and-type : %s" (signature s sign)
	| ConstStringUTF8 s -> IO.printf ch "utf8 %s" s
	| ConstUnusable -> IO.printf ch "unusable"


let dump_stackmap ch (offset,locals,stack) =
	let dump_verif_info = function
		| VTop -> "Top"
		| VInteger -> "Integer"
		| VFloat -> "Float"
		| VDouble -> "Double"
		| VLong -> "Long"
		| VNull -> "Null"
		| VUninitializedThis -> "UninitializedThis"
		| VObject cl -> sprintf "Object %d" cl
		| VUninitialized off -> sprintf "Uninitialized %d" off
	in
	IO.printf ch "\n      offset=%d,\n      locals=[" offset;
	List.iter (fun t -> IO.printf ch "\n        %s" (dump_verif_info t)) locals;
	IO.printf ch "],\n      stack=[";
	List.iter (fun t -> IO.printf ch "\n        %s" (dump_verif_info t)) stack

(* added by eandre@irisa.fr 2006/06/08 *)
let dump_exc ch cl exc =
  IO.printf ch "\n      [%d-%d] -> %d (" exc.e_start exc.e_end exc.e_handler;
  if exc.e_catch_type = 0 then
    IO.printf ch "<finally>"
  else
    dump_constant ch cl.j_consts.(exc.e_catch_type);
  IO.printf ch ")"

let rec dump_code ch cl code =
	IO.printf ch "max_stack = %d , max_locals = %d\n" code.c_max_stack code.c_max_locals;
	Array.iteri (fun i c ->
		match c with
		  | OpInvalid -> IO.printf ch "__\n"
		      (* modified by eandre@irisa.fr 2006/05/03 *)
		  | _ -> IO.printf ch "      %.4i (%.4X) %s\n" i i (opcode c)
	) code.c_code;
	List.iter (dump_attrib ch cl) code.c_attributes

and dump_attrib ch cl = function
	| AttributeSourceFile s -> IO.printf ch "    source = %s\n" s
	| AttributeConstant c -> IO.printf ch "    const "; dump_constant ch c; IO.printf ch "\n";
	| AttributeCode code -> IO.printf ch "    code "; dump_code ch cl code;
	    (* added by eandre@irisa.fr 2006/06/08 *)
	    IO.printf ch "    exceptions"; List.iter (dump_exc ch cl) code.c_exc_tbl
	| AttributeLineNumberTable lines -> IO.printf ch "    line-numbers\n"
	| AttributeStackMap stackmap_frames -> IO.printf ch "    stackmap = ["; List.iter (dump_stackmap ch) stackmap_frames; IO.printf ch "]\n";
	| AttributeUnknown (s,_) -> IO.printf ch "    ?%s\n" s

let dump_field ch cl f =
	IO.printf ch "  %s%s\n" (access_flags f.f_flags) (signature f.f_name f.f_signature);
	List.iter (dump_attrib ch cl) f.f_attributes

let dump_method ch cl m =
	IO.printf ch "  %s%s\n" (access_flags m.m_flags) (signature m.m_name m.m_signature);
	List.iter (dump_attrib ch cl) m.m_attributes;
	IO.write ch '\n'

let dump ch cl =
	IO.printf ch "%sclass %s\n" (access_flags cl.j_flags) (class_name cl.j_name);
	(match cl.j_super with None -> () | Some c -> IO.printf ch "  extends %s\n" (class_name c));
	if cl.j_interfaces <> [] then IO.printf ch "  implements %s\n" (String.concat " " (List.map class_name cl.j_interfaces));
	IO.printf ch "{\n\n";
	IO.printf ch "/* **** CONSTANTS ****\n";
	Array.iteri (fun i c ->
		IO.printf ch "    %d  " i;
		dump_constant ch c;
		IO.write ch '\n'
	) cl.j_consts;
	IO.printf ch "// ****************** */\n\n";	
	List.iter (dump_field ch cl) cl.j_fields;
	IO.printf ch "\n";
	List.iter (dump_method ch cl) cl.j_methods;
	IO.printf ch "}\n";
