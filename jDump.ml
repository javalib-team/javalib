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

let basic_type = function
  | `Bool -> "bool"
  | `Char -> "char"
  | `Float -> "float"
  | `Double -> "double"
  | `Byte -> "byte"
  | `Short -> "short"
  | `Int -> "int"
  | `Long -> "long"

let rec object_value_signature name = function
	| TClass cl -> class_name cl ^ " " ^ name
	| TArray s ->
		value_signature name s ^ "[]"

and value_signature name = function
  | TBasic b -> basic_type b ^ name
  | TObject o -> object_value_signature name o

let method_signature name (sl,sr) =
		(match sr with
		| None -> "void " ^ name
		| Some s -> value_signature name s) ^ "(" ^ String.concat "," (List.map (value_signature "") sl) ^ ")"

let signature name = function
  | SValue v -> value_signature name v
  | SMethod m -> method_signature name m

let jvm_basic_type = function
	| `Int2Bool -> 'i'
	| `Long -> 'l'
	| `Float -> 'f'
	| `Double -> 'd'

let dump_constant_value ch = function
  | ConstString s -> IO.printf ch "string '%s'" s
  | ConstInt i -> IO.printf ch "int %ld" i
  | ConstFloat f -> IO.printf ch "float %f" f
  | ConstLong i -> IO.printf ch "long %Ld" i
  | ConstDouble f -> IO.printf ch "double %f" f
  | ConstClass cl -> IO.printf ch "class %s" (object_value_signature "" cl)

let dump_constant ch = function
  | ConstValue v -> dump_constant_value ch v
  | ConstField (cl,f,sign) -> IO.printf ch "field : %s"(value_signature (class_name cl ^ "::" ^ f) sign)
  | ConstMethod (cl,f,sign) -> IO.printf ch "method : %s"(method_signature (object_value_signature "" cl ^ "::" ^ f) sign)
  | ConstInterfaceMethod (cl,f,sign) -> IO.printf ch "interface-method : %s"(method_signature (class_name cl ^ "::" ^ f) sign)
  | ConstNameAndType (s,sign) -> IO.printf ch "name-and-type : %s" (signature s sign)
  | ConstStringUTF8 s -> IO.printf ch "utf8 %s" s
  | ConstUnusable -> IO.printf ch "unusable"

let opcode = function
	| OpNop -> "nop"
	| OpConst x ->
	    (match x with
	       | `ANull -> "aconstnull"
	       | `I i -> sprintf "iconst %ld" i
	       | `L i -> sprintf "lconst %Ld" i
	       | `F f -> sprintf "fconst %f" f
	       | `D f -> sprintf "dconst %f" f
	       | `B n -> sprintf "bipush %d" n
	       | `S a -> sprintf "sipush %d " a)
	    (* modified by eandre@irisa.fr 2006/05/02 *)
	| OpLdc n -> sprintf "ldc %s" (let s = IO.output_string () in dump_constant_value s n ; IO.close_out s)

	| OpLoad (k,n) ->
	    (match k with
	       | `Object -> sprintf "aload %d" n
	       | `Int2Bool | `Long | `Float | `Double as k -> sprintf "%cload %d" (jvm_basic_type k) n)

	| OpArrayLoad k ->
	    (match k with
	       | `Object -> "aaload"
	       | `ByteBool -> "baload"
	       | `Char -> "caload"
	       | `Short -> "saload"
	       | `Int -> sprintf "%caload" (jvm_basic_type `Int2Bool)
	       | `Long | `Float | `Double as k -> sprintf "%caload" (jvm_basic_type k))

	| OpStore (k,n) ->
	    (match k with
	       | `Object -> sprintf "astore %d" n
	       | `Int2Bool | `Long | `Float | `Double as k -> sprintf "%cstore %d" (jvm_basic_type k) n)

	| OpArrayStore k ->
	    (match k with
	       | `Object -> "aastore"
	       | `ByteBool -> "bastore"
	       | `Char -> "castore"
	       | `Short -> "sastore"
	       | `Int -> sprintf "%castore" (jvm_basic_type `Int2Bool)
	       | `Long | `Float | `Double as k -> sprintf "%castore" (jvm_basic_type k))

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

	| OpCmp x ->
	    (match x with
	       | `L -> "lcmp"
	       | `FL -> "fcmpl"
	       | `FG -> "fcmpg"
	       | `DL -> "dcmpl"
	       | `DG -> "dcmpg")
	| OpIf (x, n) ->
	    (match x with
		 `Eq -> sprintf "ifeq %d" n
	       | `Ne -> sprintf "ifne %d" n
	       | `Lt -> sprintf "iflt %d" n
	       | `Ge -> sprintf "ifge %d" n
	       | `Gt -> sprintf "ifgt %d" n
	       | `Le -> sprintf "ifle %d" n
	       | `Null -> sprintf "ifnull %d" n
	       | `NonNull -> sprintf "ifnonnull %d" n)
	| OpIfCmp (x, n) ->
	    (match x with
		 `IEq -> sprintf "ifcmpeq %d" n
	       | `INe -> sprintf "ifcmpne %d" n
	       | `ILt -> sprintf "ifcmplt %d" n
	       | `IGe -> sprintf "ifcmpge %d" n
	       | `IGt -> sprintf "ifcmpgt %d" n
	       | `ILe -> sprintf "ifcmpme %d" n
	       | `AEq -> sprintf "ifacmpeq %d" n
	       | `ANe -> sprintf "ifacmpne %d" n)
	| OpGoto n -> sprintf "goto %d" n
	| OpJsr n -> sprintf "jsr %d" n
	| OpRet n -> sprintf "ret %d" n

	| OpTableSwitch (def,min,max,tbl) -> "tableswitch <...>"
	| OpLookupSwitch (def,pairs) -> "lookupswitch <...>"

	| OpReturn k ->
	    (match k with
	       | `Object -> "areturn"
	       | `Void -> "return"
	       | `Int2Bool | `Long | `Float | `Double as k -> sprintf "%creturn" (jvm_basic_type k))

	| OpGetStatic (c, name, sign) -> sprintf "getstatic %s.%s:%s" (class_name c) name (value_signature "" sign)
	| OpPutStatic (c, name, sign) -> sprintf "putstatic %s.%s:%s" (class_name c) name (value_signature "" sign)
	| OpPutField (c, name, sign) -> sprintf "putfield %s.%s:%s" (class_name c) name (value_signature "" sign)
	| OpGetField (c, name, sign) -> sprintf "getfield %s.%s:%s" (class_name c) name (value_signature "" sign)
	| OpInvoke (x, name, sign) ->
	    (match x with
	       | `Virtual c -> sprintf "invokevirtual %s.%s:%s" (object_value_signature "" c) name (method_signature "" sign)
	       | `Special c -> sprintf "invokenonvirtual %s.%s:%s" (class_name c) name (method_signature "" sign)
	       | `Static c -> sprintf "invokestatic %s.%s:%s" (class_name c) name (method_signature "" sign)
	       | `Interface c -> sprintf "invokeinterface %s.%s:%s" (class_name c) name (method_signature "" sign))
	| OpNew c -> sprintf "new %s" (class_name c)
	| OpNewArray t ->
	    (match t with
	       | TBasic t -> sprintf "newarray %s" (basic_type t)
	       | TObject c -> sprintf "anewarray %s" (object_value_signature "" c))
	| OpArrayLength -> "arraylength"
	    (* Modified by eandre@irisa.fr 2006/06/08 *)
	| OpThrow -> "athrow"
	| OpCheckCast c -> sprintf "checkcast %s" (object_value_signature "" c)
	| OpInstanceOf c -> sprintf "instanceof %s" (object_value_signature "" c)
	| OpMonitorEnter -> "monitorenter"
	| OpMonitorExit -> "monitorexit"
	| OpAMultiNewArray (a,b) -> sprintf "amultinewarray %s %d" (object_value_signature "" a) b
	| OpBreakpoint -> "breakpoint"

	| OpInvalid -> "invalid"

let dump_stackmap ch (offset,locals,stack) =
	let dump_verif_info = function
		| VTop -> "Top"
		| VInteger -> "Integer"
		| VFloat -> "Float"
		| VDouble -> "Double"
		| VLong -> "Long"
		| VNull -> "Null"
		| VUninitializedThis -> "UninitializedThis"
		| VObject c -> sprintf "Object %s" (object_value_signature "" c)
		| VUninitialized off -> sprintf "Uninitialized %d" off
	in
	IO.printf ch "\n      offset=%d,\n      locals=[" offset;
	List.iter (fun t -> IO.printf ch "\n        %s" (dump_verif_info t)) locals;
	IO.printf ch "],\n      stack=[";
	List.iter (fun t -> IO.printf ch "\n        %s" (dump_verif_info t)) stack

(* added by eandre@irisa.fr 2006/06/08 *)
let dump_exc ch cl exc =
  IO.printf ch "\n      [%d-%d] -> %d (" exc.e_start exc.e_end exc.e_handler;
  (match exc.e_catch_type with
     | None -> IO.printf ch "<finally>"
     | Some cl -> IO.printf ch "class %s" (class_name cl));
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
	| AttributeConstant c -> IO.printf ch "    const "; dump_constant_value ch c; IO.printf ch "\n";
	| AttributeCode code -> IO.printf ch "    code "; dump_code ch cl code;
	    (* added by eandre@irisa.fr 2006/06/08 *)
	    IO.printf ch "    exceptions"; List.iter (dump_exc ch cl) code.c_exc_tbl
	| AttributeLineNumberTable lines -> IO.printf ch "    line-numbers\n"
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
	| AttributeStackMap stackmap_frames -> IO.printf ch "    stackmap = ["; List.iter (dump_stackmap ch) stackmap_frames; IO.printf ch "]\n";
	| AttributeUnknown (s,_) -> IO.printf ch "    ?%s\n" s

let dump_field ch cl f =
	IO.printf ch "  %s%s\n" (access_flags f.f_flags) (value_signature f.f_name f.f_signature);
	List.iter (dump_attrib ch cl) f.f_attributes

let dump_method ch cl m =
	IO.printf ch "  %s%s\n" (access_flags m.m_flags) (method_signature m.m_name m.m_signature);
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
