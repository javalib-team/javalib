(*
 *  This file is part of JavaLib
 *  Copyright (c)2007 Université de Rennes 1 / CNRS
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
open JDumpBasics
open JClass
open JHigh2Low

let sprintf = Printf.sprintf

let opcode = function
  | OpNop -> "nop"
  | OpConst x ->
      (match x with
	| `ANull -> "aconstnull"
	| `Int i -> sprintf "iconst %ld" i
	| `Long i -> sprintf "lconst %Ld" i
	| `Float f -> sprintf "fconst %f" f
	| `Double f -> sprintf "dconst %f" f
	| `Byte n -> sprintf "bipush %d" n
	| `Short a -> sprintf "sipush %d " a
	| `Class c -> sprintf "ldc class %s" (object_value_signature "" c)
	| `String s -> sprintf "ldc string '%s'" s)

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

  | OpTableSwitch (_def,_min,_max,_tbl) -> "tableswitch <...>"
  | OpLookupSwitch (_def,_pairs) -> "lookupswitch <...>"

  | OpReturn k ->
      (match k with
	| `Object -> "areturn"
	| `Void -> "return"
	| `Int2Bool | `Long | `Float | `Double as k -> sprintf "%creturn" (jvm_basic_type k))

  | OpGetStatic (c, sign) -> sprintf "getstatic %s.%s:%s" (class_name c) sign.fs_name (value_signature "" sign.fs_type)
  | OpPutStatic (c, sign) -> sprintf "putstatic %s.%s:%s" (class_name c) sign.fs_name (value_signature "" sign.fs_type)
  | OpPutField (c, sign) -> sprintf "putfield %s.%s:%s" (class_name c) sign.fs_name (value_signature "" sign.fs_type)
  | OpGetField (c, sign) -> sprintf "getfield %s.%s:%s" (class_name c) sign.fs_name (value_signature "" sign.fs_type)
  | OpInvoke (x, sign) ->
      (match x with
	| `Virtual c -> sprintf "invokevirtual %s.%s:%s" (object_value_signature "" c) sign.ms_name (method_signature "" (sign.ms_parameters,sign.ms_return_type))
	| `Special c -> sprintf "invokenonvirtual %s.%s:%s" (class_name c) sign.ms_name (method_signature "" (sign.ms_parameters,sign.ms_return_type))
	| `Static c -> sprintf "invokestatic %s.%s:%s" (class_name c) sign.ms_name (method_signature "" (sign.ms_parameters,sign.ms_return_type))
	| `Interface c -> sprintf "invokeinterface %s.%s:%s" (class_name c) sign.ms_name (method_signature "" (sign.ms_parameters,sign.ms_return_type)))
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

let dump_code ch cl code =
  IO.printf ch "max_stack = %d , max_locals = %d\n" code.c_max_stack code.c_max_locals;
  Array.iteri (fun i c ->
    match c with
      | OpInvalid -> IO.printf ch "__\n"
      | _ -> IO.printf ch "      %.4i (%.4X) %s\n" i i (opcode c)
  ) code.c_code;
  IO.printf ch "    exceptions"; List.iter (dump_exc ch cl) code.c_exc_tbl;
  List.iter (function (s,_) -> IO.printf ch "    ?%s\n" s) code.c_attributes

let dump_cfield ch _c f =
  JDumpLow.dump_field ch () (h2l_cfield () f)

let dump_ifield ch _c f =
  JDumpLow.dump_field ch () (h2l_ifield () f)

let dump_cmethod ch consts m =
  JDumpLow.dump_method ch () (h2l_cmethod consts m)

let dump_amethod ch consts m =
  JDumpLow.dump_method ch () (h2l_amethod consts m)

let dump_acmethod ch consts = function
  | AbstractMethod m -> dump_amethod ch consts m
  | ConcreteMethod m -> dump_cmethod ch consts m

let dump_super ch = function
  | None -> ()
  | Some c -> IO.printf ch "  extends %s\n" (class_name c)

let dump ch c = JDumpLow.dump ch (high2low c)
