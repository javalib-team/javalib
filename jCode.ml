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

open IO
open IO.BigEndian
open ExtList
open JClassLow
open JBasics

(* OpCodes Parsing *)
(*******************)

let jvm_basic_type_of = function
	| 0 -> `Int2Bool
	| 1 -> `Long
	| 2 -> `Float
	| 3 -> `Double
	| n -> assert false

let jvm_basic_type place = function
	| 0 -> `Int2Bool
	| 1 -> `Long
	| 2 -> `Float
	| 3 -> `Double
	| n -> raise (Illegal_value (string_of_int n, "type of " ^ place))

let jvm_basic_type' place = function
	| 0 -> `Int
	| 1 -> `Long
	| 2 -> `Float
	| 3 -> `Double
	| n -> raise (Illegal_value (string_of_int n, "type of " ^ place))

let read_unsigned ch wide =
  if wide then read_ui16 ch else IO.read_byte ch

let read_signed ch wide =
  if wide then read_i16 ch else IO.read_signed_byte ch

let parse_opcode op ch wide =
  match op with
	| 0 ->
		OpCodeNop
	(* ---- push ----------------------------------- *)
	| 1 ->
		OpCodeAConstNull
	| 2 ->
		OpCodeIConst Int32.minus_one
	| 3 | 4 | 5 | 6 | 7 | 8 ->
		OpCodeIConst (Int32.of_int (op - 3))
	| 9 ->
		OpCodeLConst Int64.zero
	| 10 ->
		OpCodeLConst Int64.one
	| 11 | 12 | 13 ->
		OpCodeFConst (float_of_int (op - 11))
	| 14 ->
		OpCodeDConst 0.
	| 15 ->
		OpCodeDConst 1.
	| 16 ->
		OpCodeBIPush (IO.read_byte ch)
	| 17 ->
(*		let b1 = IO.read_byte ch in
		let b2 = IO.read_byte ch in
		OpCodeSIPush (b1,b2) *)
	    OpCodeSIPush (read_i16 ch)
	| 18 ->
	    OpCodeLdc1 (IO.read_byte ch)
	| 19 ->
	    OpCodeLdc1w (read_ui16 ch)
	| 20 ->
	    OpCodeLdc2w (read_ui16 ch)
	(* ---- load ----------------------------------- *)
	| 21 | 22 | 23 | 24 ->
		OpCodeLoad (jvm_basic_type "load" (op - 21),read_unsigned ch wide)
	| 25 ->
		OpCodeALoad (read_unsigned ch wide)
	| 26 | 27 | 28 | 29 ->
		OpCodeLoad (`Int2Bool,op - 26)
	| 30 | 31 | 32 | 33 ->
		OpCodeLoad (`Long,op - 30)
	| 34 | 35 | 36 | 37 ->
		OpCodeLoad (`Float,op - 34)
	| 38 | 39 | 40 | 41 ->
		OpCodeLoad (`Double,op - 38)
	| 42 | 43 | 44 | 45 ->
		OpCodeALoad (op - 42)
	(* ---- array load ---------------------------- *)
	| 46 | 47 | 48 | 49 ->
		OpCodeArrayLoad (jvm_basic_type' "arrayload" (op - 46))
	| 50 ->
		OpCodeAALoad
	| 51 ->
		OpCodeBALoad
	| 52 ->
		OpCodeCALoad
	| 53 ->
		OpCodeSALoad		
	(* ---- store ----------------------------------- *)
	| 54 | 55 | 56 | 57 ->
		OpCodeStore (jvm_basic_type "store" (op - 54),read_unsigned ch wide)
	| 58 ->
		OpCodeAStore (read_unsigned ch wide)
	| 59 | 60 | 61 | 62 ->
		OpCodeStore (`Int2Bool , op - 59)
	| 63 | 64 | 65 | 66 ->
		OpCodeStore (`Long , op - 63)
	| 67 | 68 | 69 | 70 ->
		OpCodeStore (`Float , op - 67)
	| 71 | 72 | 73 | 74 ->
		OpCodeStore (`Double , op - 71)
	| 75 | 76 | 77 | 78 ->
		OpCodeAStore (op - 75)
	(* ---- array store ---------------------------- *)
	| 79 | 80 | 81 | 82 ->
		OpCodeArrayStore (jvm_basic_type' "arraystore" (op - 79))
	| 83 ->
		OpCodeAAStore
	| 84 ->
		OpCodeBAStore
	| 85 ->
		OpCodeCAStore
	| 86 ->
		OpCodeSAStore
	(* ---- stack ---------------------------------- *)
	| 87 ->
		OpCodePop
	| 88 ->
		OpCodePop2
	| 89 ->
		OpCodeDup
	| 90 ->
		OpCodeDupX1
	| 91 ->
		OpCodeDupX2
	| 92 ->
		OpCodeDup2
	| 93 ->
		OpCodeDup2X1
	| 94 ->
		OpCodeDup2X2
	| 95 ->
		OpCodeSwap
	(* ---- arithmetics ---------------------------- *)
	| 96 | 97 | 98 | 99 ->
		OpCodeAdd (jvm_basic_type_of (op - 96))
	| 100 | 101 | 102 | 103 ->
		OpCodeSub (jvm_basic_type_of (op - 100))
	| 104 | 105 | 106 | 107 ->
		OpCodeMult (jvm_basic_type_of (op - 104))
	| 108 | 109 | 110 | 111 ->
		OpCodeDiv (jvm_basic_type_of (op - 108))
	| 112 | 113 | 114 | 115 ->
		OpCodeRem (jvm_basic_type_of (op - 112))
	| 116 | 117 | 118 | 119 ->
		OpCodeNeg (jvm_basic_type_of (op - 116))
	(* ---- logicals ------------------------------- *)
	| 120 ->
		OpCodeIShl 
	| 121 ->
		OpCodeLShl
	| 122 ->
		OpCodeIShr
	| 123 ->
		OpCodeLShr
	| 124 ->
		OpCodeIUShr
	| 125 ->
		OpCodeLUShr
	| 126 ->
		OpCodeIAnd
	| 127 ->
		OpCodeLAnd
	| 128 ->
		OpCodeIOr
	| 129 ->
		OpCodeLOr
	| 130 ->
		OpCodeIXor
	| 131 ->
		OpCodeLXor
	(* ---- incr ----------------------------------- *)
	| 132 ->
		let idx = read_unsigned ch wide in
		let c = read_signed ch wide in
		OpCodeIInc (idx,c)
	(* ---- conversions ---------------------------- *)
	| 133 ->
		OpCodeI2L 
	| 134 ->
		OpCodeI2F
	| 135 ->
		OpCodeI2D
	| 136 ->
		OpCodeL2I
	| 137 ->
		OpCodeL2F
	| 138 ->
		OpCodeL2D
	| 139 ->
		OpCodeF2I
	| 140 ->
		OpCodeF2L
	| 141 ->
		OpCodeF2D
	| 142 ->
		OpCodeD2I
	| 143 ->
		OpCodeD2L
	| 144 ->
		OpCodeD2F
	| 145 ->
		OpCodeI2B
	| 146 ->
		OpCodeI2C
	| 147 ->
		OpCodeI2S
	(* ---- jumps ---------------------------------- *)
	| 148 ->
		OpCodeLCmp
	| 149 ->
		OpCodeFCmpL
	| 150 ->
		OpCodeFCmpG
	| 151 ->
		OpCodeDCmpL
	| 152 ->
		OpCodeDCmpG
	| 153 ->
		OpCodeIfEq (read_i16 ch)
	| 154 ->
		OpCodeIfNe (read_i16 ch)
	| 155 ->
		OpCodeIfLt (read_i16 ch)
	| 156 ->
		OpCodeIfGe (read_i16 ch)
	| 157 ->
		OpCodeIfGt (read_i16 ch)
	| 158 ->
		OpCodeIfLe (read_i16 ch)
	| 159 ->
		OpCodeICmpEq (read_i16 ch)
	| 160 ->
		OpCodeICmpNe (read_i16 ch)
	| 161 ->
		OpCodeICmpLt (read_i16 ch)
	| 162 ->
		OpCodeICmpGe (read_i16 ch)
	| 163 ->
		OpCodeICmpGt (read_i16 ch)
	| 164 ->
		OpCodeICmpLe (read_i16 ch)
	| 165 ->
		OpCodeACmpEq (read_i16 ch)
	| 166 ->
		OpCodeACmpNe (read_i16 ch)
	| 167 ->
		OpCodeGoto (read_i16 ch)
	| 168 ->
		OpCodeJsr (read_i16 ch)
	| 169 ->
		OpCodeRet (read_unsigned ch wide)
	| 170 ->
		let def = read_i32 ch in
		let low = read_real_i32 ch in
		let high = read_real_i32 ch in
		let tbl = Array.init (Int32.to_int (Int32.sub high low) + 1) (fun _ -> read_i32 ch) in
		OpCodeTableSwitch (def,low,high,tbl)
	| 171 ->
		let def = read_i32 ch in
		let npairs = read_i32 ch in
		let tbl = List.init npairs (fun _ ->
			let v = read_real_i32 ch in
			let j = read_i32 ch in
			v , j
		) in
		OpCodeLookupSwitch (def,tbl)
	(* ---- returns --------------------------------- *)
	| 172 | 173 | 174 | 175 ->
		OpCodeReturn (jvm_basic_type "return" (op - 172))
	| 176 ->
		OpCodeAReturn
	| 177 ->
		OpCodeReturnVoid
	(* ---- OO ------------------------------------- *)
	| 178 ->
	    OpCodeGetStatic (read_ui16 ch)
	| 179 ->
	    OpCodePutStatic (read_ui16 ch)
	| 180 ->
	    OpCodeGetField (read_ui16 ch)
	| 181 ->
	    OpCodePutField (read_ui16 ch)
	| 182 ->
	    OpCodeInvokeVirtual (read_ui16 ch)
	| 183 ->
	    OpCodeInvokeNonVirtual (read_ui16 ch)
	| 184 ->
	    OpCodeInvokeStatic (read_ui16 ch)
	| 185 ->
	    let index = read_ui16 ch in
	    let nargs = IO.read_byte ch in
	    let _ = IO.read_byte ch in
	      OpCodeInvokeInterface (index, nargs)
	(* ---- others --------------------------------- *)
	| 187 ->
		OpCodeNew (read_ui16 ch)
	| 188 ->
		OpCodeNewArray (match IO.read_byte ch with
			| 4 -> `Bool
			| 5 -> `Char
			| 6 -> `Float
			| 7 -> `Double
			| 8 -> `Byte
			| 9 -> `Short
			| 10 -> `Int
			| 11 -> `Long
			| n -> raise (Illegal_value (string_of_int n, "type of newarray")))
	| 189 ->
		OpCodeANewArray (read_ui16 ch)
	| 190 ->
		OpCodeArrayLength
	| 191 ->
		OpCodeThrow
	| 192 ->
		OpCodeCheckCast (read_ui16 ch)
	| 193 ->
		OpCodeInstanceOf (read_ui16 ch)
	| 194 ->
		OpCodeMonitorEnter
	| 195 ->
		OpCodeMonitorExit
	| 197 ->
	    let c = read_ui16 ch in
	    let dims = IO.read_byte ch in
	      OpCodeAMultiNewArray (c,dims)
	| 198 -> 
	    OpCodeIfNull (read_i16 ch)
	| 199 -> 
	    OpCodeIfNonNull (read_i16 ch)

	| 200 ->
		OpCodeGotoW (read_i32 ch)
	| 201 ->
		OpCodeJsrW (read_i32 ch)
	| 202 ->
		OpCodeBreakpoint
	| 209 ->
		OpCodeRetW (read_ui16 ch)
	| _ ->
	    raise (Illegal_value (string_of_int op, "opcode"))

let parse_full_opcode ch pos =
  let p = pos() in
  let op = IO.read_byte ch in
    if op = 196
    then parse_opcode (IO.read_byte ch) ch true
    else (
      if (op = 170 || op = 171) && (p + 1) mod 4 > 0
      then ignore(IO.nread ch (4 - ((p + 1) mod 4)));
      parse_opcode op ch false
    )

let parse_code ch len =
  let ch , pos = IO.pos_in ch in
  let code = Array.create len OpCodeInvalid in
    while pos() < len do
      let p = pos() in
	code.(p) <- parse_full_opcode ch pos
    done;
    code

(* OpCodes unparsing *)
(**************************)

module OpCodecodeMap =
  Map.Make(struct type t = opcode let compare = compare end)

let simple_table =
  List.fold_left
    (fun m (offset, opcodes) ->
       fst
	 (Array.fold_left
	    (fun (m, code) op ->
	       OpCodecodeMap.add op code m, succ code)
	    (m, offset)
	    opcodes))
    OpCodecodeMap.empty
    [
      050, [|OpCodeAALoad;
	     OpCodeBALoad;
	     OpCodeCALoad;
	     OpCodeSALoad
	   |];
      083, [|OpCodeAAStore;
	     OpCodeBAStore;
	     OpCodeCAStore;
	     OpCodeSAStore
	   |];
      087, [|OpCodePop;
	     OpCodePop2;
	     OpCodeDup;
	     OpCodeDupX1;
	     OpCodeDupX2;
	     OpCodeDup2;
	     OpCodeDup2X1;
	     OpCodeDup2X2;
	     OpCodeSwap
	   |];
      120, [|OpCodeIShl;
	     OpCodeLShl;
	     OpCodeIShr;
	     OpCodeLShr;
	     OpCodeIUShr;
	     OpCodeLUShr;
	     OpCodeIAnd;
	     OpCodeLAnd;
	     OpCodeIOr;
	     OpCodeLOr;
	     OpCodeIXor;
	     OpCodeLXor
	   |];
      133, [|OpCodeI2L;
	     OpCodeI2F;
	     OpCodeI2D;
	     OpCodeL2I;
	     OpCodeL2F;
	     OpCodeL2D;
	     OpCodeF2I;
	     OpCodeF2L;
	     OpCodeF2D;
	     OpCodeD2I;
	     OpCodeD2L;
	     OpCodeD2F;
	     OpCodeI2B;
	     OpCodeI2C;
	     OpCodeI2S
	   |];
      148, [|OpCodeLCmp;
	     OpCodeFCmpL;
	     OpCodeFCmpG;
	     OpCodeDCmpL;
	     OpCodeDCmpG
	   |];
      000, [|OpCodeNop;
	     OpCodeAConstNull
	   |];
      176, [|OpCodeAReturn;
	     OpCodeReturnVoid
	   |];
      190, [|OpCodeArrayLength;
	     OpCodeThrow
	   |];
      194, [|OpCodeMonitorEnter;
	     OpCodeMonitorExit
	   |];
      202, [|OpCodeBreakpoint|]
    ]

exception Not_in_range

(* Instruction without arguments *)
let simple ch op =
  try
    write_ui8 ch
      (OpCodecodeMap.find op simple_table)
  with
      Not_found -> raise Not_in_range

let int_of_jvm_basic_type = function
  | `Int2Bool -> 0
  | `Long -> 1
  | `Float -> 2
  | `Double -> 3

(* Instructions with a jvm_basic_type argument added to the base opcode. *)
let jvm_basic_type ch inst =
  let jvm_basic_type, opcode = match inst with
    | OpCodeArrayLoad k -> (match k with `Int -> `Int2Bool | #other_num as k -> k), 46
    | OpCodeArrayStore k -> (match k with `Int -> `Int2Bool | #other_num as k -> k), 79
    | OpCodeAdd i -> i, 96
    | OpCodeSub i -> i, 100
    | OpCodeMult i -> i, 104
    | OpCodeDiv i -> i, 108
    | OpCodeRem i -> i, 112
    | OpCodeNeg i -> i, 116
    | OpCodeReturn k -> k, 172
    | _ -> raise Not_in_range
  in
    write_ui8 ch (opcode + int_of_jvm_basic_type jvm_basic_type)

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
    | OpCodeLoad (_, value)
    | OpCodeALoad value
    | OpCodeStore (_, value)
    | OpCodeAStore value -> value
    | _ -> raise Not_in_range
  in
    if value < 4
    then
      write_ui8 ch
	(value +
	   match instr with
	     | OpCodeLoad (jvm_basic_type, _) -> 26 + 4 * int_of_jvm_basic_type jvm_basic_type
	     | OpCodeALoad _ -> 42
	     | OpCodeStore (jvm_basic_type, _) -> 59 + 4 * int_of_jvm_basic_type jvm_basic_type
	     | OpCodeAStore _ -> 75
	     | _ -> assert false)
    else
      unparse_local_instruction ch
	(match instr with
	   | OpCodeLoad (jvm_basic_type, _) -> 21 +  int_of_jvm_basic_type jvm_basic_type
	   | OpCodeALoad _ -> 25
	   | OpCodeStore (jvm_basic_type, _) -> 54 +  int_of_jvm_basic_type jvm_basic_type
	   | OpCodeAStore _ -> 58
	   | _ -> assert false)
	value

(* Instructions with one 16 bits signed argument *)
let i16 ch inst =
  let i, opcode = match inst with
    | OpCodeSIPush i -> i, 17
    | OpCodeIfEq i -> i, 153
    | OpCodeIfNe i -> i, 154
    | OpCodeIfLt i -> i, 155
    | OpCodeIfGe i -> i, 156
    | OpCodeIfGt i -> i, 157
    | OpCodeIfLe i -> i, 158
    | OpCodeICmpEq i -> i, 159
    | OpCodeICmpNe i -> i, 160
    | OpCodeICmpLt i -> i, 161
    | OpCodeICmpGe i -> i, 162
    | OpCodeICmpGt i -> i, 163
    | OpCodeICmpLe i -> i, 164
    | OpCodeACmpEq i -> i, 165
    | OpCodeACmpNe i -> i, 166
    | OpCodeGoto i -> i, 167
    | OpCodeJsr i -> i, 168
    | OpCodeIfNull i -> i, 198
    | OpCodeIfNonNull i -> i, 199
    | _ -> raise Not_in_range
  in
    write_ui8 ch opcode;
    write_i16 ch i

(* Instructions with one 16 bits unsigned argument *)
let ui16 ch inst =
  let i, opcode = match inst with
    | OpCodeLdc1w i -> i, 19
    | OpCodeRetW i -> i, 209
    | OpCodeNew i -> i, 187
    | OpCodeANewArray i -> i, 189
    | OpCodeCheckCast i -> i, 192
    | OpCodeInstanceOf i -> i, 193
    | OpCodeGetStatic i -> i, 178
    | OpCodePutStatic i -> i, 179
    | OpCodeGetField i -> i, 180
    | OpCodePutField i -> i, 181
    | OpCodeInvokeVirtual i -> i, 182
    | OpCodeInvokeNonVirtual i -> i, 183
    | OpCodeInvokeStatic i -> i, 184
    | _ -> raise Not_in_range
  in
    write_ui8 ch opcode;
    write_ui16 ch i

let basic_type = [|
  `Bool;
  `Char;
  `Float;
  `Double;
  `Byte;
  `Short;
  `Int;
  `Long
|]

let padding ch count =
  flush ch;
  for i = 1 + (count () - 1) mod 4 to 3 do
    write_ui8 ch 0
  done

(* Everything else *)
let other count ch = function
  | OpCodeIConst n -> write_ui8 ch (3 + Int32.to_int n)
  | OpCodeLConst n -> write_ui8 ch (9 + Int64.to_int n)
  | OpCodeFConst n -> write_ui8 ch (11 + int_of_float n)
  | OpCodeDConst n -> write_ui8 ch (14 + int_of_float n)
  | OpCodeBIPush n ->
      write_ui8 ch 16;
      write_ui8 ch n
  | OpCodeLdc1 index ->
      write_ui8 ch 18;
      write_ui8 ch index
  | OpCodeLdc2w index ->
      write_ui8 ch 20;
      write_ui16 ch index
  | OpCodeIInc (index, incr) ->
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
  | OpCodeRet pc ->
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
  | OpCodeTableSwitch (def, low, high, tbl) ->
      write_ui8 ch 170;
      padding ch count;
      write_i32 ch def;
      write_real_i32 ch low;
      write_real_i32 ch high;
      Array.iter (write_i32 ch) tbl
  | OpCodeLookupSwitch (def, tbl) ->
      write_ui8 ch 171;
      padding ch count;
      write_i32 ch def;
      write_with_size write_i32 ch
	(function v, j ->
	   write_real_i32 ch v;
	   write_i32 ch j)
	tbl
  | OpCodeInvokeInterface (index, nargs) ->
      write_ui8 ch 185;
      write_ui16 ch index;
      write_ui8 ch nargs;
      write_ui8 ch 0
  | OpCodeNewArray at ->
      write_ui8 ch 188;
      write_ui8 ch (4 + ExtArray.Array.findi (( = ) at) basic_type)
  | OpCodeAMultiNewArray (c, dims) ->
      write_ui8 ch 197;
      write_ui16 ch c;
      write_ui8 ch dims
  | OpCodeGotoW i ->
      write_ui8 ch 200;
      write_i32 ch i
  | OpCodeJsrW i ->
      write_ui8 ch 201;
      write_i32 ch i
  | OpCodeInvalid -> ()
  | _ -> raise Not_in_range

let unparse_instruction ch count inst =
  try
    List.iter
      (function unparse ->
	 try
	   unparse ch inst;
	   raise Exit
	 with
	     Not_in_range -> ())
      [
	simple;
	jvm_basic_type;
	ilfda_loadstore;
	i16;
	ui16;
	other count
      ];
    assert false
  with
      Exit -> ()

let unparse_code ch code =
  let ch, count = pos_out ch in
    Array.iteri
      (fun i opcode ->
      (* On suppose que unparse_instruction n'écrit rien pour OpInvalid *)
	 if not (opcode = OpCodeInvalid || count () = i)
	 then raise Bad_allignement_of_low_level_bytecode;
	 unparse_instruction ch count opcode)
      code;
    if not (count () = Array.length code)
    then raise Bad_allignement_of_low_level_bytecode
