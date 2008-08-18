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

open IO
open IO.BigEndian
open ExtList
open JClassLow
open JBasics

(* Ops Parsing *)
(*******************)

let jvm_basic_type_of = function
	| 0 -> `Int2Bool
	| 1 -> `Long
	| 2 -> `Float
	| 3 -> `Double
	| _ -> assert false

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
		OpNop
	(* ---- push ----------------------------------- *)
	| 1 ->
		OpAConstNull
	| 2 ->
		OpIConst Int32.minus_one
	| 3 | 4 | 5 | 6 | 7 | 8 ->
		OpIConst (Int32.of_int (op - 3))
	| 9 ->
		OpLConst Int64.zero
	| 10 ->
		OpLConst Int64.one
	| 11 | 12 | 13 ->
		OpFConst (float_of_int (op - 11))
	| 14 ->
		OpDConst 0.
	| 15 ->
		OpDConst 1.
	| 16 ->
		OpBIPush (IO.read_signed_byte ch)
	| 17 ->
	    OpSIPush (read_i16 ch)
	| 18 ->
	    OpLdc1 (IO.read_byte ch)
	| 19 ->
	    OpLdc1w (read_ui16 ch)
	| 20 ->
	    OpLdc2w (read_ui16 ch)
	(* ---- load ----------------------------------- *)
	| 21 | 22 | 23 | 24 ->
		OpLoad (jvm_basic_type "load" (op - 21),read_unsigned ch wide)
	| 25 ->
		OpALoad (read_unsigned ch wide)
	| 26 | 27 | 28 | 29 ->
		OpLoad (`Int2Bool,op - 26)
	| 30 | 31 | 32 | 33 ->
		OpLoad (`Long,op - 30)
	| 34 | 35 | 36 | 37 ->
		OpLoad (`Float,op - 34)
	| 38 | 39 | 40 | 41 ->
		OpLoad (`Double,op - 38)
	| 42 | 43 | 44 | 45 ->
		OpALoad (op - 42)
	(* ---- array load ---------------------------- *)
	| 46 | 47 | 48 | 49 ->
		OpArrayLoad (jvm_basic_type' "arrayload" (op - 46))
	| 50 ->
		OpAALoad
	| 51 ->
		OpBALoad
	| 52 ->
		OpCALoad
	| 53 ->
		OpSALoad
	(* ---- store ----------------------------------- *)
	| 54 | 55 | 56 | 57 ->
		OpStore (jvm_basic_type "store" (op - 54),read_unsigned ch wide)
	| 58 ->
		OpAStore (read_unsigned ch wide)
	| 59 | 60 | 61 | 62 ->
		OpStore (`Int2Bool , op - 59)
	| 63 | 64 | 65 | 66 ->
		OpStore (`Long , op - 63)
	| 67 | 68 | 69 | 70 ->
		OpStore (`Float , op - 67)
	| 71 | 72 | 73 | 74 ->
		OpStore (`Double , op - 71)
	| 75 | 76 | 77 | 78 ->
		OpAStore (op - 75)
	(* ---- array store ---------------------------- *)
	| 79 | 80 | 81 | 82 ->
		OpArrayStore (jvm_basic_type' "arraystore" (op - 79))
	| 83 ->
		OpAAStore
	| 84 ->
		OpBAStore
	| 85 ->
		OpCAStore
	| 86 ->
		OpSAStore
	(* ---- stack ---------------------------------- *)
	| 87 ->
		OpPop
	| 88 ->
		OpPop2
	| 89 ->
		OpDup
	| 90 ->
		OpDupX1
	| 91 ->
		OpDupX2
	| 92 ->
		OpDup2
	| 93 ->
		OpDup2X1
	| 94 ->
		OpDup2X2
	| 95 ->
		OpSwap
	(* ---- arithmetics ---------------------------- *)
	| 96 | 97 | 98 | 99 ->
		OpAdd (jvm_basic_type_of (op - 96))
	| 100 | 101 | 102 | 103 ->
		OpSub (jvm_basic_type_of (op - 100))
	| 104 | 105 | 106 | 107 ->
		OpMult (jvm_basic_type_of (op - 104))
	| 108 | 109 | 110 | 111 ->
		OpDiv (jvm_basic_type_of (op - 108))
	| 112 | 113 | 114 | 115 ->
		OpRem (jvm_basic_type_of (op - 112))
	| 116 | 117 | 118 | 119 ->
		OpNeg (jvm_basic_type_of (op - 116))
	(* ---- logicals ------------------------------- *)
	| 120 ->
		OpIShl
	| 121 ->
		OpLShl
	| 122 ->
		OpIShr
	| 123 ->
		OpLShr
	| 124 ->
		OpIUShr
	| 125 ->
		OpLUShr
	| 126 ->
		OpIAnd
	| 127 ->
		OpLAnd
	| 128 ->
		OpIOr
	| 129 ->
		OpLOr
	| 130 ->
		OpIXor
	| 131 ->
		OpLXor
	(* ---- incr ----------------------------------- *)
	| 132 ->
		let idx = read_unsigned ch wide in
		let c = read_signed ch wide in
		OpIInc (idx,c)
	(* ---- conversions ---------------------------- *)
	| 133 ->
		OpI2L
	| 134 ->
		OpI2F
	| 135 ->
		OpI2D
	| 136 ->
		OpL2I
	| 137 ->
		OpL2F
	| 138 ->
		OpL2D
	| 139 ->
		OpF2I
	| 140 ->
		OpF2L
	| 141 ->
		OpF2D
	| 142 ->
		OpD2I
	| 143 ->
		OpD2L
	| 144 ->
		OpD2F
	| 145 ->
		OpI2B
	| 146 ->
		OpI2C
	| 147 ->
		OpI2S
	(* ---- jumps ---------------------------------- *)
	| 148 ->
		OpLCmp
	| 149 ->
		OpFCmpL
	| 150 ->
		OpFCmpG
	| 151 ->
		OpDCmpL
	| 152 ->
		OpDCmpG
	| 153 ->
		OpIfEq (read_i16 ch)
	| 154 ->
		OpIfNe (read_i16 ch)
	| 155 ->
		OpIfLt (read_i16 ch)
	| 156 ->
		OpIfGe (read_i16 ch)
	| 157 ->
		OpIfGt (read_i16 ch)
	| 158 ->
		OpIfLe (read_i16 ch)
	| 159 ->
		OpICmpEq (read_i16 ch)
	| 160 ->
		OpICmpNe (read_i16 ch)
	| 161 ->
		OpICmpLt (read_i16 ch)
	| 162 ->
		OpICmpGe (read_i16 ch)
	| 163 ->
		OpICmpGt (read_i16 ch)
	| 164 ->
		OpICmpLe (read_i16 ch)
	| 165 ->
		OpACmpEq (read_i16 ch)
	| 166 ->
		OpACmpNe (read_i16 ch)
	| 167 ->
		OpGoto (read_i16 ch)
	| 168 ->
		OpJsr (read_i16 ch)
	| 169 ->
		OpRet (read_unsigned ch wide)
	| 170 ->
		let def = read_i32 ch in
		let low = read_real_i32 ch in
		let high = read_real_i32 ch in
		let tbl = Array.init (Int32.to_int (Int32.sub high low) + 1) (fun _ -> read_i32 ch) in
		OpTableSwitch (def,low,high,tbl)
	| 171 ->
		let def = read_i32 ch in
		let npairs = read_i32 ch in
		let tbl = List.init npairs (fun _ ->
			let v = read_real_i32 ch in
			let j = read_i32 ch in
			v , j
		) in
		OpLookupSwitch (def,tbl)
	(* ---- returns --------------------------------- *)
	| 172 | 173 | 174 | 175 ->
		OpReturn (jvm_basic_type "return" (op - 172))
	| 176 ->
		OpAReturn
	| 177 ->
		OpReturnVoid
	(* ---- OO ------------------------------------- *)
	| 178 ->
	    OpGetStatic (read_ui16 ch)
	| 179 ->
	    OpPutStatic (read_ui16 ch)
	| 180 ->
	    OpGetField (read_ui16 ch)
	| 181 ->
	    OpPutField (read_ui16 ch)
	| 182 ->
	    OpInvokeVirtual (read_ui16 ch)
	| 183 ->
	    OpInvokeNonVirtual (read_ui16 ch)
	| 184 ->
	    OpInvokeStatic (read_ui16 ch)
	| 185 ->
	    let index = read_ui16 ch in
	    let nargs = IO.read_byte ch in
	    let _ = IO.read_byte ch in
	      OpInvokeInterface (index, nargs)
	(* ---- others --------------------------------- *)
	| 187 ->
		OpNew (read_ui16 ch)
	| 188 ->
		OpNewArray (match IO.read_byte ch with
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
		OpANewArray (read_ui16 ch)
	| 190 ->
		OpArrayLength
	| 191 ->
		OpThrow
	| 192 ->
		OpCheckCast (read_ui16 ch)
	| 193 ->
		OpInstanceOf (read_ui16 ch)
	| 194 ->
		OpMonitorEnter
	| 195 ->
		OpMonitorExit
	| 197 ->
	    let c = read_ui16 ch in
	    let dims = IO.read_byte ch in
	      OpAMultiNewArray (c,dims)
	| 198 ->
	    OpIfNull (read_i16 ch)
	| 199 ->
	    OpIfNonNull (read_i16 ch)

	| 200 ->
		OpGotoW (read_i32 ch)
	| 201 ->
		OpJsrW (read_i32 ch)
	| 202 ->
		OpBreakpoint
	| 209 ->
		OpRetW (read_ui16 ch)
	| _ ->
	    raise (Illegal_value (string_of_int op, "opcode"))

let parse_full_opcode ch pos =
  let p = pos() in
  let op = IO.read_byte ch in
    if op = 196
    then parse_opcode (IO.read_byte ch) ch true
    else (
      if (op = 170 || op = 171) && (p + 1) mod 4 > 0
      then ignore(IO.really_nread ch (4 - ((p + 1) mod 4)));
      parse_opcode op ch false
    )

let parse_code ch len =
  let ch , pos = IO.pos_in ch in
  let code = Array.create len OpInvalid in
    while pos() < len do
      let p = pos() in
	code.(p) <- parse_full_opcode ch pos
    done;
    code

(* Ops unparsing *)
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

exception Not_in_range

(* Instruction without arguments *)
let simple ch op =
  try
    write_ui8 ch
      (OpcodeMap.find op simple_table)
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
    | OpArrayLoad k -> (match k with `Int -> `Int2Bool | #other_num as k -> k), 46
    | OpArrayStore k -> (match k with `Int -> `Int2Bool | #other_num as k -> k), 79
    | OpAdd i -> i, 96
    | OpSub i -> i, 100
    | OpMult i -> i, 104
    | OpDiv i -> i, 108
    | OpRem i -> i, 112
    | OpNeg i -> i, 116
    | OpReturn k -> k, 172
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
    | OpLoad (_, value)
    | OpALoad value
    | OpStore (_, value)
    | OpAStore value -> value
    | _ -> raise Not_in_range
  in
    if value < 4
    then
      write_ui8 ch
	(value +
	   match instr with
	     | OpLoad (jvm_basic_type, _) -> 26 + 4 * int_of_jvm_basic_type jvm_basic_type
	     | OpALoad _ -> 42
	     | OpStore (jvm_basic_type, _) -> 59 + 4 * int_of_jvm_basic_type jvm_basic_type
	     | OpAStore _ -> 75
	     | _ -> assert false)
    else
      unparse_local_instruction ch
	(match instr with
	   | OpLoad (jvm_basic_type, _) -> 21 +  int_of_jvm_basic_type jvm_basic_type
	   | OpALoad _ -> 25
	   | OpStore (jvm_basic_type, _) -> 54 +  int_of_jvm_basic_type jvm_basic_type
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
    | _ -> raise Not_in_range
  in
    write_ui8 ch opcode;
    write_i16 ch i

(* Instructions with one 16 bits unsigned argument *)
let ui16 ch inst =
  let i, opcode = match inst with
    | OpLdc1w i -> i, 19
    | OpRetW i -> i, 209
    | OpNew i -> i, 187
    | OpANewArray i -> i, 189
    | OpCheckCast i -> i, 192
    | OpInstanceOf i -> i, 193
    | OpGetStatic i -> i, 178
    | OpPutStatic i -> i, 179
    | OpGetField i -> i, 180
    | OpPutField i -> i, 181
    | OpInvokeVirtual i -> i, 182
    | OpInvokeNonVirtual i -> i, 183
    | OpInvokeStatic i -> i, 184
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
  | OpIConst n -> assert (-1l <= n && n <= 5l);write_ui8 ch (3 + Int32.to_int n)
  | OpLConst n -> assert (0L=n || n=1L);write_ui8 ch (9 + Int64.to_int n)
  | OpFConst n -> assert (0.=n || n=1. || n=2.);write_ui8 ch (11 + int_of_float n)
  | OpDConst n -> assert (0.=n || n=1.);write_ui8 ch (14 + int_of_float n)
  | OpBIPush n ->
      write_ui8 ch 16;
      write_i8 ch n
  | OpLdc1 index ->
      write_ui8 ch 18;
      write_ui8 ch index
  | OpLdc2w index ->
      write_ui8 ch 20;
      write_ui16 ch index
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
  | OpInvokeInterface (index, nargs) ->
      write_ui8 ch 185;
      write_ui16 ch index;
      write_ui8 ch nargs;
      write_ui8 ch 0
  | OpNewArray at ->
      write_ui8 ch 188;
      write_ui8 ch (4 + ExtArray.Array.findi (( = ) at) basic_type)
  | OpAMultiNewArray (c, dims) ->
      write_ui8 ch 197;
      write_ui16 ch c;
      write_ui8 ch dims
  | OpGotoW i ->
      write_ui8 ch 200;
      write_i32 ch i
  | OpJsrW i ->
      write_ui8 ch 201;
      write_i32 ch i
  | OpInvalid -> ()
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
      (* On suppose que unparse_instruction n'Ã©crit rien pour OpInvalid *)
	 if not (opcode = OpInvalid || count () = i)
	 then raise (Class_structure_error "unparsing Badly alligned low level bytecode");
	 unparse_instruction ch count opcode)
      code;
    if not (count () = Array.length code)
    then raise (Class_structure_error "unparsing Badly alligned low level bytecode")
