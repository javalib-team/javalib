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

(* file modified by eandre@irisa.fr 2006/05/19 *)

open IO.BigEndian
open ExtList
open JClass
open JConsts

exception Invalid_opcode of int

let kind = function
	| 0 -> KInt
	| 1 -> KLong
	| 2 -> KFloat
	| 3 -> KDouble
	| _ -> assert false
	    

(* Added by eandre@irisa.fr 2006/05/19
   in order to accept wide offsets*)
let read_unsigned ch wide =
  if wide then read_ui16 ch else IO.read_byte ch


(* Added by eandre@irisa.fr 2006/05/19
   in order to accept wide offsets*)
let read_signed ch wide =
  if wide then read_i16 ch else IO.read_signed_byte ch


(* Modified by eandre@irisa.fr 2006/05/19
   in order to accept wide offsets*)
let parse_opcode op ch consts wide =
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
		OpBIPush (IO.read_byte ch)
	| 17 ->
(*		let b1 = IO.read_byte ch in
		let b2 = IO.read_byte ch in
		OpSIPush (b1,b2) *)
	    OpSIPush (read_i16 ch)
	| 18 ->
	    OpLdc1 (get_constant consts (IO.read_byte ch))
	| 19 ->
	    OpLdc1w (get_constant consts (read_ui16 ch))
	| 20 ->
	    OpLdc2w (get_constant consts (read_ui16 ch))
	(* ---- load ----------------------------------- *)
	| 21 | 22 | 23 | 24 ->
	    (* Modified by eandre@irisa.fr 2006/05/19
	       in order to accept wide offsets*)
		OpLoad (kind (op - 21),read_unsigned ch wide)
	| 25 ->
	    (* Modified by eandre@irisa.fr 2006/05/19
	       in order to accept wide offsets*)
		OpALoad (read_unsigned ch wide)
	| 26 | 27 | 28 | 29 ->
		OpLoad (KInt,op - 26)
	| 30 | 31 | 32 | 33 ->
		OpLoad (KLong,op - 30)
	| 34 | 35 | 36 | 37 ->
		OpLoad (KFloat,op - 34)
	| 38 | 39 | 40 | 41 ->
		OpLoad (KDouble,op - 38)
	| 42 | 43 | 44 | 45 ->
		OpALoad (op - 42)
	(* ---- array load ---------------------------- *)
	| 46 | 47 | 48 | 49 ->
		OpArrayLoad (kind (op - 46))
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
	    (* Modified by eandre@irisa.fr 2006/05/19
	       in order to accept wide offsets*)
		OpStore (kind (op - 54),read_unsigned ch wide)
	| 58 ->
	    (* Modified by eandre@irisa.fr 2006/05/19
	       in order to accept wide offsets*)
		OpAStore (read_unsigned ch wide)
	| 59 | 60 | 61 | 62 ->
		OpStore (KInt , op - 59)
	| 63 | 64 | 65 | 66 ->
		OpStore (KLong , op - 63)
	| 67 | 68 | 69 | 70 ->
		OpStore (KFloat , op - 67)
	| 71 | 72 | 73 | 74 ->
		OpStore (KDouble , op - 71)
	| 75 | 76 | 77 | 78 ->
		OpAStore (op - 75)
	(* ---- array store ---------------------------- *)
	| 79 | 80 | 81 | 82 ->
		OpArrayStore (kind (op - 79))
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
		OpAdd (kind (op - 96))
	| 100 | 101 | 102 | 103 ->
		OpSub (kind (op - 100))
	| 104 | 105 | 106 | 107 ->
		OpMult (kind (op - 104))
	| 108 | 109 | 110 | 111 ->
		OpDiv (kind (op - 108))
	| 112 | 113 | 114 | 115 ->
		OpRem (kind (op - 112))
	| 116 | 117 | 118 | 119 ->
		OpNeg (kind (op - 116))
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
	    (* Modified by eandre@irisa.fr 2006/05/19
	       in order to accept wide offsets*)
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
	    (* Modified by eandre@irisa.fr 2006/05/19
	       in order to accept wide offsets*)
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
		OpReturn (kind (op - 172))
	| 176 ->
		OpAReturn
	| 177 ->
		OpReturnVoid
	(* ---- OO ------------------------------------- *)
	| 178 ->
	    let c, f, s = get_field consts ch in
	      OpGetStatic (c, f, s)
	| 179 ->
	    let c, f, s = get_field consts ch in
	      OpPutStatic (c, f, s)
	| 180 ->
	    let c, f, s = get_field consts ch in
	      OpGetField (c, f, s)
	| 181 ->
	    let c, f, s = get_field consts ch in
	      OpPutField (c, f, s)
	| 182 ->
	    let c, m, s = get_method consts ch in
	      OpInvokeVirtual (c, m, s)
	| 183 ->
	    let c, m, s = get_method consts ch in
	      OpInvokeNonVirtual (c, m, s)
	| 184 ->
	    let c, m, s = get_method consts ch in
	      OpInvokeStatic (c, m, s)
	| 185 ->
	    let c, m, s = get_interface_method consts ch in
	    let nargs = IO.read_byte ch in
	    let _ = IO.read_byte ch in
	      OpInvokeInterface (c, m, s, nargs)
	(* ---- others --------------------------------- *)
	| 187 ->
		OpNew (get_class consts ch)
	| 188 ->
		OpNewArray (match IO.read_byte ch with
			| 4 -> ATBool
			| 5 -> ATChar
			| 6 -> ATFloat
			| 7 -> ATDouble
			| 8 -> ATByte
			| 9 -> ATShort
			| 10 -> ATInt
			| 11 -> ATLong
			| _ -> raise Exit)
	| 189 ->
		OpANewArray (get_class consts ch)
	| 190 ->
		OpArrayLength
	| 191 ->
		OpThrow
	| 192 ->
		OpCheckCast (get_class consts ch)
	| 193 ->
		OpInstanceOf (get_class consts ch)
	| 194 ->
		OpMonitorEnter
	| 195 ->
		OpMonitorExit
	| 196 ->
	    (* Modified by eandre@irisa.fr 2006/05/19
	       because there was a (big) error *)
	    if wide
	    then failwith "wide wide"
	    else OpWide
	| 197 ->
	    let c = get_class consts ch in
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
	| n ->
	    raise Exit


(* L'utilisation des wides est pas très claire. Lors du parsing,
   il vaudrait mieux mettre l'instruction au bon offset, et pas après. *)

(* Modified by eandre@irisa.fr 2006/05/19
   in order to accept wide offsets *)
let parse_code ch consts len =
  let ch , pos = IO.pos_in ch in
  let code = Array.create len OpInvalid in

  let rec step wide =
    let p = pos() in
    let op = IO.read_byte ch in
      if (op = 170 || op = 171) && (p + 1) mod 4 > 0 then ignore(IO.nread ch (4 - ((p + 1) mod 4)));
      try
	let my_code = parse_opcode op ch consts wide in
	  code.(p) <- my_code;
	  if my_code = OpWide then step true
      with
	  Exit -> raise (Invalid_opcode op)
  in

    while pos() < len do
      step false
    done;
    code
      
