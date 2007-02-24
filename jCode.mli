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

open ExtLib

exception Invalid_opcode of int

(* Modified by eandre@irisa.fr 2006/05/19
   in order to accept wide offsets

   The boolean indicates that this instruction was preceded by a
   wide and should be read accordingly. *)
val parse_opcode : int -> IO.input -> JClass.constant array -> bool -> JClass.opcode

(* For testing *)
val parse_instruction : IO.input -> (unit -> int ) -> JClass.constant array -> JClass.opcode

(* Parses a sequence of instructions of given size (in bytes) and
   returns an array of instructions. Each instruction is at the index
   corresponding to its absolute offset. The array is padded with the
   OpInvalid instruction. The absolute and relative offset that appear
   in the instructions are therefore valid positions in the array.
   OpInvalid may be interpreted as nop, or the direct successor of
   an instruction can alternatively by defined as the first following
   non-OpInvalid instruction. *)
val parse_code : IO.input -> JClass.constant array -> int -> JClass.opcode array
