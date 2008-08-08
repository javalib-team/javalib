(*
 *  This file is part of JavaLib
 *  Copyright (c)2007-2008 Université de Rennes 1
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

(** Conversion between low and high level representations of instructions. *)

(** Maps an arrray of low level bytecode to a high level code.
    Each instruction is at the index
    corresponding to its absolute offset. The array is padded with the
    OpInvalid instruction. The absolute and relative offset that appear
    in the instructions are therefore valid positions in the array.
    OpInvalid may be interpreted as nop, or the direct successor of
    an instruction can alternatively by defined as the first following
    non-OpInvalid instruction. *)
val opcodes2code : JBasics.constant array -> JClassLow.opcode array -> JClass.opcode array

(** Maps a high level code to a valid arrray of low level bytecode
    instructions. The distance between the offset of two successive
    non-OpInvalid instructions is assumed to be at least the length
    of the optimal coding for the first instruction (for example,
    iload_0 vs iload 0). The code is padded with nops if this length
    is shorter. Note that the constants refered to by
    a ldc-like instruction should be present in the provided constant
    pool at an index that is compatible with their length: if a wide
    form is needed but the instruction was read as a "simple" form,
    the traduction will fail. *)
val code2opcodes :
  JBasics.constant DynArray.t -> JClass.opcode array -> JClassLow.opcode array

(**/**)

(* For testing. *)

(** Low level to high level bytecode instruction. *)
val opcode2instruction : JBasics.constant array -> JClassLow.opcode -> JClass.opcode

(** High level to low level bytecode instruction. *)
val instruction2opcode : JBasics.constant DynArray.t -> JClass.opcode -> JClassLow.opcode
