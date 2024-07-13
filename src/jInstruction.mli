(*
 * This file is part of Javalib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007 Laurent Hubert (CNRS)
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1, with the special exception on linking described in file
 * LICENSE.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)

(** Conversion between low and high level representations of instructions. *)

val opcodes2code :
  JBasics.constant array ->
  JBasics.bootstrap_method array ->
  JClassLow.opcode array ->
  JCode.jopcodes
(** Maps an arrray of low level bytecode to a high level code.
    Each instruction is at the index
    corresponding to its absolute offset. The array is padded with the
    OpInvalid instruction. The absolute and relative offset that appear
    in the instructions are therefore valid positions in the array.
    OpInvalid may be interpreted as nop, or the direct successor of
    an instruction can alternatively by defined as the first following
    non-OpInvalid instruction. *)

val code2opcodes :
  JBasics.constant JLib.DynArray.t ->
  JBasics.bootstrap_method JLib.DynArray.t ->
  JCode.jopcodes ->
  JClassLow.opcode array
(** Maps a high level code to a valid arrray of low level bytecode
    instructions. The distance between the offset of two successive
    non-OpInvalid instructions is assumed to be at least the length of the
    optimal coding for the first instruction (for example, iload_0 vs iload
    0). The opcode may be encoded with the non-optimal form to fill the
    available space.

    @raise Class_structure_error if the length of an opcode produced is greater
    than the available space (number of OpInvalid + 1) except if
    {!JBasics.set_permissive} has been called with [true].  Note that this
    will only be an issue when dumping the code to a class file.
*)

val opcode2instruction :
  JBasics.constant array ->
  JBasics.bootstrap_method array ->
  JClassLow.opcode ->
  JCode.jopcode
(** Low level to high level bytecode instruction. *)

val instruction2opcode :
  JBasics.constant JLib.DynArray.t ->
  JBasics.bootstrap_method JLib.DynArray.t ->
  int ->
  JCode.jopcode ->
  JClassLow.opcode
(** High level to low level bytecode instruction.

    [instruction2opcode consts length instr] tries to produce an
    opcode which could be unparsed in [length] byte(s).

    @raise JBasics.Class_structure_error if the class has not a valid structure.
*)
