(*
 * This file is part of Javalib
 * Copyright (c)2004 Nicolas Cannasse
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

(** Low level (un)parsing of bytecode instructions. *)

val parse_code : JLib.IO.input -> int -> JClassLow.opcode array
(** Parse a sequence of instructions of given size (in bytes) and
    returns an array of instructions. *)

val unparse_code : 'a JLib.IO.output -> JClassLow.opcode array -> unit
(** Unparse a sequence of instructions.

    @raise OpcodeLengthError if an opcode cannot be encoded in the allocated
    place.  *)

exception OpcodeLengthError of int * JClassLow.opcode
(** [OpcodeLengthError] takes as argument the opcode and the excepted
    length that cannot be matched by unparsing the opcode. *)

val parse_full_opcode : JLib.IO.input -> (unit -> int) -> JClassLow.opcode

val unparse_instruction :
  'a JLib.IO.output -> (unit -> int) -> int -> JClassLow.opcode -> unit
(** [unparse_instruction ch count length opcode] output on [ch] the
    opcode [opcode] in the [length] byte(s) format. E.g [OpLoad
    (`Int,1)] can be encoded on 4 bytes ([wide iload 0x0001]), 2 bytes
    ([iload 0x01]) or 1 byte ([iload_1]).

    @raise OpcodeLengthError if the length provided cannot be matched.*)
