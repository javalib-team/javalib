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

(** Low level (un)parsing of bytecode instructions. *)

(** Parse a sequence of instructions of given size (in bytes) and
    returns an array of instructions. *)
val parse_code : IO.input -> int -> JClassLow.opcode array

(** Unparse a sequence of instructions. Provided constants are kept unchanged.
    Missing constant are added at the end of the constant pool if
    needed. *)
val unparse_code :
  'a IO.output -> JClassLow.opcode array -> unit

(**/**)

(* For testing. *)

val parse_full_opcode :
  IO.input -> (unit -> int) -> JClassLow.opcode
val unparse_instruction :
  'a IO.output -> (unit -> int) -> JClassLow.opcode -> unit
