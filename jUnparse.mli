(*
 *  This file is part of JavaLib
 *  Copyright (c)2007 Université de Rennes 1
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

(* Unparses a class to a file. Provided constants are kept unchanged.
   Missing constant are added at the end of the constant pool if
   needed. The Code attribute for methods is assumed to be such that
   the distance between the offset of two successive non-OpInvalid
   instructions is at least the length of the optimal coding for
   the first instruction (for example, iload_0 vs iload 0). The code
   is padded with nops if this length is shorter. OpWides are
   ignored, i.e., an instruction preceded by an OpWide is written at
   the offset of the OpWide. *)
val unparse_class : 'a IO.output -> JClass.jclass -> unit

(* For testing: *)
val unparse_instruction : 'a IO.output -> JClass.constant DynArray.t -> (unit -> int) -> JClass.opcode -> unit
