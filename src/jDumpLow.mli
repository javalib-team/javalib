(*
 * This file is part of Javalib
 * Copyright (c)2004 Nicolas Cannasse
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008 Laurent Hubert (CNRS)
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

(** Prints data from {!JClassLow} to a provided output.*)

val opcode : JClassLow.opcode -> string

val dump_code :
  'a JLib.IO.output -> JBasics.constant array -> JClassLow.code -> unit

val dump_attrib :
  'a JLib.IO.output -> JBasics.constant array -> JClassLow.attribute -> unit

val access_flags : [< JClassLow.access_flag ] list -> string

val dump_field :
  'a JLib.IO.output -> JBasics.constant array -> JClassLow.jfield -> unit

val dump_method :
  'a JLib.IO.output -> JBasics.constant array -> JClassLow.jmethod -> unit

val dump : 'a JLib.IO.output -> JClassLow.jclass -> unit
