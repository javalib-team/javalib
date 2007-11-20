(*
 *  This file is part of JavaLib
 *  Copyright (c)2004 Nicolas Cannasse
 *  Copyright (c)2007 Université de Rennes 1 / CNRS
 *  Tiphaine.Turpin@irisa.fr
 *  Laurent.Hubert@irisa.fr
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

val opcode : JClassLow.opcode -> string
val dump_code : 'a IO.output -> 'b -> JClassLow.jcode -> unit
val dump_attrib : 'a IO.output -> 'b -> JClassLow.attribute -> unit
val access_flags : JClassLow.access_flag list -> string
val dump_field : 'a IO.output -> 'b -> JClassLow.jfield -> unit
val dump_method : 'a IO.output -> 'b -> JClassLow.jmethod -> unit
val dump : 'a IO.output -> JClassLow.jclass -> unit
