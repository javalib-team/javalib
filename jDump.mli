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

(* Last modified by eandre@irisa.fr 2006/06/08 *)

(** Dumping of low level classes. *)

open JClassLow

(** Dump a whole class file. *)
val dump : 'a IO.output -> jclass -> unit

(** {2 Basic elements.} *)

val class_name : class_name -> string
val access_flags : access_flag list -> string
val signature : string -> signature -> string
val jvm_basic_type : jvm_basic_type -> char
val basic_type : java_basic_type -> string
val opcode : opcode -> string

(** {2 Larger parts.} *)

(* Replaced 'b by jclass 2006/06/08 *)
val dump_constant : 'a IO.output -> constant -> unit
val dump_constantpool : 'a IO.output -> constant array -> unit
val dump_super : 'a IO.output -> class_name option -> unit
val dump_code : 'a IO.output -> jclass -> jcode -> unit
val dump_attrib : 'a IO.output -> jclass -> attribute -> unit
val dump_field : 'a IO.output -> jclass -> jfield -> unit
val dump_method : 'a IO.output -> jclass -> jmethod -> unit
