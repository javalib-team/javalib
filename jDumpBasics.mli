(*
 *  This file is part of JavaLib
 *  Copyright (c)2004 Nicolas Cannasse
 *  Copyright (c)2007 Université de Rennes 1 / CNRS
 *  Tiphaine Turpin <first.last@irisa.fr>
 *  Laurent Hubert <first.last@irisa.fr>
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

val class_name : JBasics.class_name -> string
val basic_type :
  [< `Bool | `Byte | `Char | `Double | `Float | `Int | `Long | `Short ] ->
  string
val object_value_signature : JBasics.object_type -> string
val value_signature : JBasics.value_type -> string
val method_signature :
  string -> JBasics.value_type list * JBasics.value_type option -> string
val signature : string -> JBasics.name_and_type -> string
val jvm_basic_type : [< `Double | `Float | `Int | `Int2Bool | `Long ] -> char
val java_basic_type :
  [< `Bool | `Byte | `Char | `Double | `Float | `Int | `Long | `Short ] ->
  char
val dump_constant_value : 'a IO.output -> JBasics.constant_value -> unit
val dump_constant : 'a IO.output -> JBasics.constant -> unit
val dump_constantpool : 'a IO.output -> JBasics.constant array -> unit
val dump_stackmap :
  'a IO.output ->
  int * JBasics.verification_type list * JBasics.verification_type list ->
  unit
val dump_exc : 'a IO.output -> 'b -> JBasics.exception_handler -> unit
