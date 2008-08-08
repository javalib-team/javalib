(*
 *  This file is part of JavaLib
 *  Copyright (c)2004 Nicolas Cannasse
 *  Copyright (c)2007-2008 Université de Rennes 1 / CNRS
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

(** Convert some data from {!JBasics} to strings or print them on the
    provided output.  *)

open JBasics

val class_name : class_name -> string
  (** [class_name cn] returns the fully qualified class name as a
      string with dots between packages and between package name and
      class name. *)
val basic_type : java_basic_type -> string
  (** [basic_type t] returns the Java representation of the type [t]
      ({i e.g.} [basic_type `Int] returns "int"). *)
val object_value_signature : object_type -> string
  (** [object_value_signature s] return the Java representation of
      type [s]. *)
val value_signature : value_type -> string
  (** [value_signature s] return the Java representation of type
      [s]. *)
val type2shortstring : value_type -> string
  (** [type2shortstring t] return the JVM representation of type [t]
      ({i e.g.} [type2shortstring `Int] returns "I"). *)
val rettype2shortstring : value_type option -> string
  (** [rettype2shortstring t] return the JVM representation of the
      return type [t] ({i e.g.} [rettype2shortstring None] returns
      "V"). *)
val method_signature : string -> method_descriptor -> string
  (** [method_signature mn md], where [mn] is a method name and [md] a
      method descriptor, returns the method signature as in Java ({i
      e.g.} "bool equals(java.lang.Object)"). *)
val signature : string -> name_and_type -> string
  (** [signature name des] returns a string corresponding to the field
      or method [name] with the descriptor [des].  (See
      {!JDumpBasics.method_signature})*)
val jvm_basic_type :  [< `Int | jvm_basic_type ] -> char
  (** [jvm_basic_type t] returns the lowercase character that
      corresponds to the type [t]. *)
val java_basic_type : java_basic_type -> char
  (** [java_basic_type t] returns the lowercase character that
      corresponds to the type [t]. *)
val dump_constant_value : 'a IO.output -> constant_value -> unit
  (** [dump_constant_value ch cst] prints on [ch] the constant value
      [ch] preceded by its type ({i e.g} "int 3").*)
val dump_constant : 'a IO.output -> constant -> unit
  (** [dump_constant ch cst] prints on [ch] the constant pool constant
      [cst] of the. *)
val dump_constantpool : 'a IO.output -> constant array -> unit
  (** [dump_constantpool ch pool] print on [ch] the constant pool
      [pool].*)
val dump_stackmap :
  'a IO.output ->
  int * verification_type list * verification_type list ->
  unit
    (** [dump_stackmap ch sm] prints on [ch] the stackmap [sm]. *)
val dump_exc : 'a IO.output -> 'b -> exception_handler -> unit
  (** [dump_exc ch _ ex] prints on [ch] the exception handler
      declaration [ex].*)
