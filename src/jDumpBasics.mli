(*
 * This file is part of Javalib
 * Copyright (c)2004 Nicolas Cannasse
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
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

(** Convert some data from {!JBasics} to strings or print them on the
    provided output.  *)

open JBasics

val class_name : ?jvm:bool -> class_name -> string
(** [class_name ~jvm cn] returns the fully qualified class name as a
      string. If [jvm] is false (default value): with dots between
      packages and between package name and class name. If [jvm] is
      true returns the JVM representation. *)

val basic_type : ?jvm:bool -> java_basic_type -> string
(** [basic_type ~jvm t], if [jvm] is false (default value), returns the
      Java representation of the type [t] ({i e.g.} [basic_type `Int]
      returns "int"). If [jvm] is true: returns the JVM
      representation of type [t] ({i e.g.} [type2shortstring `Int]
      returns "I").*)

val object_value_signature : ?jvm:bool -> object_type -> string
(** [object_value_signature ~jvm s], if [jvm] is false (by
      default), returns the Java representation of type [s]. If [jvm]
      is true returns the JVM representation of type [s].*)

val value_signature : ?jvm:bool -> value_type -> string
(** [value_signature ~jvm s], if [jvm] is false (default value),
      returns the Java representation of the type [t]. If [jvm] is
      true: returns the JVM representation of type [t] *)

val type2shortstring : value_type -> string
(** @deprecated {!type2shortstring} is an alias for {!value_signature} [~jvm:true].*)

val rettype2shortstring : ?jvm:bool -> value_type option -> string
(** [rettype2shortstring t], if [jvm] is true (default value),
      returns the JVM representation of the return type [t] ({i e.g.}
      [rettype2shortstring None] returns "V"). If [jvm] is false
      returns the Java representation of type [t] ({i e.g.}
      [rettype2shortstring None] returns "void").*)

val arraytype2shortstring : jvm_array_type -> string
(** [arraytype2shortstring t] return the JVM representation of the
      array type [t] ({i e.g.} [arraytype2shortstring `Object] returns
      "A"). *)

val method_signature : ?jvm:bool -> string -> method_descriptor -> string
(** [method_signature mn md], where [mn] is a method name and [md] a
      method descriptor, returns the method signature as in Java ({i
      e.g.} "bool equals(java.lang.Object)"). *)

val signature : string -> descriptor -> string
(** [signature name des] returns a string corresponding to the field
      or method [name] with the descriptor [des].  (See
      {!JDumpBasics.method_signature})*)

val jvm_basic_type : [< `Int | jvm_basic_type ] -> char
(** [jvm_basic_type t] returns the lowercase character that
      corresponds to the type [t]. *)

val jvm_array_type : jvm_array_type -> char
(** [jvm_array_type t] returns the lowercase character that
      corresponds to the type [t]. *)

val java_basic_type : java_basic_type -> char
(** [java_basic_type t] returns the lowercase character that
      corresponds to the type [t]. *)

val method_handle_kind : JBasicsLow.method_handle_kind -> string
(** [method_handle_kind k] returns the string that corresponds
      to the method handle kind [k]. *)

val dump_constant : 'a JLib.IO.output -> constant -> unit
(** [dump_constant ch cst] prints on [ch] the constant pool constant
      [cst] of the. *)

val dump_bootstrap_argument : 'a JLib.IO.output -> bootstrap_argument -> unit
(** [dump_bootstrap_argument b] prints on [ch] the bootstrap argument [b]. *)

val dump_bootstrap_method : 'a JLib.IO.output -> bootstrap_method -> unit
(** [dump_bootstrap_method m] prints on [ch] the bootstrap method [m]. *)

val dump_constantpool : 'a JLib.IO.output -> constant array -> unit
(** [dump_constantpool ch pool] print on [ch] the constant pool
      [pool].*)

val dump_verification_type : verification_type -> string

val dump_stackmap : 'a JLib.IO.output -> stackmap_frame -> unit
(** [dump_stackmap ch sm] prints on [ch] the stackmap [sm]. *)

val dump_exc : 'a JLib.IO.output -> 'b -> JCode.exception_handler -> unit
(** [dump_exc ch _ ex] prints on [ch] the exception handler
      declaration [ex].*)

val constant_attribute : JClass.constant_attribute -> string
