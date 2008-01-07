(*
 *  This file is part of JavaLib
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

(** Pretty-printer for high level classes and programs. *)

open JBasics
open JClass

(** This pretty printer has two output formats: a text format and an
    HTML one.  The HTML format also offers the possibility to select
    dynamically the parts that are visible.

    Additionaly to the bytecode, this module can also display some
    other information associated to classes, fields, methods or
    program points.  Some data can also be filtered so as to output,
    for example, only the classes that have been identified to be
    accessible.  *)

(** This record represents the information that must be defined by the
    user of this module if he wants to print additional information or
    to filter some data.  *)
type info = {
  p_global : Format.formatter -> unit;
  (** Prints global information that is printed before any class when
      printing a program. *)
  p_class : class_name -> Format.formatter -> unit;
  (** Prints class information that is printed inside the class, along with
      other attributes of the class. *)
  p_field : class_name -> field_signature -> Format.formatter -> unit;
  (** Prints field information that is printed along with the corresponding
      field. *)
  p_method : class_name -> method_signature -> Format.formatter -> unit;
  (** Prints method information that is printed inside the method,
      along with other attributes of the method. *)
  p_pp : class_name -> method_signature -> int -> Format.formatter -> unit;
  (** Prints information associated to program points. The information is
      printed after the instruction. *)
  
  f_class : class_name -> bool;
  (** iff [f_class cn] returns true, the class is printed. *)
  f_field : class_name -> field_signature -> bool;
  (** iff [f_field cn fs] returns true, the field is printed. *)
  f_method : class_name -> method_signature -> bool;
  (** iff [f_method cn ms] returns true, the method is printed. *)
}

(** [void_info] is an instance of [info] that does not print anything
    nor filter anything. *)
val void_info : info

(** print an anchor in an HTML formatter (or nothing in a text
    formatter). *)
val cn2anchor : class_name -> Format.formatter -> unit
val fs2anchor : (class_name*field_signature) -> Format.formatter -> unit
val ms2anchor : (class_name*method_signature) -> Format.formatter -> unit

(** print the string a link in an HTML formatter (or just the string
    in a text formatter). *)
val cn2link : class_name -> Format.formatter -> string -> unit
val fs2link : (class_name*field_signature) -> Format.formatter -> string -> unit
val ms2link : (class_name*method_signature) -> Format.formatter -> string -> unit

(** {2 HTML printing functions} *)

val pprint_class_to_html_file :
  string -> info -> string -> interface_or_class -> unit
  (** [pprint_class_to_html_file intro info output c] prints the class
      [c] to the HTML file [output]. It copies the content of [intro]
      at the beginning of [output]. This file must at least open the
      tags [<html>] and [<body>] and it should defines the JavaScript
      functions to switch the visibility of the data. *)
val pprint_program_to_html_file :
  string -> info -> string -> JProgram.program -> unit
  (** [pprint_program_to_html_file] is similar to
      [pprint_class_to_html_file].  The only difference is that it prints
      a program.  *)

(** {2 Text printing functions} *)

val pprint_class : info -> Format.formatter -> interface_or_class -> unit
  (** [pprint_class info fmt c] prints the class [c] in the formatter
      [fmt] with the filtering and the additional information given in
      [info]. *)
val pprint_program : info -> Format.formatter -> JProgram.program -> unit
  (** [pprint_program info fmt p] prints the class [p] in the formatter
      [fmt] with the filtering and the additional information given in
      [info]. *)


(** {2 Some other useful functions}*)

(** [pp_concat f pp_open pp_close pp_sep l] do nothing if [l] is [],
    otherwise it prints [pp_open], iterates [f] on [l] printing
    [pp_sep] between each iteration, and finishes with [pp_close]. *)
val pp_concat :
  ('a -> unit) ->
  (unit -> unit) -> (unit -> unit) -> (unit -> unit) -> 'a list -> unit

val pp_field_signature : Format.formatter -> field_signature -> unit
  (** pretty prints a field signature.*)
val pp_method_signature : Format.formatter -> method_signature -> unit
  (** pretty prints a method signature.*)
