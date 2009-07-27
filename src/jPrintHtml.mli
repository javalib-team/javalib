(*
 * This file is part of JavaLib
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
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

(** Pretty-Html-printer for high level programs. *)

open JBasics
open JClass
open JProgram

(** This module allows to generate a web site from high level programs.
    This web site can be used for any kind of visualisation or
    debbuging purposes. Annotations can be attached to the program and
    will be displayed properly (according to a given css). *)

(** This abstract type represents the information that will be printed. *)
type info

(** [void_info] is an instance of [info] that does not print anything
    nor filter anything. *)
val void_info : info

(** [get_program_info p_class p_field p_method p_pp] returns an instance of
    [info] given annotation functions [p_class], [p_field], [p_method] and
    [p_pp]. *)
val get_program_info :
  program ->
  (class_name_index -> string list) ->
  (class_name_index -> field_signature_index -> string list) ->
  (class_name_index -> method_signature_index -> string list) ->
  (class_name_index -> method_signature_index -> int -> string list) -> info

val css:string
val js:string

(** {2 HTML printing functions} *)

(** [pp_print_program_to_html_files ~css ~js program outputdir info]
    generates html files representing the program [p] in the output
    directory [outputdir], given the annotation information [info], an
    optional Cascading Style Sheet (CSS) [css] and an optional
    JavaCcript file [js].  If [css] or [js] is not provided, {!css}
    and {!js} are used when [css] or [js] is not provided.
*)
val pp_print_program_to_html_files :
  ?css:string -> ?js:string -> program -> string -> info -> unit
