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

(** This record represents the information that must be defined by the
    user of this module if he wants to print additional information or
    to filter some data.  *)
type info =
    { p_class : class_name_index -> string list;
      (** Prints class information that is printed inside the class, along with
	  other attributes of the class. *)
      p_field : class_name_index -> field_signature_index -> string list;
      (** Prints field information that is printed along with the corresponding
	  field. *)  
      p_method : class_name_index -> method_signature_index -> string list;
      (** Prints method information that is printed inside the method,
	  along with other attributes of the method. *)
      p_callers : class_name_index -> method_signature_index -> ClassMethSet.t;
      (** Prints information about the possible method callers. *)
      p_pp : class_name_index -> method_signature_index -> int -> string list;
      (** Prints information associated to program points. The information is
	  printed after the instruction. *)
    }

(** [void_info] is an instance of [info] that does not print anything
    nor filter anything. *)
val void_info : info

(** {2 HTML printing functions} *)

val pp_print_program_to_html_files :
  program -> string -> info -> string -> string -> unit