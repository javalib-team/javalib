(*
 * This file is part of JavaLib
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
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

(** Allows to manipulate abstract data structures representing native
    methods object allocation and method calls. The module provides
    methods to store these data structures in files, to load and to
    merge them. *)

(** {2 Navigable hierarchy} *)

type jmethod = { m_type : string;
		 m_class : string;
		 m_name : string;
		 m_signature : string
	       }
    
module ClassSignatureSet : Set.S with type elt = string
  
module MethodSet : Set.S with type elt = jmethod
  
module MethodMap : Map.S with type key = jmethod
  
module StringMap : Map.S with type key = string
  
type native_method_info = { native_alloc : ClassSignatureSet.t;
			    native_calls : MethodSet.t }
    
type native_info = native_method_info MethodMap.t

(** abstract type representing information associated to native methods. *)
type t

(** converts from native_info to abstract type t *)
val make_t : native_info -> t

(** represents empty information. *)
val empty_info : t

(** [get_native_methods native_info] returns the list of native methods
    contained in [native_info]. A native method is represented by a triplet
    of string representing (method_class,method_name,method_signature). *)
val get_native_methods : t -> (string * string * string) list
  
(** [get_native_method_allocations m native_info] returns a list of classes
    signatures, for classes that are allocated in the native method [m]. *)
val get_native_method_allocations :
  (string * string * string) -> t -> string list
  
(** [get_native_method_calls m native_info] return a list of methods that
    are called from the native method [m]. *)
val get_native_method_calls :
  (string * string * string) -> t -> (string * string * string) list
  
(** [fprint_native_info native_info file] exports the representation of
    [native_info] in a file of path [file]. *)
val fprint_native_info : t -> string -> unit
  
(** [merge_native_info native_info1 native_info2] returns the result of
    merging both parameters. *)
val merge_native_info : t -> t -> t
  
(** idem that [merge_native_info] with file parameters *)
val merge_native_info_files : string -> string -> t
  
(** [parse_native_info_file infofile] builds an abstract representation
    from the file [infofile]. Files like [infofile] can be created by the
    function [fprint_native_info]. *)
val parse_native_info_file : string -> t
