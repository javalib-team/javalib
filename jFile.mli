(*
 *  This file is part of JavaLib
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

(** Accessing classes in files, directories and jar files. *)

(** {2 Loading a single class.} *)

(** The type of "compiled" class paths (jar files are opened for efficiency). *)
type class_path

(** Create a class path from a list of directories and jar files separated by [:]. *)
val class_path : string -> class_path

(** Close a class path. *)
val close_class_path : class_path -> unit

(** Parse a single class. This function does not check that the name of the parsed
    class is the same as the argument class name. *)
val get_class : class_path -> string -> JClass.interface_or_class

(** Same as {! get_class} with low level class files. *)
val get_class_low : class_path -> string -> JClassLow.jclass

(** [write_class outputdir c] writes the class [c] in the subdirectory of
    [outputdir] that correspond to the package name of [c]. *)
val write_class : string -> JClass.interface_or_class -> unit

(** Same as {! write_class} with low level class files. *)
val write_class_low : string -> JClassLow.jclass -> unit

(** {2 Reading/transforming a set of classes.} *)

(** The following functions search for class files in the following order :
    - [directories] is a list of directories separated by [:]. If a name can be
    found in some directory, subsequent directories are ignored.
    - If a name is the name of an existing directory, then every .class file
    inside this directory is read, and the search is over (even if the
    directory is empy).
    - Otherwise, if the name refers to an existing .class file (without the
    extension) then this file is read.
    - Otherwise, if the name ends in .jar and the file exists, it is assumed
    to be jar file and the class files inside are read.

    Dots in class and directory names are interpreted as / (but not for jar
    files). *)

(** [read directories f acc names] iterates [f] over all classes specified by
    [names]. [acc] is the initial accumulator value. *)
val read :
  string -> ('a -> JClass.interface_or_class -> 'a) -> 'a -> string list -> 'a

(** [transform directories outputdir f names] applies [f] to all classes specified
    by [names], writing the resulting classes in [outputdir]. Jar files are
    mapped to jar files, and the non-class files are kept unchanged in the
    resulting archive. *)
val transform :
  string -> string ->
  (JClass.interface_or_class -> JClass.interface_or_class) ->
  string list -> unit

(** Same as {! read} with low level class files. *)
val read_low : string -> ('a -> JClassLow.jclass -> 'a) -> 'a -> string list -> 'a

(** Same as {! transform} with low level class files. *)
val transform_low :
  string -> string -> (JClassLow.jclass -> JClassLow.jclass) -> string list -> unit
