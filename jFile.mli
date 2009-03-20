(*
 * This file is part of JavaLib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008 Laurent Hubert (CNRS)
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

(** Accessing classes in files, directories and jar files. *)

(** {2 Loading a single class.} *)

(** The type of "compiled" class paths (jar files are opened for efficiency). *)
type class_path

(** [class_path cp] opens a class path from the list [cp] of
    directories and jar files separated by colons ([:]).  jar files in
    the given directories are also considered, but they are not looked
    for recursively.  If [cp] is empty([""]), then the current
    directory is used.  Note: the order matters: the search stops when
    a class file is found. Directories and jar files are read in the
    given order. When several directories are given, the order of the
    jar file inside those directory are unspecified, but the jar file
    of the first directory will be read before the others.
    Note : the following works :
{[try class_path (Sys.getenv "CLASSPATH")
with Not_found -> class_path ""]}*)
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
