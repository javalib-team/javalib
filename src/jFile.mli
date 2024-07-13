(*
 * This file is part of Javalib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
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

(** Accessing classes in files, directories and jar (or zip) files. *)

open JBasics

(** {1 Loading a single class.} *)

type class_path
(** The type of "compiled" class paths (jar (or zip) files are opened for efficiency). *)

val sep : char
(** [sep] is the class path separator. It contains a colon (:) under
    Unix and Cygwin and a semi-colon (;) under Windows (or MinGW). *)

val class_path : string -> class_path
(** [class_path cp] opens a class path from the list [cp] of
    directories and jar (or zip) files separated by {!JFile.sep}.  jar
    (or zip) files in the given directories are also considered, but
    they are not looked for recursively.  If [cp] is empty([""]), then
    the current directory is used.  Note: the order matters: the
    search stops when a class file is found. Directories and jar (or
    zip) files are read in the given order. When several directories
    are given, the order of the jar (or zip) file inside those
    directory are unspecified, but the jar (or zip) file of the first
    directory will be read before the others.

    Note : the following works :
    {[try class_path (Sys.getenv "CLASSPATH")
with Not_found-> class_path ""]}*)

val close_class_path : class_path -> unit
(** Closes a class path. *)

val get_class :
  class_path -> class_name -> JCode.jcode JClass.interface_or_class
(** Parses a single class. It takes as argument the class name built
    with {!JBasics.make_cn}.
    This function does not check that the name of the parsed class is the
    same as the argument xclass name.

    @raise JBasics.No_class_found if the class [class_name] has not been found
    in [class_path].

    @raise JBasics.Class_structure_error if the class file does not match the
    official specification (although it does not check the class file
    entirely).  *)

val get_class_low : class_path -> class_name -> JClassLow.jclass
(** Same as {! get_class} with low level class files. *)

val write_class : string -> JCode.jcode JClass.interface_or_class -> unit
(** [write_class outputdir c] writes the class [c] in the subdirectory of
    [outputdir] that correspond to the package name of [c].

    @raise Class_structure_error if an opcode cannot be encoded in the available
    place.  *)

val write_class_low : string -> JClassLow.jclass -> unit
(** Same as {! write_class} with low level class files. *)

val extract_class_name_from_file : string -> JBasics.class_name * string
(** [extract_class_name_from_file f] recovers a class name and a class
    path from the file [f]. @raise Sys_error if [f] is not a file. [f]
    must contain the [.class] extension. *)

(** {1 Reading/transforming a set of classes.} *)

val iter :
  ?debug:bool ->
  (JCode.jcode JClass.interface_or_class -> unit) ->
  string ->
  unit
(** [iter ~debug:false f filename] applies the function successively the
    function [f] on each classes specified by [filename]. [filename] is either a
    valid class file, a valid jar (or zip) file, or a valid directory with jar
    (or zip) files inside.  The dirname of [filename] is used as classpath.  If
    [debug] is [true] then the number of classes parsed when given a .jar file or
    a directory is printed on the standard error output.  *)

type directories
(** Abstract type representing a list of directories. *)

val make_directories : string -> directories
(** [make_directories directories] returns an abstract [directories] type.  The
    string [directories] must be a list of files separated by {!JFile.sep}. Only
    directories are filtered. *)

(** The following functions search for class files in the following order :
    - If a name can be found in some directory, subsequent directories are
    ignored.
    - If a name is the name of an existing directory, then every
    .class file inside this directory is read, and the search is over
    (even if the directory is empty).
    - Otherwise, if the name refers to an existing .class file
    (without the extension) then this file is read.
    - Otherwise, if the name ends in .jar (or .zip) and the file exists, it is
    assumed to be jar (or zip) file and the class files inside are read.

    Dots in class names are interpreted as / (but not for jar (or zip)
    files). *)

val read :
  directories ->
  ('a -> JCode.jcode JClass.interface_or_class -> 'a) ->
  'a ->
  string list ->
  'a
(** [read directories f acc names] iterates [f] over all classes specified by
    [names]. [acc] is the initial accumulator value.  *)

val transform :
  directories ->
  string ->
  (JCode.jcode JClass.interface_or_class ->
  JCode.jcode JClass.interface_or_class) ->
  string list ->
  unit
(** [transform directories outputdir f names] applies [f] to all
    classes specified by [names], writing the resulting classes in
    [outputdir]. Jar (Or Zip) files are mapped to jar (or zip) files,
    and the non-class files are kept unchanged in the resulting
    archive.  *)

val read_low :
  directories -> ('a -> JClassLow.jclass -> 'a) -> 'a -> string list -> 'a
(** Same as {! read} with low level class files. *)

val transform_low :
  directories ->
  string ->
  (JClassLow.jclass -> JClassLow.jclass) ->
  string list ->
  unit
(** Same as {! transform} with low level class files.  *)
