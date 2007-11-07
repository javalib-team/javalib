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

(** The following functions search for class files in the following order :
    - [classpath] is a list of directories separated by [:]. If a name can be
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

(** [read classpath f acc names] iterates [f] over all classes specified by
    [names]. [acc] is the initial accumulator value. *)
val read : string -> ('a -> JClass.class_file -> 'a) -> 'a -> string list -> 'a

(** [read classpath f acc names] iterates [f] over all classes specified by
    [names]. [acc] is the initial accumulator value. *)
val read_low : string -> ('a -> JClassLow.jclass -> 'a) -> 'a -> string list -> 'a

(** [transform classpath outputdir f names] applies [f] to all classes specified
    by [names], writing the resulting classes in [outputdir]. Jar files are
    mapped to jar files, and the non-class files are kept unchanged in the
    resulting archive. Works on the low-level representation of classes. *)
val transform_low : string -> string -> (JClassLow.jclass -> JClassLow.jclass) -> string list -> unit

(** [transform classpath outputdir f names] applies [f] to all classes specified
    by [names], writing the resulting classes in [outputdir]. Jar files are
    mapped to jar files, and the non-class files are kept unchanged in the
    resulting archive. Works on the high-level representation of classes. *)
val transform : string -> string -> (JClass.class_file -> JClass.class_file) -> string list -> unit
