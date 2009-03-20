(*
 * This file is part of JavaLib
 * Copyright (c)2009 Laurent Hubert (CNRS)
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

(** Builds high level representations of java byte-code programs using
    Rapid Type Analysis algorithm. *)

(** [parse_program ~other_entrypoints classpath (cn,ms)] returns a
    [program] composed of all the code found in [classpath] and that
    may be accessible from at least one method of
    [(cn,ms)::entrypoints].  [classpath] is a list of directories and
    [.jar] files separated with ':'.  If [entrypoints] is not
    specified, the default methods are the methods invoked natively by
    the JVM during its initialization. (cf {!default_entrypoints}).*)
val parse_program :
  ?other_entrypoints:(JBasics.class_name * JClass.method_signature) list ->
  string -> JBasics.class_name * JClass.method_signature-> JProgram.program

val parse_program_bench :
  ?other_entrypoints:(JBasics.class_name * JClass.method_signature) list ->
  string -> JBasics.class_name * JClass.method_signature-> unit

(** Sun's JVM calls some methods natively during the JVM
    initialization.  We have included the list (that we suppose
    complete but without garantee). Some of the method listed may not
    exists (as <clinit> method are optionals) but there should be
    executed in this order. *)
val default_entrypoints : (JBasics.class_name * JClass.method_signature) list
