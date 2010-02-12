(*
 * This file is part of Javalib
 * Copyright (c)2010 Laurent Hubert (CNRS)
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

(** Parsing of Java 5 annotations.  *)

(** Java 5 annotations are described in The Java Language Specification Third
    Edition, essentially in Section 9.7
    (http://java.sun.com/docs/books/jls/third_edition/html/interfaces.html#9.7)
    and in the JVMSpec 1.5 §4.8.15 to §4.8.19.  *)

(** [element_value] represents a constant value, either a number, a string, a
    class, an enum, an array of [element_value]s or another annotation. *)
type element_value =
  | EVCst of JBasics.constant_value
  | EVEnum of (string * string)
  | EVClass of JBasics.value_type option
  | EVAnnotation of annotation
  | EVArray of element_value list

(** An [annotation] contains the name [kind] of the annotation an a list of
    element-value pairs (the name of the element and its value).  Each element
    name should correspond to a name of an element of the annotation but this is
    not checked. *)
and annotation = {
  kind : JBasics.class_name;
  element_value_pairs : (string * element_value) list;
}


type visibility = RTVisible | RTInvisible

(** [rt_annotations] represent the annotations which are associated with a
    class, a method or a field. [RTVisibleParameter] and [RTInvisibleParameter]
    should only be associated to methods (but it is not checked). *)
type rt_annotations =
  | Annotations of visibility * (annotation list)
  | ParameterAnnotations of visibility * (annotation list list)

(** A [default_annotation] may only be associated with method of annotation
    class and represents the default value of the corresponding element. *)
type default_annotation = AnnotationDefault of element_value

(** shortcut  *)
type constant_pool = JBasics.constant array
type attributes = (string * string) list

(** {2 Annotation printing functions} *)

val pp_element_value : Format.formatter -> element_value -> unit
val pp_annotation : Format.formatter -> annotation -> unit
val pp_rt_annotations : Format.formatter -> rt_annotations -> unit
val pp_default_annotation : Format.formatter -> default_annotation -> unit

(** {2 Annotation parsing functions}  *)

(** [parse_RTAnnotations constant_pool attribute] parse the run-time annotations
    founds in attribute and return them along with the other attributes that
    have not been parsed. *)
val parse_RTAnnotations : constant_pool -> attributes -> rt_annotations list * attributes

(** [parse_AnnotationDefault constant_pool attributes] parse all
    AnnotationDefault annotations found in [attributes] and those annotations
    and the list of attributes where the annotations parsed have been
    removed.  *)
val parse_AnnotationDefault :
  constant_pool -> attributes -> default_annotation list * attributes
