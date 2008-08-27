(*
 *  This file is part of JavaLib
 *  Copyright (c)2007-2008 Université de Rennes 1 / CNRS
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

(** This module handles the decoding of descriptors and signatures.*)

open JBasics
open JSignature

(** {2 Parsing descriptors and their components } *)

(** *)
val parse_objectType : string -> object_type
val parse_field_descriptor : string -> value_type
val parse_method_descriptor : string -> value_type list * value_type option
val parse_descriptor : string -> descriptor



(** {2 Parsing generic signatures} *)

(** [parse_ClassSignature s] parses a Signature attribute and expects
    to find a ClassSignature (as describe in paragraph 4.4.4 of the
    Java Virtual Machine Specification of Java 5).

    @raise Class_structure_error if the signature does not correspond
    to the specifications. *)
val parse_ClassSignature : string -> classSignature

(** [parse_MethodTypeSignature s] parses a Signature attribute and
    expects to find a Methodtypesignature (as describe in paragraph
    4.4.4 of the Java Virtual Machine Specification of Java 5).

    @raise Class_structure_error if the signature does not correspond
    to the specifications. *)
val parse_MethodTypeSignature : string -> methodTypeSignature

(** [parse_FieldTypeSignature s] parses a Signature attribute [s] and
    expects to find a FieldTypeSignature (as describe in paragraph
    4.4.4 of the Java Virtual Machine Specification of Java 5).

    @raise Class_structure_error if the signature does not correspond
    to the specifications. *)
val parse_FieldTypeSignature : string -> fieldTypeSignature
