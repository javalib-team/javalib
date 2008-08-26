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

open JBasics

(** This module describe the signatures used with generics. *)

(** It defines the data types used to represent information extracted
    from the Signature attribute defined in Java 5 (chapter 4.4.4). *)

type typeVariable = TypeVariable of string

(** not only for fields *)
type fieldTypeSignature =
  | GClass of classTypeSignature
  | GArray of typeSignature
  | GVariable of typeVariable
and typeSignature =
  | GBasic of java_basic_type
  | GObject of fieldTypeSignature
and typeArgument =
  | ArgumentExtends of fieldTypeSignature
  | ArgumentInherits of fieldTypeSignature
  | ArgumentIs of fieldTypeSignature
  | ArgumentIsAny
and simpleClassTypeSignature = {
  scts_name : string;
  scts_type_arguments : typeArgument list;
}
and classTypeSignature = {
  cts_package : string list;
  cts_enclosing_classes : simpleClassTypeSignature list;
  cts_simple_class_type_signature : simpleClassTypeSignature;
}
and formalTypeParameter = {
  ftp_name : string;
  ftp_class_bound : fieldTypeSignature option;
  ftp_interface_bounds : fieldTypeSignature list;
}
and classSignature = {
  cs_formal_type_parameters : formalTypeParameter list;
  cs_super_class : classTypeSignature;
  cs_super_interfaces : classTypeSignature list;
}

type throwsSignature =
  | ThrowsClass of classTypeSignature
  | ThrowsTypeVariable of typeVariable

type methodTypeSignature ={
  mts_formal_type_parameters : formalTypeParameter list;
  mts_type_signature : typeSignature list;
  mts_return_type : typeSignature option;
  mts_throws : throwsSignature list;
}
