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

(** Tranformation of high level classes to low level classes. *)

val h2l_cfield :
  'a -> JClass.field_signature -> JClass.class_field -> JClassLow.jfield
val h2l_ifield :
  'a -> JClass.field_signature -> JClass.interface_field -> JClassLow.jfield
val h2l_cmethod :
  JBasics.constant DynArray.t ->
  JClass.method_signature -> JClass.concrete_method -> JClassLow.jmethod
val h2l_amethod :
  'a ->
  JClass.method_signature -> JClass.abstract_method -> JClassLow.jmethod
val h2l_acmethod :
  JBasics.constant DynArray.t ->
  JClass.method_signature ->
  JClass.abstract_class_method -> JClassLow.jmethod

(** Convert a high level class to a lower level class.*)
val high2low : JClass.interface_or_class -> JClassLow.jclass
