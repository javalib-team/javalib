(*
 * This file is part of Javalib
 * Copyright (c)2023 Martin Andrieux (ENS Rennes)
 * Copyright (c)2023 Alban Dutilleul (ENS Rennes)
 * Copyright (c)2023 David Pichardie (Facebook France)
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

(* Bir type from https://github.com/javalib-team/sawja/blob/master/src/bir.ml *)

type const = [`Int of int32]

type binop = Add of JBasics.jvm_basic_type

type expr = Const of const | Binop of binop * expr * expr

type instr =
  | Return of expr option
  | Ifd of ([`Eq] * expr * expr) * int
  | Goto of int

val eval_instr : int -> instr array -> int option
