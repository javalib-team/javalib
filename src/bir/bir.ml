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

type instr = Return of expr option

let rec eval_expr (bir : expr) =
  match bir with
  | Const (`Int n) ->
      Int32.to_int n
  | Binop (Add _, op1, op2) ->
      eval_expr op1 + eval_expr op2

let eval_instr (bir : instr) =
  match bir with Return None -> None | Return (Some expr) -> Some (eval_expr expr)
