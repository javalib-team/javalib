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

type var = int

type expr = Const of const | Binop of binop * expr * expr | Var of var

type instr = Assign of var * expr

type terminator = Return of expr option | If of expr * int * int | Goto of int

type phi = {result: var; operands: expr list}

type block = {pred: int list; phis: phi array; code: instr array; terminator: terminator}

module IMap = JLib.IMap

type program = block IMap.t

let rec eval_block index index_pred program heap =
  (* Print all regions in program *)
  let block = IMap.find index program in
  let heap =
    Array.fold_left
      (fun heap phi ->
        let pred = JLib.List.index_of block.pred index_pred in
        let value = eval_expr (List.nth phi.operands pred) heap in
        IMap.add phi.result value heap )
      heap block.phis
  in
  let heap = Array.fold_left (fun heap instr -> eval_instr instr heap) heap block.code in
  eval_terminator block.terminator heap program index

and eval_terminator t h program index_pred =
  match t with
  | Goto index ->
      eval_block index index_pred program h
  | If (e, index1, index2) ->
      let value = eval_expr e h in
      if value = 0 then eval_block index2 index_pred program h
      else eval_block index1 index_pred program h
  | Return (Some e) ->
      Some (eval_expr e h)
  | Return None ->
      None

and eval_expr expression heap =
  match expression with
  | Const (`Int i) ->
      Int32.to_int i
  | Binop (Add _, e1, e2) ->
      eval_expr e1 heap + eval_expr e2 heap
  | Var v ->
      IMap.find v heap

and eval_instr instr h = match instr with Assign (v, e) -> IMap.add v (eval_expr e h) h

let eval_program program = eval_block 0 0 program IMap.empty
