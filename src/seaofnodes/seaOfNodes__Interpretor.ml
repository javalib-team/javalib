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

open SeaOfNodes__Type

module InterpreterState = struct
  open Monad.State
  open Monad.State.Infix
  open JLib

  type reach_flag = Unreachable | Reachable of int | EntryPoint

  type block_info = {flag: reach_flag}

  type t = {son: Son.t; reg_map: block_info IMap.t}

  let initial_state son = {son; reg_map= IMap.empty}

  let get_block_info r =
    let* g = Monad.State.get () in
    return @@ IMap.find_opt (Son.get_id r) g.reg_map

  let set_flag r flag =
    Monad.State.modify
    @@ fun g -> {g with reg_map= IMap.add (Son.get_id r) {flag} g.reg_map}

  let get_son () = gets (fun g -> g.son)

  let get_from_son key =
    let* g = get () in
    return @@ Son.get key g.son

  let get_region_from_predecessor son pred =
    match pred with
    | Region.Jump r ->
        r
    | Branch bk -> (
      match Son.get bk son with Branch.IfT (Cond c) | IfF (Cond c) -> c.region )
end

open InterpreterState
open Monad.State
open Monad.State.Infix

let is_return c = match c with Control.Return _ -> true | _ -> false

let rec get_flag r =
  let* bi = get_block_info r in
  match bi with
  | None ->
      let* (Region.Region preds) = get_from_son r in
      let* res =
        fold_leftM
          (fun acc pred ->
            match acc with
            | Error idx -> (
              match pred with
              | Region.Jump r_pred ->
                  let* flag = get_flag r_pred in
                  return (if flag = Unreachable then Error (idx + 1) else Ok idx)
              | Region.Branch br -> (
                  let* branch = get_from_son br in
                  match branch with
                  | Branch.IfT (Cond.Cond {region; operand}) -> (
                      let* flag = get_flag region in
                      match flag with
                      | Unreachable ->
                          return @@ Error (idx + 1)
                      | _ ->
                          let* r = eval_data region operand in
                          if r = 0 then return @@ Ok idx
                          else return @@ Error (idx + 1) )
                  | Branch.IfF (Cond.Cond {region; operand}) -> (
                      let* flag = get_flag region in
                      match flag with
                      | Unreachable ->
                          return @@ Error (idx + 1)
                      | _ ->
                          let* r = eval_data region operand in
                          if r <> 0 then return @@ Ok idx
                          else return @@ Error (idx + 1) ) ) )
            | Ok idx ->
                return (Ok idx) )
          (Error 0) preds
      in
      let flag =
        match res with
        | Error 0 ->
            (* preds is empty *)
            EntryPoint
        | Error _ ->
            Unreachable
        | Ok idx ->
            Reachable idx
      in
      let* () = set_flag r flag in
      return flag
  | Some block_info ->
      return block_info.flag

and eval_data region dkey =
  let* data = get_from_son dkey in
  match data with
  | Data.Const {value} ->
      return value
  | BinOp {op= Binop.Add; operand1; operand2} ->
      let* e1 = eval_data region operand1 in
      let* e2 = eval_data region operand2 in
      return (e1 + e2)
  | Phi (Phi.Phi {region; operands}) ->
      let* flag = get_flag region in
      let idx =
        match flag with
        | Reachable idx ->
            idx
        | _ ->
            failwith "Phi has no predecessor"
      in
      let* (Region.Region preds) = get_from_son region in
      let* son = get_son () in
      eval_data
        (get_region_from_predecessor son @@ List.nth preds idx)
        (List.nth operands idx)

let translate_state () =
  let* son = get_son () in
  let _, ret =
    List.hd @@ List.filter (fun (_, c) -> is_return c) (Son.control_nodes son)
  in
  let region, operand =
    match ret with
    | Control.Return {region; operand} ->
        (region, operand)
    | _ ->
        failwith "todo"
  in
  eval_data region operand

let eval_son son =
  let initial = initial_state son in
  let _, x = Monad.State.run (translate_state ()) initial in
  x
