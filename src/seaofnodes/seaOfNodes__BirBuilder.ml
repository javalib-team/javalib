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
open Bir
module IMap = JLib.IMap

module BuilderState = struct
  open Monad.State
  open Monad.State.Infix

  type wip_terminator = Done of terminator | Empty | WaitIfT of int | WaitIfF of int

  type wip_block =
    {pred: int list; phis: phi list; code: instr array; wip_terminator: wip_terminator}

  type t = {son: Son.t; reg_map: wip_block IMap.t}

  let initial_state son = {son; reg_map= IMap.empty}

  let get_son () = gets (fun g -> g.son)

  let get_from_son key =
    let* g = get () in
    return @@ Son.get key g.son

  let unwip_block = function
    | {pred; phis; code; wip_terminator= Done t} ->
        {pred; phis= Array.of_list phis; code; terminator= t}
    | _ ->
        failwith "Not Possible"

  let get_region_from_predecessor son pred =
    match pred with
    | Region.Jump r ->
        r
    | Branch bk -> (
      match Son.get bk son with Branch.IfT (Cond c) | IfF (Cond c) -> c.region )

  let rec alloc_region rkey =
    let* g = get () in
    let (Region.Region preds) = Son.get rkey g.son in
    let reg_map =
      IMap.add (Son.get_id rkey)
        { pred= List.map (fun k -> Son.get_id (get_region_from_predecessor g.son k)) preds
        ; phis= []
        ; code= [||]
        ; wip_terminator= Empty }
        g.reg_map
    in
    modify (fun g -> {g with reg_map})

  and find_region rkey =
    let* g = get () in
    let mr = IMap.find_opt (Son.get_id rkey) g.reg_map in
    match mr with None -> return None | Some r -> return @@ Some r

  and exists_region rkey = Option.is_some <$> find_region rkey

  and create_region_if_needed rkey =
    let* b = exists_region rkey in
    if b then Option.get <$> find_region rkey >>= return
    else alloc_region rkey >> (Option.get <$> find_region rkey) >>= return

  and modify_wip_block r f =
    let* region = create_region_if_needed r in
    modify @@ fun g -> {g with reg_map= IMap.add (Son.get_id r) (f region) g.reg_map}

  type partial_term =
    | Pt_Jump of int
    | Pt_IfT of expr * int
    | Pt_IfF of expr * int
    | Pt_Ret of expr

  let add_terminator r pt =
    modify_wip_block r
    @@ fun b ->
    match pt with
    | Pt_Jump i ->
        {b with wip_terminator= Done (Goto i)}
    | Pt_Ret expr ->
        {b with wip_terminator= Done (Return (Some expr))}
    | Pt_IfF (c, i) -> (
      match b.wip_terminator with
      | Empty ->
          {b with wip_terminator= WaitIfT i}
      | WaitIfF j ->
          {b with wip_terminator= Done (If (c, j, i))}
      | _ ->
          failwith "Not Possible 1" )
    | Pt_IfT (c, j) -> (
      match b.wip_terminator with
      | Empty ->
          {b with wip_terminator= WaitIfF j}
      | WaitIfT i ->
          {b with wip_terminator= Done (If (c, j, i))}
      | _ ->
          failwith "Not Possible 2" )
end

open BuilderState
open Monad.State
open Monad.State.Infix

let is_return c = match c with Control.Return _ -> true | _ -> false

let rec translate_region rkey =
  let* (Region.Region preds) = get_from_son rkey in
  let* son = get_son () in
  let* exists = exists_region rkey in
  if exists then return ()
  else
    let* _ =
      Monad.State.fold_leftM
        (fun _ pred ->
          let region_pred = get_region_from_predecessor son pred in
          let* () = translate_region region_pred in
          match pred with
          | Region.Jump r ->
              add_terminator r (Pt_Jump (Son.get_id rkey))
          | Region.Branch brk -> (
              let* br = get_from_son brk in
              match br with
              | Branch.IfF (Cond.Cond c) ->
                  let* cond = translate_data c.operand c.region in
                  add_terminator c.region (Pt_IfF (cond, Son.get_id rkey))
              | Branch.IfT (Cond.Cond c) ->
                  let* cond = translate_data c.operand c.region in
                  add_terminator c.region (Pt_IfT (cond, Son.get_id rkey)) ) )
        () preds
    in
    return ()

and translate_data dkey cr =
  let* d = get_from_son dkey in
  match d with
  | Data.Const {value} ->
      return @@ Bir.Const (`Int (Int32.of_int value))
  | BinOp {op= Binop.Add; operand1; operand2} ->
      let* d1 = translate_data operand1 cr in
      let* d2 = translate_data operand2 cr in
      return @@ Binop (Add `Int2Bool, d1, d2)
  | Phi (Phi.Phi {region; operands}) ->
      let* (Region.Region preds) = get_from_son region in
      let* son = get_son () in
      let* operands =
        fold_leftM
          (fun l (o, p) -> translate_data o p >>= fun x -> return (x :: l))
          []
          (List.combine operands (List.map (get_region_from_predecessor son) preds))
      in
      let* () =
        modify_wip_block cr @@ fun b -> {b with phis= {result= Son.get_id dkey; operands} :: b.phis}
      in
      return (Var (Son.get_id dkey))

let translate_state () =
  let* son = get_son () in
  let _, ret = List.hd @@ List.filter (fun (_, c) -> is_return c) (Son.control_nodes son) in
  let region, operand =
    match ret with Control.Return {region; operand} -> (region, operand) | _ -> failwith "todo"
  in
  let* b = exists_region region in
  if not b then
    let* () = translate_region region in
    let* expr = translate_data operand region in
    let* () = add_terminator region (Pt_Ret expr) in
    return ()
  else return ()

let translate_son son =
  let initial = initial_state son in
  let {reg_map} = Monad.State.exec (translate_state ()) initial in
  IMap.map unwip_block reg_map
