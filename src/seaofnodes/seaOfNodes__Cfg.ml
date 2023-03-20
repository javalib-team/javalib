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

module Edge = struct
  type t =
    | EntryPoint
    | Return of int
    | IfT of int * int
    | IfF of int * int
    | Goto of int * int

  let compare x y = if x < y then -1 else if x > y then 1 else 0
end

module Cfg = struct
  module IMap = Map.Make (Int)
  include IMap

  type predecessor =
    | Jump of int
    | IfT of int
    | IfF of int
    | Implicit of int
        (** [Implicit pc] means that [pc] and [pc + 1] denote two different
            regions, and [pc] is not a control-flow instruction. *)

  type t = predecessor list IMap.t

  let add_predecessor pred = function
    | None ->
        Some [pred]
    | Some preds ->
        Some (pred :: preds)

  let add_edge edge cfg =
    match edge with
    | Edge.EntryPoint ->
        IMap.update 0 (function None -> Some [] | Some x -> Some x) cfg
    | Return _ ->
        cfg
    | IfT (source, target) ->
        IMap.update target (add_predecessor @@ IfT source) cfg
    | IfF (source, target) ->
        IMap.update target (add_predecessor @@ IfF source) cfg
    | Goto (source, target) ->
        IMap.update target (add_predecessor @@ Jump source) cfg

  let is_jump = function
    | JCode.OpGoto _ ->
        true
    | OpReturn _ ->
        true
    | OpIf _ ->
        true
    | _ ->
        false

  (* We assume that pc > 0 *)
  let rec previous_pc pc jopcodes =
    match jopcodes.(pc - 1) with
    | JCode.OpInvalid ->
        previous_pc (pc - 1) jopcodes
    | _ ->
        pc - 1

  let rec next_pc pc jopcodes =
    match jopcodes.(pc + 1) with
    | JCode.OpInvalid ->
        next_pc (pc + 1) jopcodes
    | _ ->
        pc + 1

  let add_implicit_edges explicit_edges jopcodes =
    IMap.fold
      (fun pc _ cfg ->
        if pc = 0 then cfg
        else
          let prev_pc = previous_pc pc jopcodes in
          if is_jump jopcodes.(prev_pc) then cfg
          else IMap.update pc (add_predecessor @@ Implicit (prev_pc)) cfg
        )
      explicit_edges explicit_edges
end

module Monoid = struct
  include Set.Make (Edge)

  let empty = singleton EntryPoint

  let append = union
end

module Writer = Monad.Writer (Monoid)

let tell_edge edge = Writer.tell @@ Monoid.singleton edge

let collect_edges pc jopcodes =
  let open Writer.Infix in
  match jopcodes.(pc) with
  | JCode.OpReturn _ ->
      tell_edge @@ Return pc
  | OpGoto offset ->
      tell_edge @@ Goto (pc, pc + offset)
  | OpIf (_, offset) ->
      let* () = tell_edge @@ IfT (pc, pc + offset) in
      let* () = tell_edge @@ IfF (pc, Cfg.next_pc pc jopcodes) in
      Writer.return ()
  | _ ->
      Writer.return ()

let build_cfg jopcodes =
  (* Collect all the edges of the cfg *)
  let edges =
    Writer.exec
    @@ Writer.fold_leftM
         (fun () pc -> collect_edges pc jopcodes)
         ()
         (List.init (Array.length jopcodes) Fun.id)
  in
  (* edges visible in the bytecode, such as gotos and ifs *)
  let explicit_edges = Monoid.fold Cfg.add_edge edges Cfg.empty in
  Cfg.add_implicit_edges explicit_edges jopcodes
