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

  let compare = Stdlib.compare
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

  let get_source = function Jump n | IfT n | IfF n | Implicit n -> n

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

  let%test "add_edge" =
    let cfg = add_edge EntryPoint IMap.empty in
    let cfg = add_edge (Goto (1, 2)) cfg in
    let cfg = add_edge (IfT (3, 4)) cfg in
    let cfg = add_edge (IfF (3, 0)) cfg in
    IMap.bindings cfg = [(0, [IfF 3]); (2, [Jump 1]); (4, [IfT 3])]

  let is_jump = function
    | JCode.OpGoto _ ->
        true
    | OpReturn _ ->
        true
    | OpIf _ ->
        true
    | _ ->
        false

  let%test "is_jump" =
    is_jump (OpGoto 0) && is_jump (OpReturn `Void)
    && is_jump (OpIf (`Eq, 0))
    && not (is_jump OpDup)

  (* We assume that pc > 0 *)
  let rec previous_pc pc jopcodes =
    assert (pc > 0) ;
    match jopcodes.(pc - 1) with
    | JCode.OpInvalid ->
        previous_pc (pc - 1) jopcodes
    | _ ->
        pc - 1

  let%test "previous_pc" =
    previous_pc 2 [|JCode.OpDup; OpInvalid; OpDup|] = 0
    && previous_pc 1 [|JCode.OpDup; OpDup|] = 0

  let rec next_pc pc jopcodes =
    match jopcodes.(pc + 1) with
    | JCode.OpInvalid ->
        next_pc (pc + 1) jopcodes
    | _ ->
        pc + 1

  let%test "next_pc" =
    next_pc 0 [|JCode.OpDup; OpInvalid; OpDup|] = 2
    && next_pc 0 [|JCode.OpDup; OpDup|] = 1

  let add_implicit_edges explicit_edges jopcodes =
    IMap.fold
      (fun pc _ cfg ->
        if pc = 0 then cfg
        else
          let prev_pc = previous_pc pc jopcodes in
          if is_jump jopcodes.(prev_pc) then cfg
          else IMap.update pc (add_predecessor @@ Implicit prev_pc) cfg )
      explicit_edges explicit_edges

  let%test "add_implicit_edges" =
    let jopcodes = [|JCode.OpIf (`Eq, 2); OpDup; OpDup|] in
    let cfg = add_implicit_edges (IMap.singleton 2 [IfF 0]) jopcodes in
    IMap.find 2 cfg = [Implicit 1; IfF 0]
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

let%test "build_cfg" =
  let jopcodes =
    [| JCode.OpConst (`Byte 0) ; (* 0 *)
       OpIf (`Eq, 4) ;           (* 1 *)
       OpInvalid ;               (* 2 *)
       OpGoto 3 ;                (* 3 *)
       OpInvalid ;               (* 4 *)
       OpDup ;                   (* 5 *)
       OpGoto (-6)               (* 6 *)
    |]
  in
  Cfg.bindings (build_cfg jopcodes) =
  [ (0, [Jump 6]) ;
    (3, [IfF 1]) ;
    (5, [IfT 1]) ;
    (6, [Implicit 5; Jump 3])
  ]
