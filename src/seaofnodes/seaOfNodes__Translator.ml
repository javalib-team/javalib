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
open SeaOfNodes__Cfg

module TranslatorState = struct
  open Monad.State.Infix
  module IMap = Map.Make (Int)

  type stack = Data.t Son.key list

  type region_info =
    { last_stack: stack option (* Last stack of a region, None means not computed yet *)
    ; region: Region.t Son.key
    ; entry_point: int
    ; cond: Cond.t option }

  type branch_resolver = {current_reg_pc: int; index: int; pred: Cfg.predecessor}

  type t =
    { stack: stack
    ; reg_map: region_info IMap.t
    ; pc: int
    ; son: Son.t
    ; work_list: branch_resolver list
    ; jopcodes: JCode.jopcodes
    ; cfg: Cfg.t }

  let initial jopcodes =
    let cfg = build_cfg jopcodes in
    {stack= []; reg_map= IMap.empty; pc= 0; son= Son.empty; work_list= []; jopcodes; cfg}

  let return = Monad.State.return

  let get_pc () =
    let* g = Monad.State.get () in
    Monad.State.return @@ g.pc

  let insert_data data =
    let* g = Monad.State.get () in
    let son, key = Son.alloc_data g.son data in
    let* () = Monad.State.set {g with son} in
    return key

  let insert_region region =
    let* g = Monad.State.get () in
    let son, key = Son.alloc_region g.son region in
    let* () = Monad.State.set {g with son} in
    return key

  let insert_control control =
    let* g = Monad.State.get () in
    let son, key = Son.alloc_control g.son control in
    let* () = Monad.State.set {g with son} in
    return key

  let insert_branch branch =
    let* g = Monad.State.get () in
    let son, key = Son.alloc_branch g.son branch in
    let* () = Monad.State.set {g with son} in
    return key

  let add_region region_info =
    Monad.State.modify
    @@ fun g -> {g with reg_map= IMap.add region_info.entry_point region_info g.reg_map}

  let get_predecessors pc =
    let* g = Monad.State.get () in
    return @@ Cfg.find pc g.cfg

  let add_to_work_list br = Monad.State.modify @@ fun g -> {g with work_list= br :: g.work_list}

  let is_branching_point pc =
    let* g = Monad.State.get () in
    return @@ Cfg.mem pc g.cfg

  let get_current_instruction () =
    let* g = Monad.State.get () in
    return g.jopcodes.(g.pc)

  let set_pc pc = Monad.State.modify (fun g -> {g with pc})

  let get_region_info_at pc =
    let* g = Monad.State.get () in
    let region_info = snd @@ IMap.find_last (fun k -> k <= pc) g.reg_map in
    return region_info

  let get_current_region_info () =
    let* g = Monad.State.get () in
    get_region_info_at g.pc

  let set_current_cond cond =
    let* reg_info = get_current_region_info () in
    Monad.State.modify
    @@ fun g ->
    {g with reg_map= IMap.add reg_info.entry_point {reg_info with cond= Some cond} g.reg_map}

  let push_stack x = Monad.State.modify (fun g -> {g with stack= x :: g.stack})

  let flush_stack () = Monad.State.modify (fun g -> {g with stack= []})

  let pop_stack () =
    let* g = Monad.State.get () in
    match g.stack with
    | [] ->
        failwith "empty stack"
    | x :: s ->
        Monad.State.set {g with stack= s} >> Monad.State.return x

  let save_stack_to pc =
    let* g = Monad.State.get () in
    let* reg_info = get_region_info_at pc in
    let reg_info = {reg_info with last_stack= Some g.stack} in
    Monad.State.set {g with reg_map= IMap.add reg_info.entry_point reg_info g.reg_map}

  let rec merge_stacks stacks =
    (* check that all stacks have the same size *)
    assert (List.for_all (fun s -> List.length s = List.length (List.hd stacks)) stacks) ;

    let worker stacks =
      match stacks with
      | [] ->
          return ()
      | s :: ss ->
          let* r = get_current_region_info () in
          let* k = insert_data @@ Data.phi @@ Phi.Phi {region= r.region; operands= s} in
          let* () = push_stack k in
          merge_stacks ss
    in
    worker @@ List.rev (JLib.List.transpose stacks)

  let get_node node =
    let* g = Monad.State.get () in
    return @@ Son.get node g.son

  let set_node key node = Monad.State.modify @@ fun g -> {g with son= Son.set key node g.son}

  (*  *type branch_resolver = {current_reg_pc: int; index: int; pred: Cfg.predecessor} *)
  let resolve_branch br =
    let* region = get_region_info_at br.current_reg_pc in
    let* g = Monad.State.get () in
    match br.pred with
    | Jump src ->
        let* src_region = get_region_info_at src in
        let (Region.Region ps) = Son.get region.region g.son in
        let ps = JLib.List.insert_at_reverse br.index (Region.Jump src_region.region) ps in
        let son = Son.set region.region (Region.Region ps) g.son in
        Monad.State.modify @@ fun g -> {g with son}
    | IfT src ->
        let* src_region = get_region_info_at src in
        let (Region.Region ps) = Son.get region.region g.son in
        let cond = Option.get src_region.cond in
        let* branch = insert_branch (Branch.IfT cond) in
        let ps = JLib.List.insert_at_reverse br.index (Region.Branch branch) ps in
        let son = Son.set region.region (Region.Region ps) g.son in
        Monad.State.modify @@ fun g -> {g with son}
    | IfF src ->
        let* src_region = get_region_info_at src in
        let (Region.Region ps) = Son.get region.region g.son in
        let cond = Option.get src_region.cond in
        let* branch = insert_branch (Branch.IfF cond) in
        let ps = JLib.List.insert_at_reverse br.index (Region.Branch branch) ps in
        let son = Son.set region.region (Region.Region ps) g.son in
        Monad.State.modify @@ fun g -> {g with son}
    | _ ->
        return ()

  let manage_work_list () =
    let* g = Monad.State.get () in
    Monad.State.fold_leftM (fun _ -> resolve_branch) () g.work_list
end

open Monad.State.Infix
open TranslatorState

(** Translate one opcode *)
let translate_jopcode (op : JCode.jopcode) =
  match op with
  | OpLoad (_, n) ->
      (* Get the data from the graph *)
      let key = Son.unsafe_make_key n in
      push_stack key
  | OpAdd _ ->
      let* operand1 = pop_stack () in
      let* operand2 = pop_stack () in
      let node = Data.binop Binop.Add operand1 operand2 in
      let* key = insert_data node in
      push_stack key
  | OpStore (_, id) ->
      (* Use the bytecode id *)
      let* operand_key = pop_stack () in
      let* operand = get_node operand_key in
      set_node (Son.unsafe_make_key id) operand
  | OpConst (`Int n) ->
      let data = Data.const (Int32.to_int n) in
      let* key = insert_data data in
      push_stack key
  | OpConst (`Byte n) ->
      let data = Data.const n in
      let* key = insert_data data in
      push_stack key
  | OpReturn _ ->
      let* operand = pop_stack () in
      let* {region} = get_current_region_info () in
      let node = Control.Return {region; operand} in
      let* _ = insert_control node in
      return ()
  | OpIf (_, _offset) ->
      let* operand = pop_stack () in
      let* pc = get_pc () in
      let* () = save_stack_to pc in
      let* {region} = get_current_region_info () in
      let cond = Cond.Cond {region; operand} in
      set_current_cond cond
  | OpGoto _offset ->
      let* pc = get_pc () in
      save_stack_to pc
  | _ ->
      let* pc = get_pc () in
      Printf.fprintf stderr "[warning: son_translator] Not implemented opcode at %d\n" pc;
      return ()

let compute_one_predecessor (l, index) pred =
  let* pc = get_pc () in
  match pred with
  | (Cfg.Jump src | IfT src | IfF src) when src >= pc ->
      (* Region not computed *)
      let* () = add_to_work_list {current_reg_pc= pc; index; pred} in
      return (l, index + 1)
  | Jump src ->
      let* {region= key} = get_region_info_at src in
      return @@ (Region.Jump key :: l, index + 1)
  | Implicit pc ->
      let* {region= key} = get_region_info_at pc in
      let* () = save_stack_to pc in
      return @@ (Region.Jump key :: l, index + 1)
  | IfT src -> (
      let* region_info = get_region_info_at src in
      match region_info.cond with
      | None ->
          failwith "Branch from non conditional jump"
      | Some cond ->
          let* br = insert_branch (Branch.IfT cond) in
          return @@ (Region.Branch br :: l, index + 1) )
  | IfF src -> (
      let* region_info = get_region_info_at src in
      match region_info.cond with
      | None ->
          failwith "Branch from non conditional jump"
      | Some cond ->
          let* br = insert_branch (Branch.IfF cond) in
          return @@ (Region.Branch br :: l, index + 1) )

let compute_predecessors pc =
  let* predecessors = get_predecessors pc in
  fst <$> Monad.State.fold_leftM compute_one_predecessor ([], 0) predecessors

let manage_branching_point () =
  let* pc = get_pc () in
  let* is_branch = is_branching_point pc in
  if is_branch then
    let* predecessors = compute_predecessors pc in
    let* () = flush_stack () in
    let* region = insert_region @@ Region.Region predecessors in
    let region_info = {last_stack= None; region; entry_point= pc; cond= None} in
    let* () = add_region region_info in
    let* predecessors = get_predecessors pc in
    let* stacks =
      Monad.State.fold_leftM
        (fun l p ->
          let* {last_stack} = get_region_info_at (Cfg.get_source p) in
          return (Option.default [] last_stack :: l) )
        [] predecessors
    in
    merge_stacks stacks
  else return ()

let translate_state () =
  let* () = manage_branching_point () in
  let* jopcode = get_current_instruction () in
  translate_jopcode jopcode

(** Translate opcodes *)
let translate_jopcodes (ops : JCode.jopcodes) =
  let g =
    Monad.State.exec
      (let* () =
         Monad.State.array_iterM
           (fun i op ->
             let* () = set_pc i in
             if op <> JCode.OpInvalid then translate_state () else return () )
           ops
       in
       manage_work_list () )
      (TranslatorState.initial ops)
  in
  g.son

(* checks if a simple assignment in a if can be translated, see code below
   int a = 0;
   int c = 0;
   if (a == 0) {
    c = 1;
   } else {
    c = 2;
   }
   return c;
*)

let%test_unit "translate_if" =
  let jopcodes =
    [| JCode.OpConst (`Int 0l)
     ; OpStore (`Int2Bool, 1)
     ; OpConst (`Int 0l)
     ; OpStore (`Int2Bool, 2)
     ; OpLoad (`Int2Bool, 1)
     ; OpIf (`Eq, 3)
     ; OpConst (`Int 1l)
     ; OpStore (`Int2Bool, 2)
     ; OpGoto 5
     ; OpInvalid
     ; OpInvalid
     ; OpConst (`Int 2l)
     ; OpStore (`Int2Bool, 2)
     ; OpLoad (`Int2Bool, 2)
     ; OpReturn `Int2Bool |]
  in
  let _ = translate_jopcodes jopcodes in
  ()
