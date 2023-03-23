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

(*
write a small BIR SSA interpreter by adding the necessary information
examples : return 1 + (ternary operator)
           no phi_nodes but conditional jumps 
   *)

open SeaOfNodes__Type
open SeaOfNodes__Cfg

module TranslatorState = struct
  open Monad.State.Infix
  module IMap = Map.Make (Int)
  module ISet = Set.Make (Int)

  type stack = Data.t Son.key list

  type successor = Next | Jump

  type region_info =
    { predecessors: Cfg.predecessor list
    ; last_stack:
        stack option (* Last stack of a region, None means not computed yet *)
    ; region: Region.t Son.key
    ; entry_point: int }

  type t =
    { stack: stack
    ; reg_map: region_info IMap.t
    ; jopcodes: JCode.jopcodes
    ; pc: int
    ; son: Son.t
    ; work_list: ISet.t }

  let print_state () =
    let* g = Monad.State.get () in
    Printf.printf "stack:\n" ;
    List.iter
      (fun data_key -> Printf.printf "%d, " (Son.get_id data_key))
      g.stack ;
    Printf.printf "\npc: %d\n" g.pc ;
    Monad.State.return ()

  let initial jopcodes =
    let cfg = build_cfg jopcodes in
    (* Alloc all the regions *)
    let add_node entry_point predecessors (reg_map, son) =
      let son', region = Son.alloc_region son (Region.Region []) in
      ( IMap.add entry_point
          {predecessors; last_stack= None; region; entry_point}
          reg_map
      , son' )
    in
    let reg_map, son = Cfg.fold add_node cfg (IMap.empty, Son.empty) in
    (* Add implicit predecessors to regions *)
    (* By doing this now, we avoid cumbersome management of implicit regions in the main translation loop *)
    let son =
      Cfg.fold
        (fun k preds son ->
          let {region= rk} = IMap.find k reg_map in
          List.fold_left
            (fun son pred ->
              match pred with
              | Cfg.Implicit i ->
                  let {region= ri} = IMap.find i reg_map in
                  Son.add_predecessor son rk (Region.Jump ri)
              | _ ->
                  son )
            son preds )
        cfg son
    in
    {stack= []; reg_map; jopcodes; pc= 0; son; work_list= ISet.empty}

  let return = Monad.State.return

  let flush_stack () = Monad.State.modify @@ fun g -> {g with stack= []}

  let get_pc () =
    let* g = Monad.State.get () in
    Monad.State.return @@ g.pc

  let insert_data data =
    let* g = Monad.State.get () in
    let son, key = Son.alloc_data g.son data in
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

  let add_predecessor region predecessor =
    let* g = Monad.State.get () in
    let f (Region.Region l) = Region.Region (predecessor :: l) in
    let* () = Monad.State.set {g with son= Son.modify region f g.son} in
    return ()

  let is_region pc =
    let* g = Monad.State.get () in
    return @@ IMap.mem pc g.reg_map

  let get_current_instruction () =
    let* g = Monad.State.get () in
    return g.jopcodes.(g.pc)

  let incr_pc () =
    Monad.State.modify (fun g -> {g with pc= Cfg.next_pc g.pc g.jopcodes})

  let add_to_workset pc =
    Monad.State.modify @@ fun g -> {g with work_list= ISet.add pc g.work_list}

  let jump_to_next_region () =
    let* g = Monad.State.get () in
    if ISet.is_empty g.work_list then return false
    else
      let pc = ISet.find_first (Fun.const true) g.work_list in
      let* () =
        Monad.State.set {g with pc; work_list= ISet.remove pc g.work_list}
      in
      return true

  let get_region_info_at pc =
    let* g = Monad.State.get () in
    let region_info = snd @@ IMap.find_last (fun k -> k <= pc) g.reg_map in
    return region_info

  let get_current_region_info () =
    let* g = Monad.State.get () in
    get_region_info_at g.pc

  let push_stack x = Monad.State.modify (fun g -> {g with stack= x :: g.stack})

  let pop_from_region pc =
    let* region_info = get_region_info_at pc in
    match region_info.last_stack with
    | None ->
        assert false (* Empty lists for backjumps *)
    | Some [] ->
        assert false (* All the lists have the same size *)
    | Some (d :: ds) ->
        let region_info = {region_info with last_stack= Some ds} in
        let* () =
          Monad.State.modify
          @@ fun g ->
          { g with
            reg_map= IMap.add region_info.entry_point region_info g.reg_map }
        in
        return d

  let pop_stack () =
    let* g = Monad.State.get () in
    match g.stack with
    | [] ->
        (* Stack is empty, need to lookup previous stacks *)
        let* region_info = get_region_info_at g.pc in
        let predecessors = region_info.predecessors in
        let* all_datas =
          Monad.State.fold_leftM
            (fun acc pred ->
              let* data = pop_from_region (Cfg.get_source pred) in
              return (data :: acc) )
            [] predecessors
        in
        let* {region} = get_current_region_info () in
        insert_data (Data.phi (Phi.Phi {region; operands= all_datas}))
    | x :: s ->
        Monad.State.set {g with stack= s} >> Monad.State.return x

  let save_stack_to pc =
    let* g = Monad.State.get () in
    let* reg_info = get_region_info_at pc in
    let reg_info = {reg_info with last_stack= Some g.stack} in
    Monad.State.set
      {g with reg_map= IMap.add reg_info.entry_point reg_info g.reg_map}

  let get_node node =
    let* g = Monad.State.get () in
    return @@ Son.get node g.son

  let set_node key node =
    Monad.State.modify @@ fun g -> {g with son= Son.set key node g.son}

  let get_jopcodes () = Monad.State.gets @@ fun g -> g.jopcodes
end

(** Translate one opcode *)
let translate_jopcode (op : JCode.jopcode) =
  let open Monad.State.Infix in
  let open TranslatorState in
  match op with
  | OpLoad (_, n) ->
      (* Get the data from the graph *)
      let key = Son.unsafe_make_key n in
      let* () = push_stack key in
      return Next
  | OpAdd _ ->
      let* operand1 = pop_stack () in
      let* operand2 = pop_stack () in
      let node = Data.binop Binop.Add operand1 operand2 in
      let* key = insert_data node in
      let* () = push_stack key in
      return Next
  | OpStore (_, id) ->
      (* Use the bytecode id *)
      let* operand_key = pop_stack () in
      let* operand = get_node operand_key in
      let* () = set_node (Son.unsafe_make_key id) operand in
      return Next
  | OpConst (`Int n) ->
      let data = Data.const (Int32.to_int n) in
      let* key = insert_data data in
      let* () = push_stack key in
      return Next
  | OpConst (`Byte n) ->
      let data = Data.const n in
      let* key = insert_data data in
      let* () = push_stack key in
      return Next
  | OpReturn _ ->
      let* operand = pop_stack () in
      let* {region} = get_current_region_info () in
      let node = Control.Return {region; operand} in
      let* _ = insert_control node in
      return Jump
  | OpIf (`Eq, offset) ->
      let* {region= cr} = get_current_region_info () in
      let* operand = pop_stack () in
      let* pc = get_pc () in
      let* reg_true = get_region_info_at (pc + offset) in
      let branchT = Branch.IfT (Cond.Cond {region= cr; operand}) in
      let* keyT = insert_branch branchT in
      let* () = add_predecessor reg_true.region (Region.Branch keyT) in
      let* jopcodes = get_jopcodes () in
      let* reg_false = get_region_info_at (Cfg.next_pc pc jopcodes) in
      let branchF = Branch.IfF (Cond.Cond {region= cr; operand}) in
      let* keyF = insert_branch branchF in
      let* () = add_predecessor reg_false.region (Region.Branch keyF) in
      let* () = add_to_workset (pc + offset) in
      let* () = add_to_workset (Cfg.next_pc pc jopcodes) in
      return Jump
  | OpGoto offset ->
      let* cr = get_current_region_info () in
      let* pc = get_pc () in
      let* target = get_region_info_at (pc + offset) in
      let* () = add_predecessor target.region (Region.Jump cr.region) in
      let* () = add_to_workset (pc + offset) in
      return Jump
  | _ ->
      return Next

let rec translate_state () =
  let open Monad.State.Infix in
  let open TranslatorState in
  let* jopcode = get_current_instruction () in
  let* succ = translate_jopcode jopcode in
  let _ = print_state () in
  match succ with
  | Next ->
      let* old_pc = get_pc () in
      let* () = incr_pc () in
      let* pc = get_pc () in
      let* is_new_region = is_region pc in
      if is_new_region then
        let* () = save_stack_to old_pc in
        let* () = flush_stack () in
        let* () = add_to_workset pc in
        let* is_node_to_visit = jump_to_next_region () in
        if is_node_to_visit then translate_state () else return ()
      else translate_state ()
  | Jump ->
      let* pc = get_pc () in
      let* () = save_stack_to pc in
      let* () = flush_stack () in
      let* is_node_to_visit = jump_to_next_region () in
      if is_node_to_visit then translate_state () else return ()

(** Translate opcodes *)
let translate_jopcodes (ops : JCode.jopcodes) =
  let g = Monad.State.exec (translate_state ()) (TranslatorState.initial ops) in
  g.son
