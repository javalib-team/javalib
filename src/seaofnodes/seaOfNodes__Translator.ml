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

  type t =
    { stack: Data.t Son.key list
    ; reg_map: Region.t Son.key IMap.t
    ; jopcodes: JCode.jopcodes
    ; pc: int
    ; son: Son.t }

  let initial jopcodes =
    let cfg = build_cfg jopcodes in
    (* Alloc all the regions *)
    let add_node k _ (reg_map, son) =
      let son', key = Son.alloc_region son (Region.Region []) in
      (IMap.add k key reg_map, son')
    in
    let reg_map, son = Cfg.fold add_node cfg (IMap.empty, Son.empty) in
    (* Add implicit predecessors to regions *)
    (* By doing this now, we avoid cumbersome management of implicit regions in the main translation loop *)
    let son =
      Cfg.fold
        (fun k preds son ->
          let rk = IMap.find k reg_map in
          List.fold_left
            (fun son pred ->
              match pred with
              | Cfg.Implicit i ->
                  let ri = IMap.find i reg_map in
                  Son.add_predecessor son rk (Region.Jump ri)
              | _ ->
                  son )
            son preds )
        cfg son
    in
    {stack= []; reg_map; jopcodes; pc= 0; son}

  let return = Monad.State.return

  let push_stack x = Monad.State.modify (fun g -> {g with stack= x :: g.stack})

  let pop_stack () =
    let* g = Monad.State.get () in
    match g.stack with
    | [] ->
        assert false
    | x :: s ->
        Monad.State.set {g with stack= s} >> Monad.State.return x

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

  let get_current_instruction () =
    let* g = Monad.State.get () in
    return g.jopcodes.(g.pc)

  let get_code_size () =
    let* g = Monad.State.get () in
    return @@ Array.length g.jopcodes

  let incr_pc () = Monad.State.modify (fun g -> {g with pc= g.pc + 1})

  let get_region_at pc =
    let* g = Monad.State.get () in
    Monad.State.return @@ snd @@ IMap.find_last (fun k -> k <= pc) g.reg_map

  let get_current_region () =
    let* g = Monad.State.get () in
    get_region_at g.pc

  let get_node node =
    let* g = Monad.State.get () in
    return @@ Son.get node g.son

  let set_node key node =
    Monad.State.modify @@ fun g -> {g with son= Son.set key node g.son}
end

(** Translate one opcode *)
let translate_jopcode (op : JCode.jopcode) =
  let open Monad.State.Infix in
  let open TranslatorState in
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
      let* region = get_current_region () in
      let node = Control.Return {region; operand} in
      let* _ = insert_control node in
      return ()
  | OpIf (`Eq, offset) ->
      let* cr = get_current_region () in
      let* operand = pop_stack () in
      let* pc = get_pc () in
      let* reg_true = get_region_at (pc + offset) in
      let branchT = Branch.IfT (Cond.Cond {region= cr; operand}) in
      let* keyT = insert_branch branchT in
      let* () = add_predecessor reg_true (Region.Branch keyT) in
      let* reg_false = get_region_at (pc + offset) in
      let branchF = Branch.IfF (Cond.Cond {region= cr; operand}) in
      let* keyF = insert_branch branchF in
      add_predecessor reg_false (Region.Branch keyF)
  | OpGoto offset ->
      let* cr = get_current_region () in
      let* pc = get_pc () in
      let* target = get_region_at (pc + offset) in
      add_predecessor target (Region.Jump cr)
  | _ ->
      return ()

let rec translate_state () =
  let open Monad.State.Infix in
  let open TranslatorState in
  let* jopcode = get_current_instruction () in
  let* () = translate_jopcode jopcode in
  let* () = incr_pc () in
  let* pc = get_pc () in
  let* code_size = get_code_size () in
  if pc >= code_size then return () else translate_state ()

(** Translate opcodes *)
let translate_jopcodes (ops : JCode.jopcodes) =
  let g = Monad.State.exec (translate_state ()) (TranslatorState.initial ops) in
  g.son
