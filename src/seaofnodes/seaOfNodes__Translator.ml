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

(* Describe the nature of the next instruction: directly following the current instruction or jump or end (after a return) *)
type successor = Next | Jump | End

module TranslatorState = struct
  open Monad.State.Infix
  module IMap = Map.Make (Int)
  module ISet = Set.Make (Int)

  type t =
    { stack: Data.t list
    ; reg_map: Region.t IMap.t
    ; count: int
    ; jopcodes: JCode.jopcodes
    ; pc: int
    ; to_visit: ISet.t
    ; son: Son.t }

  type 'a monad = (t, 'a) Monad.State.t

  let initial jopcodes =
    { stack= []
    ; reg_map= IMap.singleton 0 (Region.Region []) (* to:do fix this *)
    ; count= 1000
    ; jopcodes
    ; pc= 0
    ; to_visit= ISet.empty
    ; son= Son.empty }

  let return = Monad.State.return

  let push_stack x = Monad.State.modify (fun g -> {g with stack= x :: g.stack})

  let pop_stack () =
    let* g = Monad.State.get () in
    match g.stack with
    | [] ->
        assert false
    | x :: s ->
        Monad.State.set {g with stack= s} >> Monad.State.return x

  let fresh () =
    let* g = Monad.State.get () in
    let* () = Monad.State.set {g with count= g.count + 1} in
    Monad.State.return g.count

  let get_pc () =
    let* g = Monad.State.get () in
    Monad.State.return @@ g.pc

  let get_son () =
    let* g = Monad.State.get () in
    Monad.State.return @@ g.son

  let insert_son_node id node = Monad.State.modify (fun g -> {g with son= Son.add id node g.son})

  let get_current_instruction () =
    let* g = Monad.State.get () in
    Monad.State.return g.jopcodes.(g.pc)

  let goto pc = Monad.State.modify (fun g -> {g with pc})

  let get_region_at pc =
    let* g = Monad.State.get () in
    Monad.State.return @@ snd @@ IMap.find_last (fun k -> k <= pc) g.reg_map

  let get_current_region () =
    let* g = Monad.State.get () in
    get_region_at g.pc

  let get_next_jump () =
    let* g = Monad.State.get () in
    (* Find and remove lowest pc in the set *)
    let pc = ISet.find_first_opt (Fun.const true) g.to_visit in
    match pc with
    | None ->
        return None
    | Some pc ->
        let to_visit = ISet.remove pc g.to_visit in
        let* () = Monad.State.set {g with to_visit} in
        return @@ Some pc

  let add_to_workset pc =
    let* g = Monad.State.get () in
    Monad.State.set {g with to_visit= ISet.add pc g.to_visit}

  let register_region id region =
    Monad.State.modify (fun g -> {g with reg_map= IMap.add id region g.reg_map})

  let lookup_data n =
    let* g = get_son () in
    match Son.find n g with Node.Data d -> return d | _ -> assert false

  let add_precessor predecessors cr pc =
    let new_cr' = Region.Region (Jump cr :: predecessors) in
    register_region (pc + 1) new_cr'
end

(** Translate one opcode *)
let translate_jopcode (op : JCode.jopcode) : successor TranslatorState.monad =
  let open Monad.State.Infix in
  let open TranslatorState in
  match op with
  | OpLoad (_, n) ->
      (* Get the data from the graph *)
      let* data = lookup_data n in
      let* () = push_stack data in
      return Next
  | OpAdd _ ->
      let* operand1 = pop_stack () in
      let* operand2 = pop_stack () in
      let node = Data.binop Binop.Add operand1 operand2 in
      let* () = push_stack node in
      return Next
  | OpStore (_, id) ->
      (* Use the bytecode id *)
      let* operand = pop_stack () in
      let* () = insert_son_node id (Node.Data operand) in
      return Next
  | OpConst (`Int n) ->
      let node = Data.const (Int32.to_int n) in
      let* () = push_stack node in
      return Next
  | OpConst (`Byte n) ->
      let node = Data.const n in
      let* () = push_stack node in
      return Next
  | OpReturn _ ->
      let* operand = pop_stack () in
      let* region = get_current_region () in
      let node = Control.Return {region; operand} in
      (* Control nodes need to be in the graph *)
      let* id = fresh () in
      let* () = insert_son_node id (Node.Control node) in
      return End
  | OpIf (`Eq, offset) ->
      let* cr = get_current_region () in
      let* operand = pop_stack () in
      let r1 = Region.Region [Branch (Branch.IfT (Cond.Cond {region= cr; operand}))] in
      let r2 = Region.Region [Branch (Branch.IfF (Cond.Cond {region= cr; operand}))] in
      let* pc = get_pc () in
      let* () = register_region (pc + offset) r1 in
      let* () = register_region (pc + 1) r2 in
      let* () = add_to_workset (pc + offset) in
      return Next
  | OpGoto offset ->
      let* cr = get_current_region () in
      let r1 = Region.Region [Jump cr] in
      let* pc = get_pc () in
      let* () = register_region (pc + offset) r1 in
      let* () = add_to_workset (pc + offset) in
      return Jump
  | _ ->
      return Next

let rec translate_state () =
  let open Monad.State.Infix in
  let open TranslatorState in
  let* jopcode = get_current_instruction () in
  let* succ = translate_jopcode jopcode in
  match succ with
  | Next ->
      (* If necessary, add a jump node *)
      let* pc = get_pc () in
      let* cr = get_region_at pc in
      let* cr' = get_region_at (pc + 1) in
      let* () =
        if cr != cr' then
          let (Region.Region predecessors) = cr' in
          (* add_precessor *)
          add_precessor predecessors cr pc
        else return ()
      in
      let* () = goto (pc + 1) in
      translate_state ()
  | Jump ->
      let* pc = get_next_jump () in
      let* () = goto (Option.get pc) in
      translate_state ()
  | End -> (
      let* next_jump = get_next_jump () in
      match next_jump with
      | None ->
          Monad.State.return ()
      | Some pc ->
          let* () = goto pc in
          translate_state () )

(** Translate opcodes *)
let translate_jopcodes (ops : JCode.jopcodes) =
  let g = Monad.State.exec (translate_state ()) (TranslatorState.initial ops) in
  g.son
