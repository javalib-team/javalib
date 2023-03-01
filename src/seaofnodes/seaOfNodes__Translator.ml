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

module TranslatorState : sig
  type t =
    { stack: Data.t list
    ; region: Region.t
    ; count: int
    ; jopcodes: JCode.jopcodes
    ; pc: int
    ; son: Son.t }

  type 'a monad = (t, 'a) Monad.State.t

  val initial : JCode.jopcodes -> t

  val push_stack : Data.t -> unit monad

  val pop_stack : unit -> Data.t monad

  val fresh : unit -> int monad

  val get_son : unit -> Son.t monad

  val set_son : Son.t -> unit monad

  val next_instructions : unit -> int list monad

  val get_current_instruction : unit -> JCode.jopcode monad

  val jump : int -> unit monad

  val get_current_region : unit -> Region.t monad
end = struct
  open Monad.State.Infix

  type t =
    { stack: Data.t list
    ; region: Region.t
    ; count: int
    ; jopcodes: JCode.jopcodes
    ; pc: int
    ; son: Son.t }

  type 'a monad = (t, 'a) Monad.State.t

  let initial jopcodes =
    { stack= []
    ; region= Region.Region []
    ; count= 1000
    ; jopcodes
    ; pc= 0
    ; son= Son.empty }

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
    Monad.State.set {g with count= g.count + 1} >> Monad.State.return g.count

  let get_son () =
    let* g = Monad.State.get () in
    Monad.State.return @@ g.son

  let set_son son =
    Monad.State.modify (fun g -> {g with son})

  let next_instructions () =
    let* g = Monad.State.get () in
    Monad.State.return @@ JCode.succs g.jopcodes g.pc

  let get_current_instruction () =
    let* g = Monad.State.get () in
    Monad.State.return g.jopcodes.(g.pc)

  let jump pc = Monad.State.modify (fun g -> {g with pc})

  let get_current_region () =
    let* g = Monad.State.get () in
    Monad.State.return g.region
end

(** Translate one opcode *)
let translate_jopcode (op : JCode.jopcode) : unit TranslatorState.monad =
  let open Monad.State.Infix in
  let open TranslatorState in
  match op with
  | OpLoad (_, n) ->
      (* Get the data from the graph *)
      let* g = get_son () in
      let data =
        match Son.find n g with Node.Data d -> d | _ -> assert false
      in
      push_stack data
  | OpAdd _ ->
      let* operand1 = pop_stack () in
      let* operand2 = pop_stack () in
      let node = Data.binop Binop.Add operand1 operand2 in
      push_stack node
  | OpStore (_, id) ->
      (* Use the bytecode id *)
      let* operand = pop_stack () in
      let* g = get_son () in
      let new_son = Son.add id (Node.Data operand) g in
      set_son new_son
  | OpConst (`Int n) ->
      let node = Data.const (Int32.to_int n) in
      push_stack node
  | OpConst (`Byte n) ->
      let node = Data.const n in
      push_stack node
  | OpReturn _ ->
      let* operand = pop_stack () in
      let* region = get_current_region () in
      let node = Control.Return {region; operand} in
      (* Control nodes need to be in the graph *)
      let* id = fresh () in
      let* g = get_son () in
      let new_son = Son.add id (Node.Control node) g in
      set_son new_son
  | _ ->
      Monad.State.return ()

let rec translate_state () =
  let open Monad.State.Infix in
  let open TranslatorState in
  let* jopcode = get_current_instruction () in
  let* _ = translate_jopcode jopcode in
  let* succs = next_instructions () in
  match succs with
  | [] -> Monad.State.return ()
  | pc::_ ->
    let* _ = jump pc in
    translate_state ()

(** Translate opcodes *)
let translate_jopcodes (ops : JCode.jopcodes) =
  let g = Monad.State.exec (translate_state ()) (TranslatorState.initial ops) in
  g.son
