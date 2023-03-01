open SeaOfNodes__Type

module TranslatorState : sig
  type t = {stack: Data.t list; region: Region.t; count: int}

  type 'a monad = (t, 'a) Monad.State.t

  val initial : t

  val push_stack : Data.t -> unit monad

  val pop_stack : unit -> Data.t monad

  val fresh : unit -> int monad

  val get_current_region : unit -> Region.t monad
end = struct
  open Monad.State.Infix

  type t = {stack: Data.t list; region: Region.t; count: int}

  type 'a monad = (t, 'a) Monad.State.t

  let initial = {stack= []; region= Region.Region []; count= 1000}

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

  let get_current_region () =
    let* g = Monad.State.get () in
    Monad.State.return g.region
end

(* Translate one opcode *)
let translate_jopcode (g : Node.t IMap.t) (op : JCode.jopcode) :
    (TranslatorState.t, Node.t IMap.t) Monad.State.t =
  let open Monad.State.Infix in
  match op with
  | OpLoad (_, n) ->
      (* Get the data from the graph *)
      let data = match IMap.find n g with Node.Data d -> d | _ -> assert false in
      let* _ = TranslatorState.push_stack data in
      Monad.State.return g
  | OpAdd _ ->
      let* operand1 = TranslatorState.pop_stack () in
      let* operand2 = TranslatorState.pop_stack () in
      let node = Data.binop Binop.Add operand1 operand2 in
      let* _ = TranslatorState.push_stack node in
      Monad.State.return g
  | OpStore (_, id) ->
      (* Use the bytecode id *)
      let* operand = TranslatorState.pop_stack () in
      Monad.State.return @@ IMap.add id (Node.Data operand) g
  | OpConst (`Int n) ->
      let node = Data.const (Int32.to_int n) in
      let* _ = TranslatorState.push_stack node in
      Monad.State.return g
  | OpConst (`Byte n) ->
      let node = Data.const n in
      TranslatorState.push_stack node >> Monad.State.return g
  | OpReturn _ ->
      let* operand = TranslatorState.pop_stack () in
      let* region = TranslatorState.get_current_region () in
      let node = Control.Return {region; operand} in
      (* Control nodes need to be in the graph *)
      let* id = TranslatorState.fresh () in
      Monad.State.return @@ IMap.add id (Node.Control node) g
  | _ ->
      Monad.State.return g

let translate_jopcodes (ops : JCode.jopcodes) =
  (* Translate opcodes *)
  Monad.State.exec
    (Monad.State.fold_leftM translate_jopcode IMap.empty (Array.to_list @@ ops))
    TranslatorState.initial
