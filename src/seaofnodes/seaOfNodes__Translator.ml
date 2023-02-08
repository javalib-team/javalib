open SeaOfNodes__Type

module Stack = Monad.State

let push_stack x = Stack.modify (fun l -> x :: l)

let pop_stack () =
  let open Monad.State.Infix in
  let* stack = Stack.get () in
  match stack with [] -> assert false | x :: s -> Stack.set s >> Stack.return x


(* We suppose that there is only one region *)
let current_region = ref @@ Region.Region []

(* fresh name *)
let count = ref 0

let fresh () =
  incr count ;
  !count


(* Translate one opcode *)
let translate_jopcode (g : Node.t IMap.t) (op : JCode.jopcode) :
  (Data.t list, Node.t IMap.t) Stack.t =
  let open Monad.State.Infix in
  match op with
  | OpLoad (_, n) ->
    (* Get the data from the graph *)
    let data = match IMap.find n g with Node.Data d -> d | _ -> assert false in
    let* _ = push_stack data in
    Stack.return g
  | OpAdd _ ->
    let* operand1 = pop_stack () in
    let* operand2 = pop_stack () in
    let node = Data.binop Binop.Add operand1 operand2 in
    let* _ = push_stack node in
    Stack.return g
  | OpStore (_, id) ->
    (* Use the bytecode id *)
    let* operand = pop_stack () in
    Stack.return @@ IMap.add id (Node.Data operand) g
  | OpConst (`Int n) ->
    let node = Data.const (Int32.to_int n) in
    let* _ = push_stack node in
    Stack.return g
  | OpConst (`Byte n) ->
    let node = Data.const n in
    push_stack node >> Stack.return g
  | OpReturn _ ->
    let* operand = pop_stack () in
    let region = !current_region in
    let node = Control.Return {region; operand} in
    (* Control nodes need to be in the graph *)
    let id = fresh () in
    Stack.return @@ IMap.add id (Node.Control node) g
  | _ ->
    Stack.return g


let translate_jopcodes (ops : JCode.jopcodes) =
  (* Initialize mutable state *)
  count := 1000 ;
  current_region := Region.Region [] ;
  (* Translate opcodes *)
  Stack.exec (Stack.fold_leftM translate_jopcode IMap.empty (Array.to_list @@ ops)) []
