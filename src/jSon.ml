module Binop = struct
  type t =
    | Add
end

module rec Data : sig
  type t =
    | Const of int
    | BinOp of { op : Binop.t; operand1 : t; operand2 : t }
    | Phi of Phi.t

  val to_dot : Buffer.t -> t -> unit
end = struct
  type t =
    | Const of int
    | BinOp of { op : Binop.t; operand1 : t; operand2 : t }
    | Phi of Phi.t

  let to_dot buff data =
    let node_name = Printf.sprintf "node_%d" (Hashtbl.hash data) in
    match data with
    | Const n ->
      Buffer.add_string buff @@ Printf.sprintf "%s [ label = \"Const %d\" ]\n" node_name n;
    | BinOp {op = Binop.Add; operand1 = op1; operand2 = op2} ->
      let op1_name = Printf.sprintf "node_%d" (Hashtbl.hash op1) in
      let op2_name = Printf.sprintf "node_%d" (Hashtbl.hash op2) in
      Buffer.add_string buff @@ Printf.sprintf "%s [ label = \"Add\" ]\n" node_name;
      Buffer.add_string buff @@ Printf.sprintf "%s -> %s\n" node_name op1_name;
      Buffer.add_string buff @@ Printf.sprintf "%s -> %s\n" node_name op2_name;
      Data.to_dot buff op1;
      Data.to_dot buff op2;
    | _ -> ()
end

and Region : sig
  type t = | Region of { jumps : Jump.t list; branches : Branch.t list }
end = struct
  type t = | Region of { jumps : Jump.t list; branches : Branch.t list }
end

and Phi : sig
  type t = | Phi of { region : Region.t; operands : Data.t list }
end = struct
  type t = | Phi of { region : Region.t; operands : Data.t list }
end

and Jump : sig
  type t = | Jump of Region.t
end = struct
  type t = | Jump of Region.t
end

and Cond : sig
  type t = | Cond of { region : Region.t; operand : Data.t }
end = struct
  type t = | Cond of { region : Region.t; operand : Data.t }
end

and Branch : sig
  type t =
    | IfT of Cond.t
    | IfF of Cond.t
end = struct
  type t =
    | IfT of Cond.t
    | IfF of Cond.t
end

module Control : sig
  type t =
    | Jump of Jump.t
    | Cond of Cond.t
    | Return of { region : Region.t; operand : Data.t}

  val to_dot : Buffer.t -> t -> unit
end = struct
  type t =
    | Jump of Jump.t
    | Cond of Cond.t
    | Return of { region : Region.t; operand : Data.t}

  let to_dot buff control =
    match control with
    | Return {region = _; operand} ->
      let node_name = Printf.sprintf "node_%d" (Hashtbl.hash control) in
      let op_name = Printf.sprintf "node_%d" (Hashtbl.hash operand) in
      Buffer.add_string buff @@ Printf.sprintf "%s [ label = \"Return\" ]\n" node_name;
      Buffer.add_string buff @@ Printf.sprintf "%s -> %s\n" node_name op_name;
      Data.to_dot buff operand
    | _ -> ()
end

module Node : sig
  type t =
    | Data of Data.t
    | Region of Region.t
    | Control of Control.t
    | Branch of Branch.t

  val to_dot : Buffer.t -> t -> unit
end = struct
  type t =
    | Data of Data.t
    | Region of Region.t
    | Control of Control.t
    | Branch of Branch.t

  let to_dot buff node =
    Buffer.add_string buff "digraph G {\nnode [shape=record];\ncolor = black;\n";
    begin
    match node with
    | Data data -> Data.to_dot buff data
    | Control control -> Control.to_dot buff control
    | _ -> assert false
    end;
    Buffer.add_string buff "}"
end

type id = int
module IMap = Map.Make(Int)
type son = Node.t IMap.t


module Translator : sig
  val translate_jopcodes : JCode.jopcodes -> son
end = struct
  (* JVM Stack *)
  let stack = ref []
  let push_stack x = stack := x :: !stack
  let pop_stack () =
    match !stack with
    | [] -> assert false
    | x :: s -> stack := s; x

  (* We suppose that there is only one region *)
  let current_region = ref @@ Region.Region {jumps = []; branches = []}

  (* fresh name *)
  let count = ref 0
  let fresh () = incr count; !count

  (* Translate one opcode *)
  let translate_jopcode (g: Node.t IMap.t) (op: JCode.jopcode) : Node.t IMap.t =
    match op with
    | JCode.OpLoad (_, n) ->
      (* Get the data from the graph *)
      let data =
        match IMap.find n g with
        | Node.Data d -> d
        | _ -> assert false
        in
      push_stack data;
      g
    | JCode.OpAdd _ ->
      let operand1 = pop_stack () in
      let operand2 = pop_stack () in
      let node = Data.BinOp {op = Binop.Add; operand1; operand2} in
      push_stack node;
      g
    | JCode.OpStore (_, id) ->
      (* Use the bytecode id *)
      let operand = pop_stack () in
      IMap.add id (Node.Data operand) g

    | JCode.OpConst (`Int n) ->
      let node = Data.Const (Int32.to_int n) in
      push_stack node;
      g
    | JCode.OpConst (`Byte n) ->
      let node = Data.Const n in
      push_stack node;
      g

    | JCode.OpReturn _ ->
      let operand = pop_stack () in
      let region = !current_region in
      let node = Control.Return { region; operand } in
      (* Control nodes need to be in the graph *)
      let id = fresh () in
      IMap.add id (Node.Control node) g
    | _ -> g

  let translate_jopcodes (ops: JCode.jopcodes) =
    (* Initialize mutable state *)
    stack := [];
    count := 1000;
    current_region := Region.Region {jumps = []; branches = []};

    (* Translate opcodes *)
    Array.fold_left translate_jopcode IMap.empty ops
end
