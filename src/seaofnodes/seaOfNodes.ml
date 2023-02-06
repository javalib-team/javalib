module Binop = struct
  type t = Add
end

module rec Data : sig
  type t = Const of int | BinOp of {op: Binop.t; operand1: t; operand2: t} | Phi of Phi.t

  val pp_dot : out_channel -> t -> unit
end = struct
  type t = Const of int | BinOp of {op: Binop.t; operand1: t; operand2: t} | Phi of Phi.t

  let pp fmt data = Printf.fprintf fmt "node_%d" (Hashtbl.hash data)

  let rec pp_dot fmt data =
    match data with
    | Const n ->
        Printf.fprintf fmt "%a [ label = \"Const %d\" ]\n" pp data n
    | BinOp {op= Binop.Add; operand1= op1; operand2= op2} ->
        Printf.fprintf fmt "%a [ label = \"Add\" ]\n" pp data ;
        Printf.fprintf fmt "%a -> %a\n" pp data pp op1 ;
        Printf.fprintf fmt "%a -> %a\n" pp data pp op2 ;
        pp_dot fmt op1 ;
        pp_dot fmt op2
    | _ ->
        ()
end

and Region : sig
  type t = Region of {jumps: Jump.t list; branches: Branch.t list}
end = struct
  type t = Region of {jumps: Jump.t list; branches: Branch.t list}
end

and Phi : sig
  type t = Phi of {region: Region.t; operands: Data.t list}
end = struct
  type t = Phi of {region: Region.t; operands: Data.t list}
end

and Jump : sig
  type t = Jump of Region.t
end = struct
  type t = Jump of Region.t
end

and Cond : sig
  type t = Cond of {region: Region.t; operand: Data.t}
end = struct
  type t = Cond of {region: Region.t; operand: Data.t}
end

and Branch : sig
  type t = IfT of Cond.t | IfF of Cond.t
end = struct
  type t = IfT of Cond.t | IfF of Cond.t
end

module Control : sig
  type t = Jump of Jump.t | Cond of Cond.t | Return of {region: Region.t; operand: Data.t}

  val pp_dot : out_channel -> t -> unit
end = struct
  type t = Jump of Jump.t | Cond of Cond.t | Return of {region: Region.t; operand: Data.t}

  let pp fmt data = Printf.fprintf fmt "node_%d" (Hashtbl.hash data)

  let pp_dot fmt control =
    match control with
    | Return {region= _; operand} ->
        Printf.fprintf fmt "%a [ label = \"Return\" ]\n" pp control ;
        Printf.fprintf fmt "%a -> %a\n" pp control pp operand ;
        Data.pp_dot fmt operand
    | _ ->
        ()
end

module Node : sig
  type t = Data of Data.t | Region of Region.t | Control of Control.t | Branch of Branch.t

  (* val to_dot : Buffer.t -> t -> unit *)
  val pp_dot : out_channel -> t -> unit
end = struct
  type t = Data of Data.t | Region of Region.t | Control of Control.t | Branch of Branch.t

  let pp_dot fmt node =
    Printf.fprintf fmt "digraph G {\nnode [shape=record];\ncolor = black;\n" ;
    ( match node with
    | Data data ->
        Data.pp_dot fmt data
    | Control control ->
        Control.pp_dot fmt control
    | _ ->
        assert false ) ;
    Printf.fprintf fmt "}"
end

type id = int

module IMap = Map.Make (Int)

type son = Node.t IMap.t

module Translator : sig
  val translate_jopcodes : JCode.jopcodes -> son
end = struct
  (* JVM Stack *)
  module Stack = Monad.State

  let push_stack x = Stack.modify (fun l -> x :: l)

  let pop_stack () =
    let open Monad.State.Infix in
    let* stack = Stack.get () in
    match stack with [] -> assert false | x :: s -> Stack.set s >> Stack.return x


  (* We suppose that there is only one region *)
  let current_region = ref @@ Region.Region {jumps= []; branches= []}

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
        let node = Data.BinOp {op= Binop.Add; operand1; operand2} in
        let* _ = push_stack node in
        Stack.return g
    | OpStore (_, id) ->
        (* Use the bytecode id *)
        let* operand = pop_stack () in
        Stack.return @@ IMap.add id (Node.Data operand) g
    | OpConst (`Int n) ->
        let node = Data.Const (Int32.to_int n) in
        let* _ = push_stack node in
        Stack.return g
    | OpConst (`Byte n) ->
        let node = Data.Const n in
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
    current_region := Region.Region {jumps= []; branches= []} ;
    (* Translate opcodes *)
    Stack.exec (Stack.fold_leftM translate_jopcode IMap.empty (Array.to_list @@ ops)) []
end
