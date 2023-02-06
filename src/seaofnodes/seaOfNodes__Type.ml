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
