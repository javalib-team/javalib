module Binop : sig
  type t = Add
  val hash : t -> int
end = struct
  type t = Add
  let hash _ = 1
end


module rec JumpSet : GenericSet.GenericSetSig = GenericSet.Make(Jump)
and BranchSet : GenericSet.GenericSetSig = GenericSet.Make(Branch)

and Data : sig
  type t = Const of int | BinOp of {op: Binop.t; operand1: t; operand2: t} | Phi of Phi.t

  val hash : t -> int
  val pp_dot : out_channel -> t -> unit
end = struct
  type t = Const of int | BinOp of {op: Binop.t; operand1: t; operand2: t} | Phi of Phi.t

  let hash = function
    | Const n -> n
    | BinOp {op; operand1; operand2} ->
      Binop.hash op lxor Data.hash operand1 lxor Data.hash operand2
    | Phi phi -> Phi.hash phi

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
  type t = Region of {jumps: JumpSet.t; branches: BranchSet.t}
  val hash : t -> int
end = struct
  type t = Region of {jumps: JumpSet.t; branches: BranchSet.t}
  let hash (Region {jumps; branches; _}) = Hashtbl.hash jumps lxor Hashtbl.hash branches
end

and Phi : sig
  type t = Phi of {region: Region.t; operands: Data.t list}
  val hash : t -> int
end = struct
  type t = Phi of {region: Region.t; operands: Data.t list}
  let hash (Phi {region; operands}) =
    Region.hash region lxor List.fold_left ( lxor ) 1 (List.map Data.hash operands)
end

and Jump : sig
  type t = Jump of Region.t
  val get_hash : t -> int
end = struct
  type t = Jump of Region.t
  let get_hash (Jump r) = Region.hash r
end

and Cond : sig
  type t = Cond of {region: Region.t; operand: Data.t}
  val hash : t -> int
end = struct
  type t = Cond of {region: Region.t; operand: Data.t}
  let hash (Cond {region; operand}) = Region.hash region lxor Data.hash operand
end

and Branch : sig
  type t = IfT of Cond.t | IfF of Cond.t
  val get_hash : t -> int
end = struct
  type t = IfT of Cond.t | IfF of Cond.t
  let get_hash = function
    | IfT cond -> Cond.hash cond
    | IfF cond -> Cond.hash cond
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
