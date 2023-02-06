(** The module [Identified] is here to hide the tags in the SON types *)
(** We use a Weak set to remember previously created values,
    in which we store elements of type Identified t *)
(** Identified types are just an element with an integer *)
module type IDENTIFIED = sig
  type elt
  type t = int * elt
  val hash : t -> int
  val equal : t -> t -> bool
  val get_value : t -> elt
  val get_id : t -> int
end
module Identified (M : sig
    type t
    val hash : t -> int
    val equal : t -> t -> bool
  end) : IDENTIFIED with type elt = M.t = struct
  type t = int * M.t
  type elt = M.t
  let hash (_, x) = M.hash x
  let equal (_, x) (_, y) = M.equal x y

  let get_value (_, x) = x
  let get_id (id, _) = id
end


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
  (** It is possible to pattern match elements of type t, *)
  (** but you must use the smart constructors to build values *)
  type t = private Const of int | BinOp of {op: Binop.t; operand1: t; operand2: t} | Phi of Phi.t

  val hash : t -> int
  val equal : t -> t -> bool

  val pp_dot : out_channel -> t -> unit

  (** Constructors *)
  val const : int -> t
  val binop : Binop.t -> t -> t -> t
  val phi : Phi.t -> t

  (** Unique id of the node *)
  val get_id : t -> int
end = struct
  type t = Const of int | BinOp of {op: Binop.t; operand1: t; operand2: t} | Phi of Phi.t

  let hash = function
    | Const n -> n
    | BinOp {op; operand1; operand2} ->
      Binop.hash op lxor Data.hash operand1 lxor Data.hash operand2
    | Phi phi -> Phi.hash phi

  let equal t t' =
    match t, t' with
    | Const n, Const n' -> n = n'
    | BinOp {op; operand1; operand2}, BinOp {op = op'; operand1 = operand1'; operand2 = operand2'} ->
      (* We assume that binops and datas are hashconsed and use the physical equality *)
      op == op' && operand1 == operand1' && operand2 == operand2'
    | Phi phi, Phi phi' -> phi == phi'
    | _ -> false


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

  (* Data.t with tags *)
  module Id = Identified(Data)
  (* Weak set to remember previously created values *)
  module W = Weak.Make(Id)

  let nodes = W.create 5003
  let cpt = ref 1

  (** if d is on the set, we return the previously constructed element, *)
  (** otherwise we add it to the set and increment cpt *)
  let add_or_find d =
    let n0 = (!cpt, d) in
    let n = W.merge nodes n0 in
    if n == n0 then incr cpt;
    Id.get_value n

  let const n =
    add_or_find @@ Const n

  let binop op operand1 operand2 =
    add_or_find @@ BinOp {op; operand1; operand2}

  let phi phi =
    add_or_find @@ Phi phi

  let get_id data =
    let n0 = (0, data) in
    Id.get_id @@ W.find nodes n0
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
