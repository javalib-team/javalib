module Binop : sig
  type t = Add

  val hash : t -> int
end

module rec Data : sig
  type t = private
    | Const of {unique: int; value: int}
    | BinOp of {unique: int; op: Binop.t; operand1: t; operand2: t}
    | Phi of Phi.t

  val hash : t -> int

  val equal : t -> t -> bool

  val pp_dot : out_channel -> t -> unit

  val const : int -> t

  val binop : Binop.t -> t -> t -> t

  val phi : Phi.t -> t

  val get_id : t -> int
end

and Region : sig
  type predecessor = Jump of Region.t | Branch of Branch.t

  type t = Region of predecessor list

  val hash : t -> int
end

and Phi : sig
  type t = Phi of {region: Region.t; operands: Data.t list}

  val hash : t -> int
end

and Cond : sig
  type t = Cond of {region: Region.t; operand: Data.t}

  val hash : t -> int
end

and Branch : sig
  type t = IfT of Cond.t | IfF of Cond.t

  val get_hash : t -> int
end

module Control : sig
  type t = Jump of Region.t | Cond of Cond.t | Return of {region: Region.t; operand: Data.t}

  val pp_dot : out_channel -> t -> unit
end

module Node : sig
  type t = Data of Data.t | Region of Region.t | Control of Control.t | Branch of Branch.t

  val pp_dot : out_channel -> t -> unit
end

type id = int

module Son : sig
  type t
  val find : id -> t -> Node.t
  val add : id -> Node.t -> t -> t
  val empty : t
  val bindings : t -> (id * Node.t) list
end
