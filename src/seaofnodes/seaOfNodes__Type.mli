module type IDENTIFIED = sig
  type elt

  type t = int * elt

  val hash : t -> int

  val equal : t -> t -> bool

  val get_value : t -> elt

  val get_id : t -> int
end

module Identified : functor
  (M : sig
     type t

     val hash : t -> int

     val equal : t -> t -> bool
   end)
  -> sig
  type elt = M.t

  type t = int * elt

  val hash : t -> int

  val equal : t -> t -> bool

  val get_value : t -> elt

  val get_id : t -> int
end

module Binop : sig
  type t = Add

  val hash : t -> int
end

module rec Data : sig
  type t = private Const of int | BinOp of {op: Binop.t; operand1: t; operand2: t} | Phi of Phi.t

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

module IMap : sig
  type key = Int.t

  type 'a t = 'a Stdlib__Map.Make(Int).t

  val empty : 'a t

  val is_empty : 'a t -> bool

  val mem : key -> 'a t -> bool

  val add : key -> 'a -> 'a t -> 'a t

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  val singleton : key -> 'a -> 'a t

  val remove : key -> 'a t -> 'a t

  val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t

  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t

  val cardinal : 'a t -> int

  val bindings : 'a t -> (key * 'a) list

  val min_binding : 'a t -> key * 'a

  val min_binding_opt : 'a t -> (key * 'a) option

  val max_binding : 'a t -> key * 'a

  val max_binding_opt : 'a t -> (key * 'a) option

  val choose : 'a t -> key * 'a

  val choose_opt : 'a t -> (key * 'a) option

  val split : key -> 'a t -> 'a t * 'a option * 'a t

  val find : key -> 'a t -> 'a

  val find_opt : key -> 'a t -> 'a option

  val find_first : (key -> bool) -> 'a t -> key * 'a

  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option

  val find_last : (key -> bool) -> 'a t -> key * 'a

  val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t

  val to_seq : 'a t -> (key * 'a) Seq.t

  val to_rev_seq : 'a t -> (key * 'a) Seq.t

  val to_seq_from : key -> 'a t -> (key * 'a) Seq.t

  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t

  val of_seq : (key * 'a) Seq.t -> 'a t
end

type son = Node.t IMap.t
