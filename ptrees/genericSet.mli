module type S = 
sig 
  type t
  val get_hash : t -> int
end 

module type GenericSetSig =
sig
  type t
  type elt 

  val empty : t
  val is_empty : t -> bool
  val singleton : elt -> t
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val equal : t -> t -> bool
  val elements : t -> elt list
  val cardinal : t -> int
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val inter : t -> t -> t
  val of_list : elt list -> t
  val of_array : elt array -> t
end


module Make (El : S) : GenericSetSig with type elt= El.t
