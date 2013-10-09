module type S = 
sig 
  type t
  val get_hash : t -> int
end 



(** Common signature of map modules based on Ptrees library. *)
module type GenericMapSig =
sig
  type key 
  type 'a t = (key * 'a) Ptmap.t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val cardinal : 'a t -> int
  val modify: key -> ('a option -> 'a) -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    (** [merge f m1 m2] returns a map that has the bindings of [m1] and [m2] and
        which binds [k] to [f d1 d2] if [m1] and [m2] binds the same [k] to
        different [d1] and [d2], respectively. If [d1] equals [d2], [f d1 d2] is
        supposed to return [d1]. *)
  val choose_and_remove : 'a t -> key * 'a * 'a t
    (** [choose_and_remove t] returns (i,d,t') such that [t'] equals to [remove
        i t] and [d] equals to [find i t].

        @raise Not_found if [t] is empty. *)
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filteri : (key -> 'a -> bool) -> 'a t -> 'a t
  val key_elements : 'a t -> key list
  val value_elements : 'a t -> 'a list
  val elements : 'a t -> (key * 'a) list
end


module Make (El : S) : GenericMapSig with type key = El.t
