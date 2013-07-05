module type S = 
sig 
  type t
end 



module type GenericSetSig =
sig
  type f
  type t
  type elt = int * f

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




module Make ( S : sig type t end ) =
struct
  type f = S.t
  type elt = int * f
  type t = elt Ptmap.t

  let empty = Ptmap.empty
  let is_empty = Ptmap.is_empty
  let mem e m = Ptmap.mem (fst e) m
  let add e m = Ptmap.add (fst e) e m
  let singleton e = Ptmap.add (fst e) e empty
  let remove e m = Ptmap.remove (fst e) m
  let union m1 m2 = Ptmap.merge_first m1 m2
  let diff m1 m2 = Ptmap.diff (fun _ _ -> true) m1 m2
  let equal m1 m2 = 0 == (compare m1 m2)
  let elements m = Ptmap.fold (fun _ e l ->  e :: l) m []
  let cardinal m = Ptmap.cardinal m
  let iter f m = Ptmap.iter (fun _ e -> f e) m
  let fold f m b = Ptmap.fold (fun _ e b -> f e b) m b
  let exists f m = Ptmap.exists (fun _ e -> f e) m
  let filter f m = Ptmap.filter f m
  let inter m1 m2 = Ptmap.inter m1 m2
  let of_array l = Array.fold_right add l empty		     
  let of_list l = List.fold_right add l empty		     

  (* val partition : ('a -> bool) -> 'a t -> 'a t * 'a t *)
  (* val choose_and_remove : *)
end


