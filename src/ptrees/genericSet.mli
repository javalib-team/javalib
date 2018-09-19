(*
 * Copyright (C) 2013, Pierre Vittet (INRIA)
 *               2016, David Pichardie, Laurent Guillo  
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1, with the special exception on linking described in file
 * LICENSE.

 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *)


(**Provide a way to create GenericSet specifying how to hash.*)

(** This module type must be implemented to obtain an instantiated GenericSet.*)
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
  val subset : t -> t -> bool
end


module Make (El : S) : GenericSetSig with type elt= El.t
