(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*  - [add] and [merge] functions modified/added by Laurent Hubert (CNRS) *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: ptmap.mli,v 1.8 2008-07-21 14:53:06 filliatr Exp $ i*)

(*s Maps over integers implemented as Patricia trees.
    The following signature is exactly [Map.S with type key = int],
    with the same specifications. *)

module type S = sig
  type (+'a) t

  type key = int

  val empty : 'a t

  val is_empty : 'a t -> bool

  (** [add ~merge:f k d m] returns a map containing the same bindings as
      [m], plus a binding of [k] to [d].  If [k] was already bound to
      [d'] in [m], then the value [f d' d] is added instead of [d].  If
      no merge function is specified, then the previous bindings is
      simply discard. *)
  val add : ?merge:('a -> 'a -> 'a) -> int -> 'a -> 'a t -> 'a t

  val modify : int -> ('a option -> 'a) -> 'a t -> 'a t

  val find : int -> 'a t -> 'a

  val remove : int -> 'a t -> 'a t

  val mem :  int -> 'a t -> bool

  val iter : (int -> 'a -> unit) -> 'a t -> unit

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

  val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  (** [merge f m1 m2] returns a map that has the bindings of [m1] and
      [m2] and which binds [k] to [f d1 d2] if [m1] and [m2] binds the
      same [k] to [d1] and [d2], respectively. *)
  val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

end

include S
