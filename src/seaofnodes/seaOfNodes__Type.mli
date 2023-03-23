(*
 * This file is part of Javalib
 * Copyright (c)2023 Martin Andrieux (ENS Rennes)
 * Copyright (c)2023 Alban Dutilleul (ENS Rennes)
 * Copyright (c)2023 David Pichardie (Facebook France)
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1, with the special exception on linking described in file
 * LICENSE.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)

module Binop : sig
  type t = Add

  val hash : t -> int
end

module rec Data : sig
  type t = private
    | Const of {unique: int; value: int}
    | BinOp of {unique: int; op: Binop.t; operand1: t Son.key; operand2: t Son.key}
    | Phi of Phi.t

  val hash : t -> int

  val equal : t -> t -> bool

  val const : int -> t

  val binop : Binop.t -> t Son.key -> t Son.key -> t

  val phi : Phi.t -> t

  val get_id : t -> int
end

and Region : sig
  type predecessor = Jump of Region.t Son.key | Branch of Branch.t Son.key

  type t = Region of predecessor list

  val hash : t -> int
end

and Phi : sig
  type t = Phi of {region: Region.t Son.key; operands: Data.t Son.key list}
end

and Cond : sig
  type t = Cond of {region: Region.t Son.key; operand: Data.t Son.key}
end

and Branch : sig
  type t = IfT of Cond.t | IfF of Cond.t
end

and Control : sig
  type t =
    | Jump of Region.t Son.key
    | Cond of Cond.t Son.key
    | Return of {region: Region.t Son.key; operand: Data.t Son.key}
end

and Son : sig
  type t

  type 'a key

  val alloc_data : t -> Data.t -> t * Data.t key

  val alloc_region : t -> Region.t -> t * Region.t key

  val alloc_control : t -> Control.t -> t * Control.t key

  val alloc_branch : t -> Branch.t -> t * Branch.t key

  val get : 'a key -> t -> 'a

  val set : 'a key -> 'a -> t -> t

  val modify : 'a key -> ('a -> 'a) -> t -> t

  val get_id : 'a key -> int

  val add_predecessor: t -> Region.t key -> Region.predecessor -> t

  val empty : t

  val data_nodes : t -> (Data.t Son.key * Data.t) list

  val control_nodes : t -> (Control.t Son.key * Control.t) list

  val unsafe_make_key : int -> Data.t key
end
