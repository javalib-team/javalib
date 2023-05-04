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
 
module Cfg : sig
  type t
  type predecessor =
    | Jump of int
    | IfT of int
    | IfF of int
    | Implicit of int
        (** [Implicit pc] means that [pc] and [pc + 1] denote two different
            regions, and [pc] is not a control-flow instruction. *)

  val get_source : predecessor -> int

  val find : int -> t -> predecessor list
  val mem : int -> t -> bool
  val empty : t
  val iter : (int -> predecessor list -> unit) -> t -> unit
  val fold : (int -> predecessor list -> 'a -> 'a) -> t -> 'a -> 'a

  val next_pc : int -> JCode.jopcodes -> int
end

val build_cfg : JCode.jopcode array -> Cfg.t
