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

module TranslatorState : sig
  type t = {stack: SeaOfNodes__Type.Data.t list; region: SeaOfNodes__Type.Region.t; count: int}

  type 'a monad = (t, 'a) Monad.State.t

  val initial : t

  val push_stack : SeaOfNodes__Type.Data.t -> unit monad

  val pop_stack : unit -> SeaOfNodes__Type.Data.t monad

  val fresh : unit -> int monad

  val get_current_region : unit -> SeaOfNodes__Type.Region.t monad
end

val translate_jopcode :
     SeaOfNodes__Type.Son.t
  -> JCode.jopcode
  -> (TranslatorState.t, SeaOfNodes__Type.Son.t) Monad.State.t

val translate_jopcodes : JCode.jopcodes -> SeaOfNodes__Type.Son.t
