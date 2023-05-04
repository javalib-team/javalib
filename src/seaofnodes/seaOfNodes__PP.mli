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
 
val key : out_channel -> 'a SeaOfNodes__Type.Son.key -> unit
val binop : out_channel -> SeaOfNodes__Type.Binop.t -> unit
val data : out_channel -> SeaOfNodes__Type.Data.t -> unit
val phi : out_channel -> SeaOfNodes__Type.Phi.t -> unit

val predecessor :
  out_channel -> SeaOfNodes__Type.Region.predecessor -> unit

val region : out_channel -> SeaOfNodes__Type.Region.t -> unit
val cond : out_channel -> SeaOfNodes__Type.Cond.t -> unit
val branch : out_channel -> SeaOfNodes__Type.Branch.t -> unit
val control : out_channel -> SeaOfNodes__Type.Control.t -> unit
val son : out_channel -> SeaOfNodes__Type.Son.t -> unit
