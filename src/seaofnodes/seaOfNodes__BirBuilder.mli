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
 
module IMap = JLib.IMap
val translate_son : SeaOfNodes__Type.Son.t -> Bir.block IMap.t
