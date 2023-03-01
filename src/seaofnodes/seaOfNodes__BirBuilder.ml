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

open SeaOfNodes__Type
open Bir

let rec data_to_bir (data : Data.t) =
  match data with
  | Data.Const {value= n} ->
      Const (`Int (Int32.of_int n))
  | Data.BinOp {op= Binop.Add; operand1; operand2} ->
      Binop (Add `Int2Bool, data_to_bir operand1, data_to_bir operand2)
  | _ ->
      failwith "todo"

let control_to_bir (control : Control.t) =
  match control with
  | Return {operand} ->
      Return (Some (data_to_bir operand))
  | _ ->
      failwith "todo"

let node_to_bir (node : Node.t) =
  match node with Control control -> control_to_bir control | _ -> failwith "todo"
