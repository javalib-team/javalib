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

let rec elements ty fmt l =
  match l with
  | [] ->
      Printf.fprintf fmt ""
  | [elt] ->
      Printf.fprintf fmt "%a" ty elt
  | elt :: elts ->
      Printf.fprintf fmt "%a; %a" ty elt (elements ty) elts

let list ty fmt l = Printf.fprintf fmt "[%a]" (elements ty) l

let prefixed s pp fmt = Printf.fprintf fmt "%s%a" s pp

let (++) = prefixed

let key fmt key = Printf.fprintf fmt "%d" (Son.get_id key)

let rec binop fmt binop =
  match binop with Binop.Add -> Printf.fprintf fmt "Add"

and data fmt data =
  match data with
  | Data.Const {value} ->
      Printf.fprintf fmt "Const %d" value
  | BinOp {op; operand1; operand2} ->
      Printf.fprintf fmt "BinOp {op: %a; operand1: %a; operand2: %a}" binop op
        ("D" ++ key) operand1 ("D" ++ key) operand2
  | Phi phi_ ->
      Printf.fprintf fmt "%a" phi phi_

and phi fmt (Phi.Phi {region; operands}) =
  Printf.fprintf fmt "Phi {region : R%a; operands : %a}" ("D" ++ key) region
    (list ("D" ++ key))
    operands


and predecessor fmt pred =
  match pred with
  | Region.Jump r ->
      ("R" ++ key) fmt r
  | Branch b ->
      ("B" ++ key) fmt b

and region fmt (Region.Region pred) = list predecessor fmt pred

and cond fmt (Cond.Cond {region; operand}) =
  Printf.fprintf fmt "Cond {region: %a; operand: %a}" ("R" ++ key) region
    ("D" ++ key) operand

and branch fmt br =
  match br with
  | Branch.IfT c ->
      Printf.fprintf fmt "IfT %a" cond c
  | Branch.IfF c ->
      Printf.fprintf fmt "IfF %a" cond c

and control fmt control =
  match control with
  | Control.Jump r ->
      Printf.fprintf fmt "Jump %a" ("R" ++ key) r
  | Cond c ->
      Printf.fprintf fmt "Cond %a" ("C" ++ key) c
  | Return {region; operand} ->
      Printf.fprintf fmt "Return {region: %a; operand: %a}" ("R" ++ key)
        region ("D" ++ key) operand

and son fmt son =
  let datas = Son.data_nodes son in
  let regions = Son.region_nodes son in
  let controls = Son.control_nodes son in
  let branches = Son.branch_nodes son in
  Printf.fprintf fmt "SEA OF NODE{\n" ;
  Printf.fprintf fmt "\tDATAS {\n" ;
  List.iter
    (fun (k, d) -> Printf.fprintf fmt "\t\t%a -> %a\n" ("D" ++ key) k data d)
    datas ;
  Printf.fprintf fmt "\t}\n\tREGIONS {\n" ;
  List.iter
    (fun (k, d) -> Printf.fprintf fmt "\t\t%a -> %a\n" ("R" ++ key) k region d)
    regions ;
  Printf.fprintf fmt "\t}\n\tCONTROLS {\n" ;
  List.iter
    (fun (k, d) -> Printf.fprintf fmt "\t\t%a -> %a\n" ("C" ++ key) k control d)
    controls ;
  Printf.fprintf fmt "\t}\n\tBRANCHES {\n" ;
  List.iter
    (fun (k, d) -> Printf.fprintf fmt "\t\t%a -> %a\n" ("B" ++ key) k branch d)
    branches ;
  Printf.fprintf fmt "\t}\n}"
