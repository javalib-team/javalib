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

(* This implementation is inspired by the paper
   "Semantic reasoning about the sea of nodes" written by Delphine Demange,
   Yon Fernández de Retana, David Pichardie; see
   https://hal.inria.fr/hal-01723236/file/sea-of-nodes-hal.pdf for more details *)

module Binop : sig
  type t = Add

  val hash : t -> int
end = struct
  type t = Add

  let hash _ = 1
end

module rec Data : sig
  (* It is possible to pattern match elements of type t,
     but you must use the smart constructors to build values *)
  type t = private
    | Const of {unique: int; value: int}
    | BinOp of
        {unique: int; op: Binop.t; operand1: t Son.key; operand2: t Son.key}
    | Phi of Phi.t

  val hash : t -> int

  val equal : t -> t -> bool

  (* Constructors *)
  val const : int -> t

  val binop : Binop.t -> t Son.key -> t Son.key -> t

  val phi : Phi.t -> t

  (* Unique id of the node *)
  val get_id : t -> int
end = struct
  type t =
    | Const of {unique: int; value: int}
    | BinOp of
        {unique: int; op: Binop.t; operand1: t Son.key; operand2: t Son.key}
    | Phi of Phi.t

  let hash = function
    | Const n ->
        n.value
    | BinOp {op; operand1; operand2} ->
        (*  Boost hash combiner *)
        let hash_nums (ns : int list) =
          List.fold_left
            (fun hash n ->
              hash lxor (n + 0x9e3779b9 + (hash lsl 6) + (hash asr 2)) )
            0 ns
        in
        hash_nums [Binop.hash op; Son.get_id operand1; Son.get_id operand2]
    | Phi _ ->
        0

  let equal t t' =
    match (t, t') with
    | Const n, Const n' ->
        n.value = n'.value
    | ( BinOp {op; operand1; operand2}
      , BinOp {op= op'; operand1= operand1'; operand2= operand2'} ) ->
        (* We assume that binops and datas are hashconsed and use the physical equality *)
        op == op' && operand1 == operand1' && operand2 == operand2'
    | Phi phi, Phi phi' ->
        phi == phi'
    | _ ->
        false

  (* Weak set to remember previously created values *)
  module W = Weak.Make (Data)

  let nodes = W.create 5003

  let cpt = ref 1

  (* if d is on the set, we return the previously constructed element,
     otherwise we add it to the set and increment cpt *)
  let add_or_find n0 =
    let n = W.merge nodes n0 in
    if n == n0 then incr cpt ;
    n

  let const n =
    let n0 = Const {unique= !cpt; value= n} in
    add_or_find n0

  let binop op operand1 operand2 =
    let n0 = BinOp {unique= !cpt; op; operand1; operand2} in
    add_or_find n0

  let phi phi = Phi phi

  let get_id data =
    match data with Const n -> n.unique | BinOp n -> n.unique | Phi _ -> -1
end

and Region : sig
  type predecessor = Jump of Region.t Son.key | Branch of Branch.t Son.key

  type t = Region of predecessor list

  val hash : t -> int
end = struct
  type predecessor = Jump of Region.t Son.key | Branch of Branch.t Son.key

  type t = Region of predecessor list

  let hash (Region predecessors) = Hashtbl.hash predecessors
end

and Phi : sig
  type t = Phi of {region: Region.t Son.key; operands: Data.t Son.key list}
end = struct
  type t = Phi of {region: Region.t Son.key; operands: Data.t Son.key list}
end

and Cond : sig
  type t = Cond of {region: Region.t Son.key; operand: Data.t Son.key}
end = struct
  type t = Cond of {region: Region.t Son.key; operand: Data.t Son.key}
end

and Branch : sig
  type t = IfT of Cond.t | IfF of Cond.t
end = struct
  type t = IfT of Cond.t | IfF of Cond.t
end

and Control : sig
  type t =
    | Jump of Region.t Son.key
    | Cond of Cond.t Son.key
    | Return of {region: Region.t Son.key; operand: Data.t Son.key}
end = struct
  type t =
    | Jump of Region.t Son.key
    | Cond of Cond.t Son.key
    | Return of {region: Region.t Son.key; operand: Data.t Son.key}
end

and Son : sig
  type t

  type 'a key

  val get_id : 'a key -> int

  val alloc_data : t -> Data.t -> t * Data.t key

  val alloc_region : t -> Region.t -> t * Region.t key

  val alloc_control : t -> Control.t -> t * Control.t key

  val alloc_branch : t -> Branch.t -> t * Branch.t key

  val get : 'a key -> t -> 'a

  val set : 'a key -> 'a -> t -> t

  val modify : 'a key -> ('a -> 'a) -> t -> t

  val add_predecessor : t -> Region.t key -> Region.predecessor -> t

  val empty : t

  val data_nodes : t -> (Data.t Son.key * Data.t) list

  val control_nodes : t -> (Control.t Son.key * Control.t) list

  val unsafe_make_key : int -> Data.t key
end = struct
  module IMap = Map.Make (Int)
  include IMap

  (* TODO: Move to its own module *)
  type 'a map = {map: 'a IMap.t; next: int}

  let empty_map_at n = {map= IMap.empty; next= n}

  let empty_map = empty_map_at 0

  let find key map = IMap.find key map.map

  let add key value map = {map with map= IMap.add key value map.map}

  let alloc map x =
    ({map= IMap.add map.next x map.map; next= map.next + 1}, map.next)

  type t =
    { data_map: Data.t map
    ; region_map: Region.t map
    ; control_map: Control.t map
    ; branch_map: Branch.t map }

  type 'a key =
    | DataKey : int -> Data.t key
    | RegionKey : int -> Region.t key
    | ControlKey : int -> Control.t key
    | BranchKey : int -> Branch.t key

  (* This is not unsafe, because we cannot reconstruct a key from its value *)
  let get_id (type a) (key : a key) =
    match key with DataKey n | RegionKey n | ControlKey n | BranchKey n -> n

  (* This is unsafe, see comment above *)
  let unsafe_make_key n = DataKey n

  let empty =
    { data_map= empty_map_at 1000
    ; region_map= empty_map
    ; control_map= empty_map
    ; branch_map= empty_map }

  let alloc_data m data =
    let data_map, key = alloc m.data_map data in
    ({m with data_map}, DataKey key)

  let alloc_region m region =
    let region_map, key = alloc m.region_map region in
    ({m with region_map}, RegionKey key)

  let alloc_control m control =
    let control_map, key = alloc m.control_map control in
    ({m with control_map}, ControlKey key)

  let alloc_branch m branch =
    let branch_map, key = alloc m.branch_map branch in
    ({m with branch_map}, BranchKey key)

  let get (type a) (key : a key) (map : t) : a =
    match key with
    | DataKey k ->
        find k map.data_map
    | RegionKey k ->
        find k map.region_map
    | ControlKey k ->
        find k map.control_map
    | BranchKey k ->
        find k map.branch_map

  let set (type a) (key : a key) (value : a) (map : t) : t =
    match key with
    | DataKey k ->
        {map with data_map= add k value map.data_map}
    | RegionKey k ->
        {map with region_map= add k value map.region_map}
    | ControlKey k ->
        {map with control_map= add k value map.control_map}
    | BranchKey k ->
        {map with branch_map= add k value map.branch_map}

  let modify (type a) (key : a key) (f : a -> a) (map : t) : t =
    let v = get key map in
    set key (f v) map

  let add_predecessor t rk p =
    let (Region.Region ps) = get rk t in
    let r = Region.Region (p :: ps) in
    set rk r t

  let data_nodes m =
    List.map (fun (i, data) -> (DataKey i, data))
    @@ IMap.bindings m.data_map.map

  let control_nodes m =
    List.map (fun (i, control) -> (ControlKey i, control))
    @@ IMap.bindings m.control_map.map
end
