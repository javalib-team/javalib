(* The module [Identified] is here to hide the tags in the SON types
   We use a Weak set to remember previously created values,
   in which we store elements of type Identified t
   Identified types are just an element with an integer *)

(* This implementation is inspired by the paper "Semantic reasoning about the sea of nodes" written by Delphine Demange, Yon Fernández de Retana, David Pichardie; see https://hal.inria.fr/hal-01723236/file/sea-of-nodes-hal.pdf for more details *)

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
    | BinOp of {unique: int; op: Binop.t; operand1: t; operand2: t}
    | Phi of Phi.t

  val hash : t -> int

  val equal : t -> t -> bool

  val pp_dot : out_channel -> t -> unit

  (* Constructors *)
  val const : int -> t

  val binop : Binop.t -> t -> t -> t

  val phi : Phi.t -> t

  (* Unique id of the node *)
  val get_id : t -> int
end = struct
  type t =
    | Const of {unique: int; value: int}
    | BinOp of {unique: int; op: Binop.t; operand1: t; operand2: t}
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
        hash_nums [Binop.hash op; Data.get_id operand1; Data.get_id operand2]
    | Phi phi ->
        Phi.hash phi

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

  let pp fmt data = Printf.fprintf fmt "node_%d" (Hashtbl.hash data)

  let rec pp_dot fmt data =
    match data with
    | Const n ->
        Printf.fprintf fmt "%a [ label = \"Const %d\" ]\n" pp data n.value
    | BinOp {op= Binop.Add; operand1= op1; operand2= op2} ->
        Printf.fprintf fmt "%a [ label = \"Add\" ]\n" pp data ;
        Printf.fprintf fmt "%a -> %a\n" pp data pp op1 ;
        Printf.fprintf fmt "%a -> %a\n" pp data pp op2 ;
        pp_dot fmt op1 ;
        pp_dot fmt op2
    | _ ->
        ()

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
  type predecessor = Jump of Region.t | Branch of Branch.t

  type t = Region of predecessor list

  val hash : t -> int
end = struct
  type predecessor = Jump of Region.t | Branch of Branch.t

  type t = Region of predecessor list

  let hash (Region predecessors) = Hashtbl.hash predecessors
end

and Phi : sig
  type t = Phi of {region: Region.t; operands: Data.t list}

  val hash : t -> int
end = struct
  type t = Phi of {region: Region.t; operands: Data.t list}

  let hash (Phi {region; operands}) =
    Region.hash region
    lxor List.fold_left ( lxor ) 1 (List.map Data.hash operands)
end

and Cond : sig
  type t = Cond of {region: Region.t; operand: Data.t}

  val hash : t -> int
end = struct
  type t = Cond of {region: Region.t; operand: Data.t}

  let hash (Cond {region; operand}) = Region.hash region lxor Data.hash operand
end

and Branch : sig
  type t = IfT of Cond.t | IfF of Cond.t

  val get_hash : t -> int
end = struct
  type t = IfT of Cond.t | IfF of Cond.t

  let get_hash = function
    | IfT cond ->
        2 * Cond.hash cond
    | IfF cond ->
        1 + (2 * Cond.hash cond)
end

module Control : sig
  type t =
    | Jump of Region.t
    | Cond of Cond.t
    | Return of {region: Region.t; operand: Data.t}

  val pp_dot : out_channel -> t -> unit
end = struct
  type t =
    | Jump of Region.t
    | Cond of Cond.t
    | Return of {region: Region.t; operand: Data.t}

  let pp fmt data = Printf.fprintf fmt "node_%d" (Hashtbl.hash data)

  let pp_dot fmt control =
    match control with
    | Return {region= _; operand} ->
        Printf.fprintf fmt "%a [ label = \"Return\" ]\n" pp control ;
        Printf.fprintf fmt "%a -> %a\n" pp control pp operand ;
        Data.pp_dot fmt operand
    | _ ->
        ()
end

module Node : sig
  type t =
    | Data of Data.t
    | Region of Region.t
    | Control of Control.t
    | Branch of Branch.t

  val pp_dot : out_channel -> t -> unit
end = struct
  type t =
    | Data of Data.t
    | Region of Region.t
    | Control of Control.t
    | Branch of Branch.t

  let pp_dot fmt node =
    Printf.fprintf fmt "digraph G {\nnode [shape=record];\ncolor = black;\n" ;
    ( match node with
    | Data data ->
        Data.pp_dot fmt data
    | Control control ->
        Control.pp_dot fmt control
    | _ ->
        assert false ) ;
    Printf.fprintf fmt "}"
end

type id = int

module Son = struct
  module IMap = Map.Make (Int)
  include IMap
  type t = Node.t IMap.t
end
