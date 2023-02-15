(* Bir type from https://github.com/javalib-team/sawja/blob/master/src/bir.ml *)

type const = [
  | `Int of int32
]

type binop =
  | Add of JBasics.jvm_basic_type

type expr =
  | Const of const
  | Binop of binop * expr * expr

type instr =
  | Return of expr option


val eval_instr : instr -> int option
