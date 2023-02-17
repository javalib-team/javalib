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

let rec eval_expr (bir : expr) =
  match bir with
  | Const (`Int n) -> Int32.to_int n
  | Binop (Add _, op1, op2) -> eval_expr op1 + eval_expr op2

let eval_instr (bir : instr) =
  match bir with
  | Return None -> None
  | Return (Some expr) -> Some (eval_expr expr)
