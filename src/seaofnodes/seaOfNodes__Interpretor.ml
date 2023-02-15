open SeaOfNodes__Type

let rec eval_data data =
  match data with
  | Data.Const {value} ->
      value
  | BinOp {op= Add; operand1; operand2} ->
      eval_data operand1 + eval_data operand2
  | Phi _ ->
      failwith "Not Implemented"
