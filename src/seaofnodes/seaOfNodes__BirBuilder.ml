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
