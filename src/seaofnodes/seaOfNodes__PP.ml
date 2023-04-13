open SeaOfNodes__Type

let rec elements ty fmt l =
  match l with
  | [] -> Printf.fprintf fmt ""
  | [elt] -> Printf.fprintf fmt "%a" ty elt
  | elt::elts -> Printf.fprintf fmt "%a; %a" ty elt (elements ty) elts

let list ty fmt l =
  Printf.fprintf fmt "[%a]" (elements ty) l

let int fmt n =
  Printf.fprintf fmt "%d" n

let key fmt key =
  Printf.fprintf fmt "%d" (Son.get_id key)

let rec binop fmt binop =
  match binop with Binop.Add -> Printf.fprintf fmt "Add"

and data fmt data =
  match data with
  | Data.Const {value} ->
      Printf.fprintf fmt "Const %d" value
  | BinOp {op; operand1; operand2} ->
      Printf.fprintf fmt "BinOp {op: %a; operand1: %d; operand2: %d}" binop op
        (Son.get_id operand1) (Son.get_id operand2)
  | Phi phi_ ->
      Printf.fprintf fmt "%a" phi phi_

and phi fmt (Phi.Phi {region; operands}) =
  Printf.fprintf fmt "Phi {region : %a; operands : %a}" key region
    (list int)
    (List.map Son.get_id operands)

and predecessor fmt pred =
  match pred with
  | Region.Jump r -> key fmt r
  | Branch b -> key fmt b

and region fmt (Region.Region pred) =
  list predecessor fmt pred

and cond fmt (Cond.Cond {region; operand}) =
  Printf.fprintf fmt "Cond {region: %a; operand: %a}" key region key operand

and branch fmt br =
  match br with
  | Branch.IfT c ->
    Printf.fprintf fmt "IfT %a" cond c
  | Branch.IfF c ->
    Printf.fprintf fmt "IfF %a" cond c

and control fmt control =
  match control with
  | Control.Jump r ->
    Printf.fprintf fmt "Jump %a" key r
  | Cond c ->
    Printf.fprintf fmt "Cond %a" key c
  | Return {region; operand} ->
    Printf.fprintf fmt "Return {region: %a; operand: %a}" key region key operand

and son fmt son =
   let datas = Son.data_nodes son in
   let regions = Son.region_nodes son in
   let controls = Son.control_nodes son in
   let branches = Son.branch_nodes son in

   Printf.fprintf fmt "SEA OF NODE{\n";
   Printf.fprintf fmt "\tDATAS {\n";
   List.iter (fun (k, d) -> Printf.fprintf fmt "\t\t%a -> %a\n" key k data d) datas;
   Printf.fprintf fmt "\t}\n\tREGIONS {\n";
   List.iter (fun (k, d) -> Printf.fprintf fmt "\t\t%a -> %a\n" key k region d) regions;
   Printf.fprintf fmt "\t}\n\tCONTROLS {\n";
   List.iter (fun (k, d) -> Printf.fprintf fmt "\t\t%a -> %a\n" key k control d) controls;
   Printf.fprintf fmt "\t}\n\tBRANCHES {\n";
   List.iter (fun (k, d) -> Printf.fprintf fmt "\t\t%a -> %a\n" key k branch d) branches;
   Printf.fprintf fmt "\t}\n}"
