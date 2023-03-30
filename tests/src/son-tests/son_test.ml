open SeaOfNodes
open Utils

let _ =
  let cp = Javalib.class_path "java_files" in
  let class_name, _ = Javalib.extract_class_name_from_file "java_files/Test1.class" in
  let jclass = Javalib.get_class cp class_name in
  let jopcodes = get_jopcodes jclass in
  let son = Translator.translate_jopcodes jopcodes in
  let data_key =
    List.hd
    @@ map_option (fun (_, control) ->
           match control with Type.Control.Return {operand; _} -> Some operand | _ -> None )
    @@ SeaOfNodes.Type.Son.control_nodes son
  in
  assert (Interpretor.eval_data son data_key == 42) ;
  let data_nodes = List.map snd (Type.Son.data_nodes son) in
  assert (is_list_hash_consed data_nodes) ;
  Javalib.close_class_path cp ;
  Printf.printf "+ [SON] Test passed successfully.\n"
