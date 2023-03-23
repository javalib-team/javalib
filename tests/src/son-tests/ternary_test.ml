open SeaOfNodes
open Utils

let _ =
  let cp = Javalib.class_path "java_files" in
  let class_name, _ = Javalib.extract_class_name_from_file "java_files/Test3.class" in
  let jclass = Javalib.get_class cp class_name in
  let jopcodes = get_jopcodes jclass in
  let son = Translator.translate_jopcodes jopcodes in

  (* check that hash-consing works properly, i.e. there is no duplicate data nodes *)
  let data_nodes = List.map snd (Type.Son.data_nodes son) in
  assert (
    List.for_all (fun a -> List.for_all (fun n -> not (a = n && a != n)) data_nodes) data_nodes ) ;

  assert (List.exists (function Type.Data.Phi _ -> true | _ -> false) data_nodes);
  Javalib.close_class_path cp;

  Printf.printf "+ [TER] Test passed successfully.\n"
