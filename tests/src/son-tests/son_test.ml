open SeaOfNodes
open Utils

let _ =
  let cp = Javalib.class_path "java_files" in
  let class_name, _ = Javalib.extract_class_name_from_file "java_files/Test1.class" in
  let jclass = Javalib.get_class cp class_name in
  let jopcodes = get_jopcodes jclass in
  let son = Translator.translate_jopcodes jopcodes in

  Printf.printf "%a\n" PP.son son;

  assert (Interpretor.eval_son son == 42) ;
  let data_nodes = List.map snd (Type.Son.data_nodes son) in
  assert (is_list_hash_consed data_nodes) ;
  Javalib.close_class_path cp ;
  Printf.printf "+ [SON] Test passed successfully.\n"
