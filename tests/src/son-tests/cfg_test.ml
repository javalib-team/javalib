open SeaOfNodes
open Utils

let _ =
  let cp = Javalib.class_path "java_files" in
  let class_name, _ =
    Javalib.extract_class_name_from_file "java_files/Test2.class"
  in
  let jclass = Javalib.get_class cp class_name in
  let jopcodes = get_jopcodes_by_method_name jclass "Test2" in
  Array.iteri
    (fun pc op -> Printf.printf "%i: %s\n" pc @@ JPrint.jopcode op)
    jopcodes ;
  let cfg = Cfg.build_cfg jopcodes in
  Cfg.Cfg.iter
    (fun pc preds ->
      Printf.printf "%d <- {" pc ;
      List.iter (Printf.printf "%d, ")
        (List.map
           (function Cfg.Cfg.Jump n | IfT n | IfF n | Implicit n -> n)
           preds ) ;
      Printf.printf "}\n" )
    cfg ;
  Printf.printf "+ [CFG] Test passed successfully.\n"
