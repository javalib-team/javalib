open SeaOfNodes
open Utils

let _ =
  let cp = Javalib.class_path "java_files" in
  let class_name, _ =
    Javalib.extract_class_name_from_file "java_files/Test1.class"
  in
  let jclass = Javalib.get_class cp class_name in
  let jopcodes = get_jopcodes jclass in
  let son = Translator.translate_jopcodes jopcodes in
  let ret =
    List.hd
    @@ map_option (fun (control_key , control) ->
           match control with
           | Type.Control.Return _ ->
               Some control_key
           | _ ->
               None )
    @@ SeaOfNodes.Type.Son.control_nodes son
  in
  let bir = BirBuilder.control_to_bir son ret in
  assert (Bir.eval_instr 0 bir = Some 42) ;
  Printf.printf "+ [BIR] Test passed successfully.\n"
