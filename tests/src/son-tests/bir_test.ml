open SeaOfNodes
open Utils

let _ =
  let cp = Javalib.class_path "java_files" in
  let class_name, _ = Javalib.extract_class_name_from_file "java_files/Test1.class" in
  let jclass = Javalib.get_class cp class_name in
  let jopcodes = get_jopcodes jclass in
  let graph = Translator.translate_jopcodes jopcodes in
  let ret =
    List.hd
    @@ map_option (fun (_, n) ->
           match n with Type.Node.Control (Type.Control.Return _) as ret -> Some ret | _ -> None )
    @@ SeaOfNodes.Type.Son.bindings graph
  in
  let bir = BirBuilder.node_to_bir ret in
  assert (Bir.eval_instr bir = Some 42) ;
  Printf.printf "+ [BIR] Test passed successfully.\n"
