open SeaOfNodes
open Utils

let tests = [("java_files/Test1.class", 42, "Test1"); ("java_files/Test3.class", 4, "Test3")]

let _ =
  let cp = Javalib.class_path "java_files" in
  let () =
    List.iter
      (fun (path, result, method_name) ->
        let class_name, _ = Javalib.extract_class_name_from_file path in
        let jclass = Javalib.get_class cp class_name in
        let jopcodes = get_jopcodes_by_method_name jclass method_name in
        let son = Translator.translate_jopcodes jopcodes in
        let bir = BirBuilder.translate_son son in
        Printf.printf "Eval : %d\n" (Option.get (Bir.eval_program bir)) ;
        Printf.printf "Eval : %d\n" (Interpretor.eval_son son) ;
        assert (Interpretor.eval_son son = result) ;
        assert (Bir.eval_program bir = Some result) )
      tests
  in
  Printf.printf "+ [BIR] Test passed successfully.\n"
