open SeaOfNodes
open Utils

let _ =
  let cp = Javalib.class_path "java_files" in
  let class_name, _ = Javalib.extract_class_name_from_file "java_files/Test3.class" in
  let jclass = Javalib.get_class cp class_name in
  let jopcodes = get_jopcodes jclass in
  let son = Translator.translate_jopcodes jopcodes in
  let bir = BirBuilder.translate_son son in
  Printf.printf "Eval : %d\n" (Option.get (Bir.eval_program bir));
  Printf.printf "Eval : %d\n" (Interpretor.eval_son son);
  assert (Interpretor.eval_son son = 4);
  assert (Bir.eval_program bir = Some(4));
  Printf.printf "+ [BIR] Test passed successfully.\n"
