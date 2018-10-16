open Javalib_pack
open Javalib
open JBasics   

let _ =
  let _ = set_permissive true in
  let javafilename = ref None in
  let verbose = ref true in
  let argspec =
    [("--silent",
      Arg.Clear verbose,
      "set silent mode")] in
  let usage_msg = "Usage: " ^Sys.argv.(0) ^" [options] <package.classname>" in
  let _ = Arg.parse argspec (fun f -> javafilename := Some f) usage_msg in
  let filename =
    match !javafilename with
      | None ->
          prerr_endline "Error: a jar file or class file must be specified";
          Arg.usage argspec usage_msg;
          exit 1
      | Some f -> f in
  let count_classes = ref 0 in
  let parsed = ref [] in
(*  let create_sub_dir cname =
    let l = BatString.split_on_char '.' cname in
    let rec mkdir_rec path = function
        [] -> ()
      | name::q ->
         let f = path ^ "/" ^ name in
         if not (Sys.file_exists f) then
           begin
             Printf.printf "mkdir %s\n" f;
             Unix.mkdir f 0o755
           end;
         mkdir_rec f q in
    mkdir_rec "out2" l in *)
  let iter_on_jar jarfile =
    if Filename.check_suffix jarfile ".jar" then
      iter
        (fun ioc ->
          let cn = get_name ioc in
          (* create_sub_dir (cn_name cn); *)
          let force _ = () in
          let _ = map_interface_or_class ~force:true force ioc in
          incr count_classes;
          if !verbose then Printf.printf "parsing class %s in %s...\n" (cn_name cn) filename;
          let destination = Printf.sprintf "out/%s.class" (cn_name cn) in
          let ch = open_out_bin destination in
          unparse_class ioc ch;
          close_out ch;
          parsed := destination :: !parsed)
        (filename^jarfile)
    else Printf.printf "strange %s\n" jarfile in
  if Sys.is_directory filename then
    let l = Sys.readdir filename in
    Array.iter iter_on_jar l
  else iter_on_jar filename;
  Printf.printf "%d classes have been parsed and dumped !\n" !count_classes;
(*  List.iter
    (fun cn ->
      let _ = Javalib.get_class classpath cn in ())
    !parsed;
  Javalib.read
    (Javalib.make_directories ".")
    (fun _ ioc ->
          let force _ = () in
          let _ = map_interface_or_class ~force:true force ioc in
          ()
    )
    ()
    !parsed; *)
  exit 0
