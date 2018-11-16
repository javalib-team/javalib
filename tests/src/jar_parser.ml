open Javalib_pack
open Javalib
open JBasics   

let _ =
  let _ = set_permissive true in
  let javafilename = ref None in
  let verbose = ref true in
  let outdir = ref "./" in
  let argspec =
    [("--silent",
      Arg.Clear verbose,
      "set silent mode");
     ("-out",
      Arg.Set_string outdir,
      "set output directory")] in
  let usage_msg = "Usage: " ^Sys.argv.(0) ^" [options] <jar files>" in
  let _ = Arg.parse argspec (fun f -> javafilename := Some f) usage_msg in
  let outdir = !outdir in
  let _ = if not(Sys.file_exists outdir && Sys.is_directory outdir)
          then failwith (Printf.sprintf "Bad output directory : %s" outdir) in
  let filenames =
    match !javafilename with
      | None ->
          prerr_endline "Error: a list of paths to jar files separated by ':' must be specified";
          Arg.usage argspec usage_msg;
          exit 1
      | Some f -> f in
  let count_classes = ref 0 in
  let parsed = ref [] in
  let to_parse = ["rt.jar"; "resources.jar"; "jce.jar";
                  "jsse.jar"; "charsets.jar"] in
  let iter_on_jar jarfile =
    if Sys.file_exists jarfile && List.mem (Filename.basename jarfile) to_parse then
      iter
        (fun ioc ->
          let cn = get_name ioc in
          let force _ = () in
          let _ = map_interface_or_class ~force:true force ioc in
          incr count_classes;
          if !verbose then Printf.printf "parsing class %s in %s...\n" (cn_name cn) (Filename.basename jarfile);
          let destination = Printf.sprintf "%s/%s.class" outdir (cn_name cn) in
          let ch = open_out_bin destination in
          unparse_class ioc ch;
          close_out ch;
          parsed := destination :: !parsed)
        jarfile
  in
  let l = ExtLib.String.nsplit filenames ":" in
  List.iter iter_on_jar l;
  Printf.printf "%d classes have been parsed and dumped !\n" !count_classes;
  exit 0
