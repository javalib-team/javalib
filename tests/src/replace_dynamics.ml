open Javalib_pack
open Javalib
open JBasics

let dump_class classpath scname prefix dest =
  let cname = make_cn scname in
  let ioc = get_class classpath cname in
  let ioc', cmap = remove_invokedynamics ioc ~prefix in
  let dest_main = Printf.sprintf "%s/%s.class" dest scname in
  let ch = open_out_bin dest_main in
  unparse_class ioc' ch;
  close_out ch;
  ClassMap.iter
    (fun lcn ioc_lambda ->
      let slambdaname = cn_name lcn in
      let dest_in = Printf.sprintf "%s/%s.class" dest slambdaname in
      let ch = open_out_bin dest_in in
      unparse_class ioc_lambda ch;
      close_out ch)
    cmap

let _ =
  let srcdir = ref "./" in
  let outdir = ref "./" in
  let argspec = [ ("-out", Arg.Set_string outdir, "set output directory") ] in
  let usage_msg =
    "Usage: " ^ Sys.argv.(0) ^ "srcdir [options] -out <output directory>"
  in
  let _ = Arg.parse argspec (fun f -> srcdir := f) usage_msg in
  let srcdir = !srcdir in
  let outdir = !outdir in
  let () = dump_class (class_path srcdir) "ExampleLambda" "Lambda_1" outdir in
  let () = dump_class (class_path srcdir) "I" "Lambda_2" outdir in
  let () = dump_class (class_path srcdir) "J" "Lambda_3" outdir in
  ()
