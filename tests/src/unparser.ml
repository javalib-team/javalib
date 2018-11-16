open Javalib_pack
open Javalib
open JBasics   

let remove_extension s =
  let l = ExtLib.String.nsplit s "." in
  let lr = match l with
    | [] -> []
    | _ -> List.rev(List.tl (List.rev l)) in
  String.concat "." lr

let parse_class cfile =
  let scname = remove_extension (Filename.basename cfile) in
  let spath = Filename.dirname cfile in
  let cname = make_cn scname in
  let path = class_path spath in
  let ioc = get_class path cname in
  let force _ = () in
  let _ = map_interface_or_class ~force:true force ioc in
  let _ = Printf.printf "[o] class %s parsed successfully\n" scname in
  ioc

let unparse_class ioc outdir =
  let cname = get_name ioc in
  let scname = cn_name cname in
  let dest =  Printf.sprintf "%s/%s.class" outdir scname in
  let ch = open_out_bin dest in
  (unparse_class ioc ch;
   Printf.printf "[o] class %s unparsed successfully\n" scname;
   close_out ch)
  
let _ =
  let _ = set_permissive true in
  let classes_file = ref None in
  let outdir = ref "./" in
  let argspec =
    [("-out",
      Arg.Set_string outdir,
      "set output directory")] in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ "classes_file [options] -out <output directory>" in
  let _ = Arg.parse argspec (fun f -> classes_file := Some f) usage_msg in
  let outdir = !outdir in
  let _ = if not(Sys.file_exists outdir && Sys.is_directory outdir)
          then failwith (Printf.sprintf "Bad output directory : %s" outdir) in
  let classes_file = (match !classes_file with
                      | None -> failwith "Missing classes file parameter"
                      | Some f -> if (Sys.file_exists f && not(Sys.is_directory f))
                                  then f
                                  else failwith (Printf.sprintf "Bad classes file : %s" f)
                     ) in
  let chan = open_in classes_file in
  let classes = Std.input_list chan in
  let _ = close_in chan in
  let ioc_list = List.map (fun c -> parse_class c) classes in
  let _ = List.iter (fun ioc -> unparse_class ioc outdir) ioc_list in
  exit 0
