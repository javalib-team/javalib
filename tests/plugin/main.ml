let env_ocamlpath = try (Sys.getenv "OCAMLPATH") with Not_found -> ""

let env_ocamlpath =
  "." ^ (if Sys.cygwin || Sys.win32 then ";" else ":") ^ env_ocamlpath

let () = Findlib.init ~env_ocamlpath ()

let () = Run.add (fun () -> print_endline "Hello from main entry point")

let plugins = List.tl (Array.to_list Sys.argv)

let () = Fl_dynload.load_packages plugins

let () = Run.apply ()
