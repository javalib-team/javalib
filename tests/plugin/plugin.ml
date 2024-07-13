let my_hello =
  Javalib_pack.JBasics.(ConstString (make_jstr "Hello from Javalib"))

let main () = print_endline (Javalib_pack.Javalib.JPrint.constant my_hello)
let () = Run.add main
