let todo = ref []

let add f = todo := f :: !todo

let apply () = List.iter (fun f -> f()) (List.rev !todo)
