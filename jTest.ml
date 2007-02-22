open IO

let input_copy ch =
  let queue = Queue.create ()
  and eq = ref true in
  let ch' =
    create_in
      ~read:(function () ->
	       let c = read ch in
		 Queue.push c queue;
		 c)
      ~input:(fun s p l ->
		let n = input ch s p l in
		  for i = p to p + n - 1 do
		    Queue.push s.[i] queue
		  done;
		  n)
      ~close:(function () -> ())
  and ch'' =
    create_out
      ~write:(function c ->
		eq := 
		  (try
		     Queue.pop queue = c && ! eq
		   with Queue.Empty -> false);
		assert ! eq) (* fail fast *)
      ~output:(fun s p l ->
		 (try
		    for i = p to p + l - 1 do
		      eq := Queue.pop queue = s.[i] && !eq
		    done
		  with Queue.Empty -> eq := false);
		 assert ! eq; (* fail fast *)
		 l)
      ~flush:(function () -> ())
      ~close:(function () -> ! eq && Queue.is_empty queue)
  in
    ch', ch''

open JClass

let parse_code ch consts len =
  let ch , pos = IO.pos_in ch in
  let code = Array.create len OpInvalid in

  let rec step wide ch =
    let p = pos() in
    let op = IO.read_byte ch in
      if (op = 170 || op = 171) && (p + 1) mod 4 > 0 then ignore(IO.nread ch (4 - ((p + 1) mod 4)));
      try
	let my_code = JCode.parse_opcode op ch consts wide in
	  code.(p) <- my_code;
	  if my_code = OpWide then step true ch
      with
	  Exit -> raise (JCode.Invalid_opcode op)
  in

    while pos() < len do
      let p = pos () in
      let ch, ch' = input_copy ch in
	step false ch;
	let c = match code.(p) with
	  | OpWide -> code.(p + 1)
	  | c -> c in
	let ch', count = pos_out ch' in
	  JUnparse.unparse_instruction ch' (DynArray.of_array consts) (function () -> p + count ()) c;
	  if
	    not (IO.close_out ch')
	  then failwith ("bad unparsing for " ^ JDump.opcode c)
    done;
    code
