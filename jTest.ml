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
    while pos() < len do
      let p = pos() in
      let ch, ch' = input_copy ch in
	code.(p) <- JCode.parse_instruction ch pos consts;
	let ch', count = pos_out ch' in
	  JUnparse.unparse_instruction ch' (DynArray.of_array consts) (function () -> p + count ()) code.(p);
	  if
	    not (IO.close_out ch')
	  then failwith ("bad unparsing for " ^ JDump.opcode code.(p))
    done;
    code
