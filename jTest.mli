(* This module was used to test the consistency of class file reading and
   writing. It might still be usefull for debugging. *)

(* This function transform an input channel so as to provide an output
   channel that matches the characters written in it against the characters
   read from the input. *)
val input_copy : IO.input -> IO.input * bool IO.output

(* Clone of JCode.parse_code that checks JUnparse.unparse_instruction
   against the input. *)
val parse_code : IO.input -> JClass.constant array -> int -> JClass.opcode array
