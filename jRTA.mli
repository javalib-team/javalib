(** Builds high level representations of java byte-code programs using
    Rapid Type Analysis algorithm. *)

(** {2 Navigable hierarchy} *)

val parse_program_bench : ?debug:bool -> string -> JBasics.class_name -> unit

val parse_program : ?debug:bool -> string -> JBasics.class_name -> JProgram.program

type callgraph = ((JBasics.class_name * JClass.method_signature * int)
		  * (JBasics.class_name * JClass.method_signature)) list

val get_callgraph : JProgram.program -> callgraph

val store_callgraph : callgraph -> string -> unit

(* Not efficient at all. Only for test purposes *)
val store_simplified_callgraph : callgraph -> string -> unit

exception Invoke_not_found of JBasics.class_name * JClass.method_signature
  * JBasics.class_name * JClass.method_signature
