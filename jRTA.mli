(** Builds high level representations of java byte-code programs using
    Rapid Type Analysis algorithm. *)

(** [parse_program ~entrypoints classpath cn] returns a [program]
    composed of all the code found in [classpath] and that may be
    accessible from at least one method of [entrypoints] of the class
    [cn].  [classpath] is a list of directories and [.jar] files
    separated with ':'.  If [entrypoints] is not specified, the
    default method is "void main(String\[\])"*)
val parse_program :
  ?debug:bool ->
  ?entrypoints:JClass.method_signature list ->
  string -> JBasics.class_name -> JProgram.program

val parse_program_bench :
  ?debug:bool ->
  ?entrypoints:JClass.method_signature list ->
  string -> JBasics.class_name -> unit

type callgraph = ((JBasics.class_name * JClass.method_signature * int)
		  * (JBasics.class_name * JClass.method_signature)) list

val get_callgraph : JProgram.program -> callgraph

val store_callgraph : callgraph -> string -> unit

(* Not efficient at all. Only for test purposes *)
val store_simplified_callgraph : callgraph -> string -> unit

exception Invoke_not_found of (JBasics.class_name * JClass.method_signature
			       * JBasics.class_name * JClass.method_signature)
