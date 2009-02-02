(** Builds high level representations of java byte-code programs using
    Rapid Type Analysis algorithm. *)

(** {2 Navigable hierarchy} *)
  
val parse_program_bench : ?debug:bool -> string -> JBasics.class_name -> unit

val parse_program : ?debug:bool -> string -> JBasics.class_name -> JProgram.program

val get_callgraph : JProgram.program ->
  ((JBasics.class_name * JClass.method_signature * int)
   * (JBasics.class_name * JClass.method_signature)) list
