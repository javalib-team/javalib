(** Builds high level representations of java byte-code programs using
    Rapid Type Analysis algorithm. *)

(** [parse_program ~other_entrypoints classpath (cn,ms)] returns a
    [program] composed of all the code found in [classpath] and that
    may be accessible from at least one method of
    [(cn,ms)::entrypoints].  [classpath] is a list of directories and
    [.jar] files separated with ':'.  If [entrypoints] is not
    specified, the default methods are the methods invoked natively by
    the JVM during its initialization. (cf {!default_entrypoints}).*)
val parse_program :
  ?other_entrypoints:(JBasics.class_name * JClass.method_signature) list ->
  string -> JBasics.class_name * JClass.method_signature-> JProgram.program

val parse_program_bench :
  ?other_entrypoints:(JBasics.class_name * JClass.method_signature) list ->
  string -> JBasics.class_name * JClass.method_signature-> unit

(** Sun's JVM calls some methods natively during the JVM
    initialization.  We have included the list (that we suppose
    complete but without garantee). *)
val default_entrypoints : (JBasics.class_name * JClass.method_signature) list
