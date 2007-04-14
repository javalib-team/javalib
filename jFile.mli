(** Accessing classes in files, directories and jar files. *)

(** The following functions search for class files in the following order :
    - [classpath] is a list of directories separated by [:]. If a name can be
    found in some directory, subsequent directories are ignored.
    - If a name is the name of an existing directory, then every .class file
    inside this directory is read, and the search is over (even if the
    directory is empy).
    - Otherwise, if the name refers to an existing .class file (without the
    extension) then this file is read.
    - Otherwise, if the name ends in .jar and the file exists, it is assumed
    to be jar file and the class files inside are read.

    Dots in class and directory names are interpreted as / (but not for jar
    files). *)

(** [read classpath f acc names] iterates [f] over all classes specified by
    [names]. [acc] is the initial accumulator value. *)
val read : string -> ('a -> JClass.jclass -> 'a) -> 'a -> string list -> 'a

(** [transform classpath outputdir f names] applies [f] to all classes specified
    by [names], writing the resulting classes in [outputdir]. Jar files are
    mapped to jar files, and the non-class files are kept unchanged in the
    resulting archive. *)
val transform : string -> string -> (JClass.jclass -> JClass.jclass) -> string list -> unit
