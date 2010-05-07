(** Accessing meta-information in jar files. *)

(** The type of manifest files: *)

type main_section = {
  manifest_version : int list;
  main_attributes : (string * string) list;
}

type section = {
  name : string;
  attributes : (string * string) list;
}

type manifest = {
  main_section : main_section;
  individual_sections : section list;
}

(** Read a manifest from a jar file (which must end with .jar). *)
val jar2manifest : string -> manifest

(** Get a midlet's main class from the MIDlet-1 attribute of its manifest. *)
val midlet_main_class : manifest -> string

(** Other functions *)

(** Read a list of sections. This function may be used for reading
    various files from the META-INF directory of a jar file, including
    the MANIFEST.MF file *)
val sections : Lexing.lexbuf -> (string * string) list list

(** Interpret a list of section as a manifest. *)
val sections2manifest : (string * string) list list -> manifest
