open JBasics
open JClass
open JCode

(** {1 Printing basic signatures and descriptors.} *)

(** Each of these functions gives a string representation of
      a type which has the corresponding name in JBasics.
      When set to true, the optional parameter [~jvm] allows to get a
      string representation following the JVM specification. Otherwise,
      a Java like representation (more readable) is given. *)

val class_name : ?jvm:bool -> class_name -> string
(** [class_name ~jvm class_name] gives a string representation of a [class_name]. *)

val cn_package : class_name -> string
(** Prints the package of the given [class_name]. *)

val cn_simple_name : class_name -> string
(** Prints the name of the given [class_name], omitting the package name. *)

val method_signature :
  ?jvm:bool ->
  ?callee:object_type ->
  ?param_names:string list ->
  method_signature ->
  string
(** [method_signature ~jvm ~param_names method_signature] gives a
      string representation of a [method_signature]. The optional
      parameter [~param_name] can contain a list of the same length as
      the parameters.  In this case, when [~jvm] is set to false (or
      by default), the names of the parameters appear in the string
      representation of the method signature (like in Java).  The
      optional parameter [~callee] can contain an [object_type] which
      will be displayed in the signature as in the following example.
      {v void Object.toString() v}

      @raise Invalid_argument if the length of [param_names] (if given) is not
      the same as the number of arguments. *)

val class_method_signature :
  ?jvm:bool -> ?param_names:string list -> class_method_signature -> string
(** [class_method_signature ~jvm ~param_names cms] gives a string
      representation of a [class_method_signature] using
      {!Javalib.JPrint.method_signature} with the class name of [cms] as
      [callee].

      @raise Invalid_argument if the length of [param_names] (if given) is not
      the same as the number of arguments of the method.. *)

val field_signature :
  ?jvm:bool -> ?declared_in:class_name -> field_signature -> string
(** [field_signature ~jvm ~declared_in field_signature] gives a string
      representation of a [field_signature] where a class name may be given. *)

val class_field_signature : ?jvm:bool -> class_field_signature -> string
(** [class_field_signature ~jvm cfs] gives a string representation of a class
      field signature using {!Javalib.JPrint.field_signature}. *)

val signature : ?jvm:bool -> string -> descriptor -> string
(** [signature ~jvm name descriptor] gives a string representation of a
      [descriptor] which can either contain a [field_descriptor] or a
      [method_descriptor], associated to the name [name]. *)

val java_basic_type : ?jvm:bool -> java_basic_type -> string
(** [java_basic_type ~jvm java_basic_type] gives a string
      representation of a [java_basic_type]. *)

val object_type : ?jvm:bool -> object_type -> string
(** [object_type ~jvm object_type] gives a string representation of an
      [object_type]. *)

val value_type : ?jvm:bool -> value_type -> string
(** [value_type ~jvm value_type] gives a string representation of a
      [value_type]. *)

val return_type : ?jvm:bool -> value_type option -> string
(** [return_type ~jvm rt] gives a string representation of the
      return type [rt]. The difference with [value_type] is that the added value
      [None] allows to represent the [void] value. *)

val value_type_list :
  ?jvm:bool -> ?names:string list -> value_type list -> string
(** [value_type_list ~names l] gives a string representation of the
     [value_type] list [l] with possibly associated [names].

     @raise Invalid_argument if the length of [names] is not the same as
     [l].  *)

val field_descriptor : ?jvm:bool -> value_type -> string
(** [field_descriptor ~jvm field_descriptor] gives a string representation of a
      [field_descriptor]. *)

val method_descriptor :
  ?jvm:bool -> value_type list -> value_type option -> string
(** [method_descriptor ~jvm method_descriptor] gives a string representation of a
      method descriptor, composed of the parameters types and the return type of a
      method. *)

(** {1 Printing stackmaps, constant pool values and exception handlers.} *)

val stack_map : stackmap_frame -> string
val constant : constant -> string
val constant_pool : constant array -> string
val exception_handler : exception_handler -> string

(** {1 Printing JVM opcodes.} *)

val jopcode : ?jvm:bool -> jopcode -> string
val jcode : ?jvm:bool -> jcode -> string list

(** {1 Outputting methods and classes.} *)

val print_method :
  ?jvm:bool -> 'a jmethod -> ('a -> string list) -> out_channel -> unit

val print_method' :
  ?jvm:bool ->
  'a jmethod ->
  ('a -> Format.formatter -> unit) ->
  Format.formatter ->
  unit

val print_class :
  ?jvm:bool ->
  'a interface_or_class ->
  ('a -> string list) ->
  out_channel ->
  unit

val print_class' :
  ?jvm:bool ->
  'a interface_or_class ->
  ('a -> Format.formatter -> unit) ->
  Format.formatter ->
  unit
