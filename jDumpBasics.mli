val class_name : JBasics.class_name -> string
val basic_type :
  [< `Bool | `Byte | `Char | `Double | `Float | `Int | `Long | `Short ] ->
  string
val object_value_signature : string -> JBasics.object_type -> string
val value_signature : string -> JBasics.value_type -> string
val method_signature :
  string -> JBasics.value_type list * JBasics.value_type option -> string
val signature : string -> JBasics.signature -> string
val jvm_basic_type : [< `Double | `Float | `Int | `Int2Bool | `Long ] -> char
val java_basic_type :
  [< `Bool | `Byte | `Char | `Double | `Float | `Int | `Long | `Short ] ->
  char
val dump_constant_value : 'a IO.output -> JBasics.constant_value -> unit
val dump_constant : 'a IO.output -> JBasics.constant -> unit
val dump_constantpool : 'a IO.output -> JBasics.constant array -> unit
val dump_stackmap :
  'a IO.output ->
  int * JBasics.verification_type list * JBasics.verification_type list ->
  unit
val dump_exc : 'a IO.output -> 'b -> JBasics.jexception -> unit
