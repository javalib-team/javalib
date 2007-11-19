val opcode : JClass.opcode -> string
val dump_code : 'a IO.output -> 'b -> JClass.code -> unit
val dump_cfield :
  'a IO.output -> 'b -> JClass.field_signature -> JClass.class_field -> unit
val dump_ifield :
  'a IO.output ->
  'b -> JClass.field_signature -> JClass.interface_field -> unit
val dump_cmethod :
  'a IO.output ->
  JBasics.constant DynArray.t ->
  JClass.method_signature -> JClass.concrete_method -> unit
val dump_amethod :
  'a IO.output ->
  'b -> JClass.method_signature -> JClass.abstract_method -> unit
val dump_acmethod :
  'a IO.output ->
  JBasics.constant DynArray.t ->
  JClass.method_signature -> JClass.abstract_class_method -> unit
val dump : 'a IO.output -> JClass.interface_or_class -> unit
