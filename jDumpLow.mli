val opcode : JClassLow.opcode -> string
val dump_code : 'a IO.output -> 'b -> JClassLow.jcode -> unit
val dump_attrib : 'a IO.output -> 'b -> JClassLow.attribute -> unit
val access_flags : JClassLow.access_flag list -> string
val dump_field : 'a IO.output -> 'b -> JClassLow.jfield -> unit
val dump_method : 'a IO.output -> 'b -> JClassLow.jmethod -> unit
val dump : 'a IO.output -> JClassLow.jclass -> unit
