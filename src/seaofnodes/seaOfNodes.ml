module Type = SeaOfNodes__Type

module Translator : sig
  val translate_jopcodes : JCode.jopcodes -> Type.Node.t Type.IMap.t
end =
  SeaOfNodes__Translator
