module Type = SeaOfNodes__Type

module Translator : sig
  val translate_jopcodes : JCode.jopcodes -> Type.Son.t
end =
  SeaOfNodes__Translator

module Interpretor = SeaOfNodes__Interpretor
module BirBuilder = SeaOfNodes__BirBuilder
