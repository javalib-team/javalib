module Type = SeaOfNodes__Type

module Translator : sig
  val translate_jopcodes : JCode.jopcodes -> Type.Son.t
end

module Interpretor : sig
  val eval_data : Type.Data.t -> int
end

module BirBuilder = SeaOfNodes__BirBuilder
