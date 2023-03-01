module TranslatorState : sig
  type t = {stack: SeaOfNodes__Type.Data.t list; region: SeaOfNodes__Type.Region.t; count: int}

  type 'a monad = (t, 'a) Monad.State.t

  val initial : t

  val push_stack : SeaOfNodes__Type.Data.t -> (t, unit) Monad.State.t

  val pop_stack : unit -> (t, SeaOfNodes__Type.Data.t) Monad.State.t

  val fresh : unit -> (t, int) Monad.State.t

  val get_current_region : unit -> (t, SeaOfNodes__Type.Region.t) Monad.State.t
end

val translate_jopcode :
     SeaOfNodes__Type.Node.t SeaOfNodes__Type.IMap.t
  -> JCode.jopcode
  -> (TranslatorState.t, SeaOfNodes__Type.Node.t SeaOfNodes__Type.IMap.t) Monad.State.t

val translate_jopcodes : JCode.jopcodes -> SeaOfNodes__Type.Node.t SeaOfNodes__Type.IMap.t
