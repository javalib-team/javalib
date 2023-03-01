module TranslatorState : sig
  type t = {stack: SeaOfNodes__Type.Data.t list; region: SeaOfNodes__Type.Region.t; count: int}

  type 'a monad = (t, 'a) Monad.State.t

  val initial : t

  val push_stack : SeaOfNodes__Type.Data.t -> unit monad

  val pop_stack : unit -> SeaOfNodes__Type.Data.t monad

  val fresh : unit -> int monad

  val get_current_region : unit -> SeaOfNodes__Type.Region.t monad
end

val translate_jopcode :
     SeaOfNodes__Type.Son.t
  -> JCode.jopcode
  -> (TranslatorState.t, SeaOfNodes__Type.Son.t) Monad.State.t

val translate_jopcodes : JCode.jopcodes -> SeaOfNodes__Type.Son.t
