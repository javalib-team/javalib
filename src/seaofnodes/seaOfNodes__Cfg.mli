module Cfg : sig
  type t
  type predecessor = Jump of int | IfT of int | IfF of int

  val find : int -> t -> predecessor list
  val empty : t
  val iter : (int -> predecessor list -> unit) -> t -> unit
end

val build_cfg : JCode.jopcode array -> Cfg.t
