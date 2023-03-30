module Cfg : sig
  type t
  type predecessor =
    | Jump of int
    | IfT of int
    | IfF of int
    | Implicit of int
        (** [Implicit pc] means that [pc] and [pc + 1] denote two different
            regions, and [pc] is not a control-flow instruction. *)

  val get_source : predecessor -> int

  val find : int -> t -> predecessor list
  val empty : t
  val iter : (int -> predecessor list -> unit) -> t -> unit
  val fold : (int -> predecessor list -> 'a -> 'a) -> t -> 'a -> 'a

  val next_pc : int -> JCode.jopcodes -> int
end

val build_cfg : JCode.jopcode array -> Cfg.t
