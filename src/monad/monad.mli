module State :
  sig
    type ('s, 'a) t = State of ('s -> 's * 'a)
    val return : 'a -> ('b, 'a) t
    val fmap : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
    val bind : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
    val seq : ('a, 'b) t -> ('a, 'c) t -> ('a, 'c) t
    val get : unit -> ('a, 'a) t
    val set : 'a -> ('a, unit) t
    val modify : ('a -> 'a) -> ('a, unit) t
    val run : ('a, 'b) t -> 'a -> 'a * 'b
    val exec : ('a, 'b) t -> 'a -> 'a
    val fold_leftM : ('a -> 'b -> ('c, 'a) t) -> 'a -> 'b list -> ('c, 'a) t
    module Infix :
      sig
        val ( <$> ) : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
        val ( >>= ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
        val ( =<< ) : ('a -> ('b, 'c) t) -> ('b, 'a) t -> ('b, 'c) t
        val ( >> ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'c) t
        val ( let* ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
      end
  end
