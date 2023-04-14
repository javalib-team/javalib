module State : sig
  type ('s, 'a) t = State of ('s -> 's * 'a)

  val return : 'a -> ('b, 'a) t

  val fmap : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t

  val bind : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t

  val seq : ('a, 'b) t -> ('a, 'c) t -> ('a, 'c) t

  val get : unit -> ('a, 'a) t

  val gets : ('a -> 'b) -> ('a, 'b) t

  val set : 'a -> ('a, unit) t

  val modify : ('a -> 'a) -> ('a, unit) t

  val run : ('a, 'b) t -> 'a -> 'a * 'b

  val exec : ('a, 'b) t -> 'a -> 'a

  val fold_leftM : ('a -> 'b -> ('c, 'a) t) -> 'a -> 'b list -> ('c, 'a) t

  val array_iterM : (int -> 'a -> ('s, unit) t) -> 'a array -> ('s, unit) t

  module Infix : sig
    val ( <$> ) : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t

    val ( >>= ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t

    val ( =<< ) : ('a -> ('b, 'c) t) -> ('b, 'a) t -> ('b, 'c) t

    val ( >> ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'c) t

    val ( let* ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  end
end

module Writer : functor
  (M : sig
     type t

     val empty : t

     val append : t -> t -> t
   end)
  -> sig
  type 'a t

  val return : 'a -> 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val seq : 'a t -> 'b t -> 'b t

  val tell : M.t -> unit t

  val run : 'a t -> M.t * 'a

  val exec : 'a t -> M.t

  val fold_leftM : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t

  val array_iterM : (int -> 'a -> unit t) -> 'a array -> unit t

  module Infix : sig
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

    val ( >>= ) : 'b t -> ('b -> 'c t) -> 'c t

    val ( =<< ) : ('a -> 'c t) -> 'a t -> 'c t

    val ( >> ) : 'b t -> 'c t -> 'c t

    val ( let* ) : 'b t -> ('b -> 'c t) -> 'c t
  end
end
