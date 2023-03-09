module State = struct
  type ('s, 'a) t = State of ('s -> 's * 'a)

  let return x = State (fun s ->
      (s, x)
    )

  let fmap f (State g) = State (fun s ->
      let (s', x) = g s in
      (s', f x)
    )

  let bind (State g) f = State (fun s ->
      let (s', x) = g s in
      let State g' = f x in
      g' s'
    )

  let seq x y = bind x (fun _ -> y)

  let gets f = State (fun s ->
      (s, f s)
    )

  let get () = gets Fun.id

  let set s = State (fun _ ->
      (s, ())
    )

  let modify f =
    bind (fmap f @@ get ()) set

  let run (State g) s =
    g s

  let exec m s = fst (run m s)

  let rec fold_leftM f acc l =
    match l with
    | [] -> return acc
    | x::xs ->
      (* the [@tailcall] attribute is required to ensure that the
         compiler optimization is applied correctly *)
      bind (f acc x) (fun w -> (fold_leftM [@tailcall]) f w xs)

  module Infix = struct
    let (<$>) f x = fmap f x
    let (>>=) x f = bind x f
    let (=<<) f x = bind x f
    let (>>) x y = seq x y
    let (let*) x f = bind x f
  end
end
