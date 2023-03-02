open Javalib
open JBasics
open JCode

let get_jopcodes c =
  match c with
  | JInterface _ ->
      assert false
  | JClass _ -> (
      let methods = get_methods c in
      let m = List.hd @@ MethodMap.value_elements methods in
      match m with
      | AbstractMethod _ ->
          assert false
      | ConcreteMethod cm -> (
        match cm.cm_implementation with
        | Native ->
            assert false
        | Java code ->
            let jcode = Lazy.force code in
            jcode.c_code ) )

let rec map_option (f : 'a -> 'b option) (l : 'a list) : 'b list =
  match l with
  | [] ->
      []
  | hd :: tl -> (
    match f hd with None -> map_option f tl | Some hd -> hd :: map_option f tl )
