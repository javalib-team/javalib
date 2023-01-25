
module DynArray = struct

  include DynArray (* ExtLib *)

end

module IO = struct

  include IO (* ExtLib *)
        
end

module String = struct

  include ExtString.String (* ExtLib *)
  
end

module Option = struct

  include Option (* ExtLib *)
        
end

module Array = struct

  include ExtArray.Array (* ExtLib *)
  
end
                 
module List = struct

  include List
  
  let rec drop n l  =
    match l with
    | _ :: l when n > 0 -> drop (n-1) l
    | _ -> l

  let remove_all l x = filter (fun y -> x<>y) l

  exception Invalid_index of int
      
  let split_nth n l =
    let rec aux i l1 l2 =
      match i, l2 with
      | 0, _ -> (List.rev l1, l2)
      | _, [] -> raise (Invalid_index n)
      | _, x :: l2 -> aux (i-1) (x::l1) l2
    in
    aux n [] l
    
  let%test "split_nth regular" =
    let l = [0; 1; 2; 3; 4; 5; 6] in
    split_nth 3 l = ([0; 1; 2], [3; 4; 5; 6])
  
  let%test "split_nth nil snd" =
    let l = [0; 1; 2; 3; 4; 5; 6] in
    split_nth 0 l = ([], l)
  
  let%test "split_nth nil fst" =
    let l = [0; 1; 2; 3; 4; 5; 6] in
    split_nth 7 l = (l, [])
  
  let%test "split_nth invalid index too big" =
    let l = [0; 1; 2; 3; 4; 5; 6] in
    try (
      let _ = split_nth 8 l in
      false
    ) with Invalid_index n -> n = 8
  
  let%test "split_nth invalid index negative" =
    let l = [0; 1; 2; 3; 4; 5; 6] in
    try (
      let _ = split_nth (-8) l in
      false
    ) with Invalid_index n -> n = -8
  
end                  

                  
                
