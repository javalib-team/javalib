
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
    in aux 0 [] l
    
       
end                  

                  
                
