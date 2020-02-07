
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

  include CCList (* Containers *)

  let split_nth = take_drop

  let remove_all l x = filter (fun y -> x<>y) l
       
end                  

                  
                
