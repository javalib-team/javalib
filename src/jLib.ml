
module DynArray = struct

  include DynArray (* ExtLib *)

end

module IO = struct

  include IO (* ExtLib *)
        
end

module UTF8 = struct
  
  include CamomileLibrary.UTF8 (* Camomile *)
        
end

module UChar = struct

  include CamomileLibrary.UChar (* Camomile *)
  
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

  include ExtList.List (* ExtLib *)
  
end                  

                  
                
