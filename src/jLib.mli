module DynArray : sig

  type 'a t
  val create : unit -> 'a t
  val length : 'a t -> int
  val index_of : ('a -> bool) -> 'a t -> int
  val add : 'a t -> 'a -> unit
  val unsafe_get : 'a t -> int -> 'a
  val of_array : 'a array -> 'a t
  val to_array : 'a t -> 'a array
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
end
     
module IO : sig

  type input
  type 'a output
  exception Overflow of string
  val input_channel : in_channel -> input
  val output_channel : out_channel -> unit output
    
  val input_string : string -> input
  val flush : 'a output -> unit                      
  val read_byte : input -> int
  val read_signed_byte : input -> int
  val really_nread : input -> int -> Bytes.t
  val really_nread_string : input -> int -> string
  val write_byte : 'a output -> int -> unit
  val nwrite_string : 'a output -> string -> unit
  val output_string : unit -> string output
  val close_out : 'a output -> 'a
  val close_in : input -> unit
  val printf : 'a output -> ('b, unit, string, unit) format4 -> 'b
  val write : 'a output -> char -> unit
  val pos_in : input -> input * (unit -> int)
  val pos_out : 'a output -> 'a output * (unit -> int)
  val input_bytes : Bytes.t -> input

  module BigEndian : sig

    val read_ui16 : input -> int
    val read_i16 : input -> int
    val read_i32 : input -> int
    val read_real_i32 : input -> int32
    val read_i64 : input -> int64      
    val read_double : input -> float
    val write_ui16 : 'a output -> int -> unit
    val write_i16 : 'a output -> int -> unit
    val write_i32 : 'a output -> int -> unit      
    val write_real_i32 : 'a output -> int32 -> unit
    val write_i64 : 'a output -> int64 -> unit
    val write_double : 'a output -> float -> unit
  end
     
end

module UChar : sig

  type t
  val of_char : char -> t

  
end
     
module UTF8 : sig

  type t = string 
  type index = int 
  val validate : t -> unit
  val out_of_range : t -> index -> bool
  val look : t -> index -> UChar.t         
  val next : t -> index -> index
  module Buf : sig
    type buf = Buffer.t 
    val create : int -> buf
    val add_char : buf -> UChar.t -> unit
    val contents : buf -> t
  end       
end


module String : sig

  val nsplit : string -> string -> string list
  val length : string -> int
  val strip : ?chars:string -> string -> string    
   
end
                  
module Option : sig

  val may : ('a -> unit) -> 'a option -> unit
  val map_default : ('a -> 'b) -> 'b -> 'a option -> 'b
end

                 
module Array : sig

  val length : 'a array -> int
  val get : 'a array -> int -> 'a
  val findi : ('a -> bool) -> 'a array -> int
  
end

module List : sig

  val length : 'a list -> int
  val init : int -> (int -> 'a) -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val drop : int -> 'a list -> 'a list
  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  val remove_all : 'a list -> 'a -> 'a list
  val split_nth : int -> 'a list -> 'a list * 'a list
  val map : ('a -> 'b) -> 'a list -> 'b list
  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val iteri : (int -> 'a -> unit) -> 'a list -> unit
  
end
