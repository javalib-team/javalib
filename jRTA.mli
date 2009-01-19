(** Builds high level representations of java byte-code programs using
    Rapid Type Analysis algorithm. *)

(** {2 Navigable hierarchy} *)

type class_name' = JProgram.class_name_index * JBasics.class_name
type method_signature' = JProgram.method_signature_index * JClass.method_signature

module ClassMethMap :
sig
  type key = class_name' * method_signature'
  type +'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

val print_callgraph :
  (('a * JBasics.class_name) * ('b * JClass.method_signature)) list
  ClassMethMap.t ->
  [< `Class of JProgram.class_file | `Interface of JProgram.interface_file ] JProgram.ClassMap.t -> unit
  
(* val start_rta : string -> JBasics.class_name -> JProgram.program *)

val parse_program : string -> JBasics.class_name -> JProgram.program
