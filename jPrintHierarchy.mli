
(** [get_hierachy prog info] returns [info] enriched with information
    from the hierachy such as subclasses, subinterfaces and
    implementations. *)
val get_hierachy : JProgram.program -> JPrint.info -> JPrint.info
