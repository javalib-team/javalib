val key : out_channel -> 'a SeaOfNodes__Type.Son.key -> unit
val binop : out_channel -> SeaOfNodes__Type.Binop.t -> unit
val data : out_channel -> SeaOfNodes__Type.Data.t -> unit
val phi : out_channel -> SeaOfNodes__Type.Phi.t -> unit

val predecessor :
  out_channel -> SeaOfNodes__Type.Region.predecessor -> unit

val region : out_channel -> SeaOfNodes__Type.Region.t -> unit
val cond : out_channel -> SeaOfNodes__Type.Cond.t -> unit
val branch : out_channel -> SeaOfNodes__Type.Branch.t -> unit
val control : out_channel -> SeaOfNodes__Type.Control.t -> unit
val son : out_channel -> SeaOfNodes__Type.Son.t -> unit
