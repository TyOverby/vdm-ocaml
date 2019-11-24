open! Core_kernel

type 'node t [@@deriving equal]

val of_array : 'node array -> 'node t
val of_ro_array : ('node, [ `Read ]) Array.Permissioned.t -> 'node t
val of_list : 'node list -> 'node t
val ro_array : 'node t -> ('node, [ `Read ]) Array.Permissioned.t
val is_empty : _ t -> bool
val iter : 'node t -> f:('node -> unit) -> unit

val diff
  :  'node t
  -> 'node t
  -> diff:('node -> 'node -> unit)
  -> eq_kind:('node -> 'node -> bool)
  -> eq_accurate:('node -> 'node -> bool)
  -> send:(Instruction.t -> unit)
  -> on_insert:('node -> unit)
  -> unit
