open! Core_kernel

type 'node t [@@deriving equal]

val make
  :  type_id:('a * 'b * 'c) Type_equal.Id.t
  -> map:('a, 'b, 'c) Core_kernel.Map.t
  -> 'b t

val is_empty : _ t -> bool
val ro_array : 'node t -> ('node, [ `Read ]) Array.Permissioned.t
val iter : 'node t -> f:('node -> unit) -> unit
