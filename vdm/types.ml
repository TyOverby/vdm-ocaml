open! Core_kernel

module Element = struct
  type 'node kind =
    | Ordered : 'node Ordered_children.t -> 'node kind
    | Linear : 'node Linear_children.t -> 'node kind

  type 'node t =
    { node_name : string
    ; attrs : string String.Map.t
    ; children : 'node kind
    }
end

module Node = struct
  type t =
    | Element of t Element.t
    | Text of String.t
end
