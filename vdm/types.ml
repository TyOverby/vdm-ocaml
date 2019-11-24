open! Core_kernel

module Element = struct
  type 'node kind =
    | Ordered of 'node Ordered_children.t
    | Linear of 'node Linear_children.t
  [@@deriving equal]

  type 'node t =
    { node_name : string
    ; attrs : string String.Map.t
    ; children : 'node kind
    }
  [@@deriving equal]

  let has_no_children { children; _ } =
    match children with
    | Ordered c -> Ordered_children.is_empty c
    | Linear c -> Linear_children.is_empty c
  ;;

  let iter_attrs { attrs; _ } = String.Map.iteri attrs

  let iter_children { children; _ } =
    match children with
    | Ordered c -> Ordered_children.iter c
    | Linear c -> Linear_children.iter c
  ;;
end

module Node = struct
  type t =
    | Element of t Element.t
    | Text of String.t
  [@@deriving equal]
end
