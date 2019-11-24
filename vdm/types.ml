open! Core_kernel

module Linear_children = struct
  type 'node t = ('node, [ `Read ]) Array.Permissioned.t
end

module Ordered_children = struct
  type 'node t =
    | Hidden :
        { map : ('k, 'node, 'cmp) Map.t
        ; type_id : ('k * 'node * 'cmp) Type_equal.Id.t
        }
        -> 'node t
end

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

type node =
  | Element of node Element.t
  | Text of String.t

type instruction =
  | Change_text_to of { text : string }
  | Replace_with_element of { node_name : string }
  | Replace_with_text of { text : string }
  | Start_attrs
  | Insert_text_before of { text : string }
  | Insert_element_before of { node_name : string }
  | Remove_attribute of { key : string }
  | Add_attribute of
      { key : string
      ; value : string
      }
  | End_attrs
  | Start_children
  | Skip of { how_many : int }
  | Descend
  | Ascend
  | Remove_current
  | End_children
[@@deriving sexp]
