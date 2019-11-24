open! Core_kernel

type t =
  | Change_text_to of { text : string }
  | Replace_with_element of { node_name : string }
  | Replace_with_text of { text : string }
  | Start_attrs
  | Insert_text_before of { text : string }
  | Insert_element_before of { node_name : string }
  | Insert_text_after of { text : string }
  | Insert_element_after of { node_name : string }
  | Remove_attribute of { key : string }
  | Add_attribute of
      { key : string
      ; data : string
      }
  | End_attrs
  | Start_children
  | Skip of { how_many : int }
  | Descend
  | Ascend
  | Remove_current
  | End_children
[@@deriving sexp]
