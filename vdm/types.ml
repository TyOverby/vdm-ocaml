open! Core_kernel

type element =
  { kind : string
  ; attrs : string String.Map.t
  ; children : (node, [ `Read ]) Array.Permissioned.t
  }
[@@deriving sexp]

and text = String.t [@@deriving sexp]

and node =
  | Element of element
  | Text of text
[@@deriving sexp]

type instruction =
  | Change_text_to of string
  | Replace_with_element of element
  | Replace_with_text of string
  | Start_attrs
  | Remove_attribute of string
  | Add_attribute of string * string
  | End_attrs
  | Start_children
  | Skip of int
  | Descend
  | Ascend
  | Remove_current
  | Insert_before of node
  | End_children
[@@deriving sexp]
