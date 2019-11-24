open! Core_kernel
open! Import

let rec f node ~is_top ~send =
  match node with
  | Node.Text text ->
    if is_top
    then send (Replace_with_text { text })
    else send (Insert_text_after { text })
  | Node.Element ({ node_name; _ } as e) ->
    if is_top
    then send (Replace_with_element { node_name })
    else send (Insert_element_after { node_name });
    if Map.is_empty e.attrs
    then ()
    else (
      send Start_attrs;
      let f ~key ~data = send (Add_attribute { key; data }) in
      Element.iter_attrs e ~f;
      send End_attrs);
    if Element.has_no_children e
    then ()
    else (
      send Start_children;
      Element.iter_children e ~f:(f ~is_top:false ~send);
      send End_children)
;;

let inserting node ~send = f node ~is_top:false ~send
let replacing node ~send = f node ~is_top:true ~send
