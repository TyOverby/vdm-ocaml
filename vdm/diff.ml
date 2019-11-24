open! Core_kernel
open! Import
open Node

let rec diff a b ~send =
  if phys_equal a b
  then ()
  else (
    match a, b with
    | Text a, Text b when String.equal a b -> ()
    | Text _, Text text | Element _, Text text ->
      send (Change_text_to { text })
    | Text _, Element { node_name; _ } ->
      send (Replace_with_element { node_name });
      failwith "not implemented"
    | Element a, Element b
      when not (String.equal a.node_name b.node_name) ->
      send (Replace_with_element { node_name = b.node_name });
      failwith "not implemented"
    | (( (Element { children = Element.Linear _; _ } as _a)
       , Element { children = Element.Ordered _; _ } ) as _b)
    | (( (Element { children = Element.Ordered _; _ } as _a)
       , Element { children = Element.Linear _; _ } ) as _b) ->
      failwith "unimplemented"
    | ( Element
          { attrs = _a_attrs
          ; children = Element.Ordered _a_children
          ; _
          }
      , Element
          { attrs = _b_attrs
          ; children = Element.Ordered _b_children
          ; _
          } ) -> failwith "not implemented"
    | ( Element
          { attrs = a_attrs
          ; children = Element.Linear a_children
          ; _
          }
      , Element
          { attrs = b_attrs
          ; children = Element.Linear b_children
          ; _
          } ) ->
      let attrs_diff =
        Map.symmetric_diff ~data_equal:String.equal a_attrs b_attrs
        |> Sequence.to_list
      in
      if List.is_empty attrs_diff
      then ()
      else (
        send Start_attrs;
        List.iter attrs_diff ~f:(function
            | key, `Left _ -> send (Remove_attribute { key })
            | key, `Right value | key, `Unequal (_, value) ->
              send (Add_attribute { key; value }));
        send End_attrs);
      if phys_equal a_children b_children
      then ()
      else (
        send Start_children;
        diff_linear_children a_children b_children ~send;
        send End_children))

and diff_linear_children xs ys ~send =
  let eq a b =
    phys_equal a b
    ||
    match a, b with
    | Element a, Element b -> String.equal a.node_name b.node_name
    | Text _, Text _ -> true
    | _ -> false
  in
  let diff = diff ~send in
  Linear_children.diff xs ys ~diff ~eq ~send ~on_insert:(function
      | Text text -> send (Insert_text_before { text })
      | Element { node_name; _ } ->
        send (Insert_element_before { node_name });
        failwith "unimplemented")
;;
