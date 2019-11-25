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
    | Element a, (Element b as e)
      when not (String.equal a.node_name b.node_name) ->
      Mount.replacing e ~send
    | ( Element
          ({ node_name = _; attrs = a_attrs; children = a_children }
          as a)
      , Element
          ({ node_name = _; attrs = b_attrs; children = b_children }
          as b) ) ->
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
            | key, `Right data | key, `Unequal (_, data) ->
              send (Add_attribute { key; data }));
        send End_attrs);
      let open Element in
      (match a_children, b_children with
      | a, b when phys_equal a b -> ()
      | _, _
        when Element.has_no_children a && Element.has_no_children b
        -> ()
      | Linear a, Linear b ->
        send Start_children;
        diff_linear_children a b ~send;
        send End_children
      | Ordered a, Linear b ->
        let a =
          a
          |> Ordered_children.ro_array
          |> Linear_children.of_ro_array
        in
        send Start_children;
        diff_linear_children a b ~send;
        send End_children
      | Linear a, Ordered b ->
        let b =
          b
          |> Ordered_children.ro_array
          |> Linear_children.of_ro_array
        in
        send Start_children;
        diff_linear_children a b ~send;
        send End_children
      | Ordered a, Ordered b ->
        send Start_children;
        diff_ordered_children a b ~send;
        send End_children))

and eq_accurate a b = phys_equal a b || Node.equal a b

and diff_ordered_children xs ys ~send =
  let on_insert = Mount.inserting ~send in
  let diff = diff ~send in
  Ordered_children.diff xs ys ~send ~diff ~on_insert ~eq_accurate

and diff_linear_children xs ys ~send =
  let eq_kind a b =
    phys_equal a b
    ||
    match a, b with
    | Element a, Element b -> String.equal a.node_name b.node_name
    | Text _, Text _ -> true
    | _ -> false
  in
  let diff = diff ~send in
  let on_insert = Mount.inserting ~send in
  Linear_children.diff
    xs
    ys
    ~diff
    ~eq_kind
    ~eq_accurate
    ~send
    ~on_insert
;;
