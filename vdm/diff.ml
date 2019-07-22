open Core_kernel
open Types
open Lcs

let rec diff a b ~send =
  if phys_equal a b
  then ()
  else (
    match a, b with
    | Text a, Text b when String.equal a b -> ()
    | Text _, Text t -> send (Change_text_to t)
    | Text _, Element e -> send (Replace_with_element e)
    | Element _, Text t -> send (Replace_with_text t)
    | Element a, Element b when not (String.equal a.kind b.kind) ->
      send (Replace_with_element b)
    | Element a, Element b ->
      let attrs_diff =
        Map.symmetric_diff ~data_equal:String.equal a.attrs b.attrs |> Sequence.to_list
      in
      if List.is_empty attrs_diff
      then ()
      else (
        send Start_attrs;
        attrs_diff
        |> List.iter ~f:(function
               | k, `Left _ -> send (Remove_attribute k)
               | k, `Right v | k, `Unequal (_, v) -> send (Add_attribute (k, v)));
        send End_attrs);
      if phys_equal a.children b.children
      then ()
      else (
        send Start_children;
        diff_children a.children b.children ~send;
        send End_children))

and diff_children xs ys ~send =
  let eq a b =
    phys_equal a b
    ||
    match a, b with
    | Element a, Element b -> String.equal a.kind b.kind
    | Text _, Text _ -> true
    | _ -> false
  in
  let matrix = Lcs.Matrix.create xs ys ~eq in
  let diff_list = Lcs.diff matrix xs ys ~eq in
  List.iter diff_list ~f:(function
      | Both_same (a, b) when phys_equal a b -> send (Skip 1)
      | Both_same (a, b) ->
        send Descend;
        diff a b ~send;
        send Ascend
      | Removal _ -> send Remove_current
      | Insertion a -> send (Insert_before a))
;;

(*
  let xl, yl = Array.Permissioned.length xs, Array.Permissioned.length ys in
  let rec _left_start_offset i xs ys =
    if i >= xl || i >= yl
    then i
    else (
      let a, b = Array.Permissioned.get xs i, Array.Permissioned.get ys i in
      if phys_equal a b then _left_start_offset (i + 1) xs ys else i)
  in
  let rec _right_start_offset i xs ys =
    if i >= xl || i >= yl
    then i
    else (
      let a, b =
        Array.Permissioned.get xs (xl - i - 1), Array.Permissioned.get ys (yl - i - 1)
      in
      if phys_equal a b then _right_start_offset (i + 1) xs ys else i)
  in
*)
