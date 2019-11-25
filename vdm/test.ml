open! Core_kernel
open! Import
open Node

let type_id = Type_equal.Id.create ~name:"foo" sexp_of_opaque

let element_l
    node_name
    ?(attrs = String.Map.empty)
    ?(children = [])
    ()
  =
  let children = Linear_children.of_list children in
  Element
    { Element.node_name; attrs; children = Element.Linear children }
;;

let element_o
    node_name
    ?(attrs = String.Map.empty)
    ?(children = Int.Map.empty)
    ()
  =
  let children = Ordered_children.make ~type_id ~map:children in
  Element
    { Element.node_name; attrs; children = Element.Ordered children }
;;

let div = element_l "div"
let empty_div () = element_l "div" ()
let empty_span () = element_l "span" ()

let div_o ~children =
  element_o "div" ~children:(Int.Map.of_alist_exn children)
;;

let print_diff a b =
  Diff.diff a b ~send:(fun i ->
      i |> Instruction.sexp_of_t |> print_s)
;;

let%expect_test "text_to_text" =
  let a = Text "hi" in
  let b = Text "bye" in
  print_diff a b;
  [%expect "(Change_text_to (text bye))"]
;;

let%expect_test "element_to_text" =
  let a = empty_div () in
  let b = Text "bye" in
  print_diff a b;
  [%expect "(Change_text_to (text bye))"]
;;

let%expect_test "element_to_element_different_kind" =
  let a = empty_div () in
  let b = empty_span () in
  print_diff a b;
  [%expect {| (Replace_with_element (node_name span)) |}]
;;

let%expect_test "element_to_element_same_kind" =
  let a = empty_div () in
  let b = empty_div () in
  print_diff a b;
  [%expect ""]
;;

let%expect_test "element_to_element_same_kind" =
  let a = empty_div () in
  let b = a in
  print_diff a b;
  [%expect ""]
;;

let%expect_test "div that gained a child" =
  let a = empty_div () in
  let b = div ~children:[ Text "hi" ] () in
  print_diff a b;
  [%expect
    {|
      Start_children 
      (Insert_text_after (text hi))
      End_children
  |}]
;;

let%expect_test "div with insertions" =
  let a = empty_div () in
  let b = div ~children:[ Text "hi" ] () in
  print_diff a b;
  [%expect
    {|
      Start_children 
      (Insert_text_after (text hi))
      End_children
  |}]
;;

let%expect_test "div with replacement" =
  let a = div ~children:[ Text "hi" ] () in
  let b = div ~children:[ Text "bye" ] () in
  print_diff a b;
  [%expect
    {|
      Start_children 
      Descend 
      (Change_text_to (text bye))
      Ascend
      End_children
  |}]
;;

let%expect_test "div with extra content" =
  let a = div ~children:[ Text "bye" ] () in
  let b = div ~children:[ Text "hi"; Text "bye" ] () in
  print_diff a b;
  [%expect
    {|
      Start_children 
      (Insert_text_after (text hi))
      (Skip (how_many 1))
      End_children
  |}]
;;

let%expect_test "div div insert" =
  let a = div ~children:[ empty_div () ] () in
  let b = div ~children:[ div ~children:[ Text "hi" ] () ] () in
  print_diff a b;
  [%expect
    {|
      Start_children 
      Descend 
      Start_children 
      (Insert_text_after (text hi))
      End_children
      Ascend
      End_children
  |}]
;;

let%expect_test "divi to divi text node" =
  let a = div ~children:[ Text "a" ] () in
  let b = div ~children:[ Text "a"; Text "b" ] () in
  print_diff a b;
  [%expect
    {|
      Start_children 
      (Insert_text_after (text a))
      Descend
      (Change_text_to (text b))
      Ascend
      End_children
  |}]
;;

let%expect_test "divi to divi text node" =
  let a = div ~children:[ Text "b" ] () in
  let b = div ~children:[ Text "a"; Text "b" ] () in
  print_diff a b;
  [%expect
    {|
      Start_children 
      (Insert_text_after (text a))
      (Skip (how_many 1))
      End_children
  |}]
;;

let%expect_test "divi to divo text node" =
  let a = div ~children:[ Text "a" ] () in
  let b = div_o ~children:[ 1, Text "a"; 2, Text "b" ] () in
  print_diff a b;
  [%expect
    {|
      Start_children 
      (Insert_text_after (text a))
      Descend
      (Change_text_to (text b))
      Ascend
      End_children
  |}]
;;

let%expect_test "divi to divo" =
  let a = div ~children:[ empty_div () ] () in
  let b = div_o ~children:[ 1, empty_span () ] () in
  print_diff a b;
  [%expect
    {|
      Start_children 
      (Insert_element_after (node_name span))
      Remove_current
      End_children
  |}]
;;

let%expect_test "div div change" =
  let a = div ~children:[ div ~children:[ Text "xx" ] () ] () in
  let b = div ~children:[ div ~children:[ Text "yy" ] () ] () in
  print_diff a b;
  [%expect
    {|
      Start_children 
      Descend 
      Start_children 
      Descend 
      (Change_text_to (text yy))
      Ascend
      End_children
      Ascend
      End_children
  |}]
;;
