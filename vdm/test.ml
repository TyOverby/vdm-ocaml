open! Core_kernel
open Types

let element node_name ?(attrs = String.Map.empty) ?(children = []) ()
  =
  let children = Linear_children.of_list children in
  Element
    { Element.node_name; attrs; children = Element.Linear children }
;;

let div = element "div"
let empty_div () = element "div" ()
let empty_span () = element "span" ()

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
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "not implemented")
  Raised at file "stdlib.ml", line 33, characters 17-33
  Called from file "vdm/test.ml", line 31, characters 2-16
  Called from file "collector/expect_test_collector.ml", line 225, characters 12-19

  Trailing output
  ---------------
  (Replace_with_element (node_name span)) |}]
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
      (Insert_text_before (text hi))
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
      (Insert_text_before (text hi))
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
      (Insert_text_before (text hi))
      Descend 
      Ascend
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
      (Insert_text_before (text hi))
      End_children
      Ascend
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
