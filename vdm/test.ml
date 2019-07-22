open! Core_kernel
open Types

let element kind ?(attrs = String.Map.empty) ?(children = []) () =
  let children = Array.Permissioned.of_list children in
  Element { kind; attrs; children }
;;

let div = element "div"
let empty_div () = element "div" ()
let empty_span () = element "span" ()
let print_diff a b = Diff.diff a b ~send:(fun i -> print_s (sexp_of_instruction i))

let%expect_test "text_to_text" =
  let a = Text "hi" in
  let b = Text "bye" in
  print_diff a b;
  [%expect "(Change_text_to bye)"]
;;

let%expect_test "element_to_text" =
  let a = empty_div () in
  let b = Text "bye" in
  print_diff a b;
  [%expect "(Replace_with_text bye)"]
;;

let%expect_test "element_to_element_different_kind" =
  let a = empty_div () in
  let b = empty_span () in
  print_diff a b;
  [%expect "(Replace_with_element ((kind span) (attrs ()) (children ())))"]
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
      (Insert_before (Text hi))
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
      (Insert_before (Text hi))
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
      (Change_text_to bye)
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
      (Insert_before (Text hi))
      Descend 
      Ascend
      End_children
  |}]
;;

let%expect_test "div div insert" =
  let a = div ~children:[ empty_div () ] () in
  let b = div ~children:[ div ~children:[ Text "hi"] ()] () in
  print_diff a b;
  [%expect
    {|
      Start_children 
      Descend 
      Start_children 
      (Insert_before (Text hi))
      End_children
      Ascend
      End_children
  |}]
;;

let%expect_test "div div change" =
  let a = div ~children:[ div ~children:[ Text "xx"] ()] () in
  let b = div ~children:[ div ~children:[ Text "yy"] ()] () in
  print_diff a b;
  [%expect
    {|
      Start_children 
      Descend 
      Start_children 
      Descend 
      (Change_text_to yy)
      Ascend
      End_children
      Ascend
      End_children
  |}]
;;
