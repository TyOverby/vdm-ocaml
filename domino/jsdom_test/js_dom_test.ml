open! Js_of_ocaml

let%expect_test _ = 
  Firebug.console##log Dom_html.document;
  print_endline "hi"
