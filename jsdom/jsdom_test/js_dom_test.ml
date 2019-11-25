open Js_of_ocaml

let%expect_test _ = 
    Js_dom.swap
    failwith "hi"
