open! Js_of_ocaml

external swap_js_dom: unit -> unit = "swap_js_dom"

let swap  = swap_js_dom

let%expect_test _ = 
    failwith "hi"
