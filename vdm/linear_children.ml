open! Core_kernel
open Instruction

type 'node t = ('node, [ `Read ]) Array.Permissioned.t

let equal = Array.Permissioned.equal
let of_array a = a |> Array.copy |> Array.Permissioned.of_array_id
let of_list = Array.Permissioned.of_list
let ro_array = Fn.id
let of_ro_array = Fn.id
let is_empty = Array.Permissioned.is_empty
let iter = Array.Permissioned.iter

let diff xs ys ~diff ~eq_kind ~eq_accurate ~send ~on_insert =
  let matrix = Lcs.Matrix.create xs ys ~eq:eq_kind in
  let diff_list = Lcs.diff matrix xs ys ~eq:eq_kind in
  List.iter diff_list ~f:(function
      | Both_same (a, b) when eq_accurate a b ->
        send (Skip { how_many = 1 })
      | Both_same (a, b) ->
        send Descend;
        diff a b;
        send Ascend
      | Removal _ -> send Remove_current
      | Insertion n -> on_insert n)
;;
