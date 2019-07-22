open Core_kernel

type 'a ro_array = ('a, [ `Read ]) Core_kernel.Array.Permissioned.t

let ( = ) = Core_kernel.Int.( = )
let ( > ) = Core_kernel.Int.( > )

module Matrix = struct
  type t = int ro_array ro_array

  let create x y ~eq =
    let xl, yl = Array.Permissioned.length x, Array.Permissioned.length y in
    let matrix = Array.create ~len:(xl + 1) [||] in
    Array.map_inplace matrix ~f:(fun _ -> Array.create ~len:(yl + 1) 0);
    Array.Permissioned.iteri x ~f:(fun i xi ->
        Array.Permissioned.iteri y ~f:(fun k yk ->
            matrix.(i + 1).(k + 1)
              <- (if eq xi yk
                 then 1 + matrix.(i).(k)
                 else Int.max matrix.(i + 1).(k) matrix.(i).(k + 1))));
    matrix
    |> Array.map ~f:Array.Permissioned.of_array_id
    |> Array.Permissioned.of_array_id
  ;;
end

type 'a diff_element =
  | Removal of 'a
  | Insertion of 'a
  | Both_same of 'a * 'a

let rec build_diff c x y ~eq i k =
  let module Diff_type = struct
    type t =
      | Add
      | Nop
      | Rem
  end
  in
  let open Diff_type in
  let get1 = Array.Permissioned.get in
  let get2 t a b = get1 (get1 t a) b in
  let diff_type =
    if i = 0
    then Add
    else if k = 0
    then Rem
    else if eq (get1 x (i - 1)) (get1 y (k - 1))
    then Nop
    else if get2 c i (k - 1) > get2 c (i - 1) k
    then Add
    else Rem
  in
  match diff_type with
  | _ when i = 0 && k = 0 -> []
  | Add ->
    let a = Insertion (get1 y (k - 1)) in
    let r = build_diff c x y ~eq i (k - 1) in
    a :: r
  | Nop ->
    let a = Both_same (get1 x (i - 1), get1 y (k - 1)) in
    let r = build_diff c x y ~eq (i - 1) (k - 1) in
    a :: r
  | Rem ->
    let a = Removal (get1 x (i - 1)) in
    let r = build_diff c x y ~eq (i - 1) k in
    a :: r
;;

let diff c x y ~eq =
  let xl, yl = Array.Permissioned.length x, Array.Permissioned.length y in
  build_diff c x y xl yl ~eq |> List.rev
;;
