open! Core_kernel

type 'a diff_element =
  | Removal of
      { rank : int
      ; value : 'a
      }
  | Insertion of
      { rank : int
      ; value : 'a
      }
  | Both_same of
      { rank : int
      ; left : 'a
      ; right : 'a
      }

let diff a b ~data_equal =
  let rank_for k =
    k
    |> Map.closest_key b `Less_than
    |> Option.bind ~f:(fun (k, _) -> Map.rank b k)
    |> Option.map ~f:(( + ) 1)
    |> Option.value ~default:0
  in
  let map_diff (key, diff) =
    let rank = rank_for key in
    match diff with
    | `Left value -> Removal { rank; value }
    | `Right value -> Insertion { rank; value }
    | `Unequal (left, right) -> Both_same { rank; left; right }
  in
  Map.symmetric_diff a b ~data_equal
  |> Sequence.map ~f:map_diff
  |> Sequence.to_list
;;
