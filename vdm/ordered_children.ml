open! Core_kernel
open Instruction

type 'node t =
  | Hidden :
      { map : ('k, 'node, 'cmp) Map.t
      ; type_id : ('k * 'node * 'cmp) Type_equal.Id.t
      }
      -> 'node t

let equal v_equal a b =
  let (Hidden { map = map_a; type_id = type_id_a }) = a in
  let (Hidden { map = map_b; type_id = type_id_b }) = b in
  match Type_equal.Id.same_witness type_id_a type_id_b with
  | Some T -> Map.equal v_equal map_a map_b
  | None -> false
;;

let make ~type_id ~map = Hidden { map; type_id }
let is_empty (Hidden { map; _ }) = Map.is_empty map
let iter (Hidden { map; _ }) = Map.iter map

let ro_array (Hidden { map; _ }) =
  let len = Map.length map in
  let array =
    match Map.min_elt map with
    | Some (_, v) -> Array.create ~len v
    | None -> [||]
  in
  let i = ref 0 in
  Map.iter map ~f:(fun v ->
      array.(!i) <- v;
      Int.incr i);
  Array.Permissioned.of_array_id array
;;

let actual_diff a b ~send ~data_equal ~on_insert ~diff =
  let ptr = ref 0 in
  Mdf.diff a b ~data_equal
  |> List.iter ~f:(fun e ->
         let rank = Mdf.rank e in
         send (Skip { how_many = rank - !ptr });
         ptr := rank;
         match e with
         | Removal _ -> send Remove_current
         | Insertion { value; _ } -> on_insert value
         | Both_same { left; right; _ } -> diff left right)
;;

let diff a b ~send ~on_insert ~diff ~eq_accurate =
  let (Hidden { map = map_a; type_id = type_id_a }) = a in
  let (Hidden { map = map_b; type_id = type_id_b }) = b in
  match Type_equal.Id.same_witness type_id_a type_id_b with
  | Some T ->
    actual_diff
      map_a
      map_b
      ~data_equal:eq_accurate
      ~send
      ~on_insert
      ~diff
  | None -> failwith "unimplemented type-equal differing witness"
;;
