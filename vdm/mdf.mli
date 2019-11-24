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

val diff
  :  ('k, 'v, 'cmp) Map.t
  -> ('k, 'v, 'cmp) Map.t
  -> data_equal:('v -> 'v -> bool)
  -> 'v diff_element list
