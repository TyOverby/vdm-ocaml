type 'a ro_array = ('a, [ `Read ]) Core_kernel.Array.Permissioned.t

module Matrix : sig
  type t

  val create : 'a ro_array -> 'a ro_array -> eq:('a -> 'a -> bool) -> t
end

type 'a diff_element =
  | Removal of 'a
  | Insertion of 'a
  | Both_same of 'a * 'a

val diff
  :  Matrix.t
  -> 'a ro_array
  -> 'a ro_array
  -> eq:('a -> 'a -> bool)
  -> 'a diff_element list
