module type S = sig
  type node

  module Text : sig
    type t

    val create_text : string -> t
    val change_text : t -> string -> unit
    val text_to_node : t -> node
    val node_to_text_exn : node -> t
  end

  module Element : sig
    type t

    val element_to_node : t -> node
    val node_to_element_exn : node -> t
    val create_element : kind:string -> t
    val add_attribue : t -> key:string -> value:string -> unit
    val remove_attribue : t -> key:string -> value:string -> unit
    val insert_before : parent:t -> target:node -> to_insert:node -> unit
    val append : parent:t -> to_insert:node -> unit
    val prepend : parent:t -> to_insert:node -> unit
  end
end
