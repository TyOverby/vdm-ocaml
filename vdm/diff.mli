open! Core_kernel
open! Import

val diff : Node.t -> Node.t -> send:(Instruction.t -> unit) -> unit
