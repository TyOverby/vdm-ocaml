open! Core_kernel
open! Import

val replacing : Node.t -> send:(Instruction.t -> unit) -> unit
val inserting : Node.t -> send:(Instruction.t -> unit) -> unit
