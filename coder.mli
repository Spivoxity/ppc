(* ppcu/coder.ml *)
(* Copyright (c) 2017--18 J. M. Spivey *)

module F(Target : Target.T) : sig
  val translate :
    Optree.symbol -> int -> int -> int -> int -> Optree.optree list -> unit
end

val debug : int ref
val optlevel : int ref
