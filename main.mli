(* ppcu/main.mli *)
(* Copyright (c) 2017--18 J. M. Spivey *)

(* The main program *)
module F(Target : Target.T) : sig
  val main : unit -> unit
end

val pic_mode : bool ref
