(* ppcu/treegen.mli *)
(* Copyright (c) 2017--18 J. M. Spivey *)

(* Intermediate code generator: this translates the program into optrees,
   and feeds them to the code generator one at a time. *)

module F(Targ : Target.T) : sig
  (* |translate| -- generate intermediate code *)
  val translate : Tree.program -> unit
end

(* Whether to include array bound and null pointer checks *)
val boundchk : bool ref
