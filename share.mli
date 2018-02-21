(* ppcu/share.mli *)
(* Copyright (c) 2017--18 J. M. Spivey *)

module F(Metrics : Target.MetricsT)(Alloc : Target.AllocT) : sig
  (* |traverse| -- find common subexpressions in a procedure body *)
  val traverse : Optree.optree list -> Optree.optree list
end

