(* ppcu/simp.ml *)
(* Copyright (c) 2017--18 J. M. Spivey *)

open Optree

(* |swap| -- find reverse operation or raise Not_found *)
let swap =
  function Plus -> Plus | Times -> Times | Eq -> Eq | Lt -> Gt 
    | Gt -> Lt | Leq -> Geq | Geq -> Leq | Neq -> Neq 
    | And -> And | Or -> Or
    | _ -> raise Not_found

(* |is_const| -- test if expression is a constant *)
let is_const = function <CONST a> -> true | <NIL> -> true | _ -> false

(* |simp| -- simplify an expression tree at the root *)
let rec simp t =
  match t with
    (* Constant folding *)
      <BINOP w, <CONST a>, <CONST b>> ->
	<CONST (do_binop w a b)>
    | <MONOP w, <CONST a>> ->
	<CONST (do_monop w a)>
  
    (* Static bound checks *)
    | <BOUND, <CONST k>, <CONST b>> -> 
	if 0 <= k && k < b then <CONST k> else t

    (* Simplifications -- mainly directed at addressing calculations *)
    | <BINOP Plus, <BINOP Plus, t1, <CONST a>>, <CONST b>> ->
        simp <BINOP Plus, t1, <CONST (a+b)>>

    | <BINOP Plus, t1, <CONST a>> when a < 0 ->
      	<BINOP Minus, t1, <CONST (-a)>>
    | <BINOP Minus, t1, <CONST a>> when a < 0 -> 
	<BINOP Plus, t1, <CONST (-a)>>

    | <OFFSET, <LOCAL a>, <CONST b>> ->
	<LOCAL (a+b)>
    | <OFFSET, <OFFSET, t1, <CONST a>>, <CONST b>> ->
        simp <OFFSET, t1, <CONST (a+b)>>
    | <OFFSET, t1, <CONST 0>> ->
        t1
    | <BINOP Times, <BINOP Times, t1, <CONST a>>, <CONST b>> ->
	simp <BINOP Times, t1, <CONST (a * b)>>
    | <BINOP Times, <BINOP Plus, t1, <CONST a>>, <CONST b>> ->
	simp <BINOP Plus, 
	  simp <BINOP Times, t1, <CONST b>>, 
	  <CONST (a*b)>>
    | <BINOP Times, <BINOP Minus, t1, <CONST a>>, <CONST b>> ->
	simp <BINOP Minus, 
	  simp <BINOP Times, t1, <CONST b>>, 
	  <CONST (a*b)>>
    | <OFFSET, t1, <BINOP Plus, t2, t3>> ->
	simp <OFFSET, simp <OFFSET, t1, t2>, t3>
    | <OFFSET, t1, <BINOP Minus, t2, <CONST n>>> ->
	simp <OFFSET, simp <OFFSET, t1, t2>, <CONST (-n)>>
    | <BINOP Times, t1, <CONST 1>> -> t1
    | <BINOP Times, t1, <CONST n>> when n > 0 -> 
        begin try 
          let k = Util.exact_log2 n in
          <BINOP Lsl, t1, <CONST k>>
        with Not_found ->
          t
        end
    | <BINOP Plus, t1, <CONST 0>> -> t1
    | <BINOP Minus, t1, <CONST 0>> -> t1

    (* Swap operands to put constant on right *)
    | <BINOP w, t1, t2> when is_const t1 ->
	if is_const t2 || not (Util.can swap w) then t else
	  simp <BINOP (swap w), t2, t1>
    | <JUMPC (w, lab), t1, t2> when is_const t1 ->
	if is_const t2 then t else
	  simp <JUMPC (swap w, lab), t2, t1>

    (* Eliminate trivial static links *)
    | <CALL n, @(fn :: <STATLINK, <NIL>> :: args)> ->
        <CALL n, @(fn :: args)>
    | <CALLW n, @(fn :: <STATLINK, <NIL>> :: args)> ->
        <CALLW n, @(fn :: args)>
    | <CALLQ n, @(fn :: <STATLINK, <NIL>> :: args)> ->
        <CALLQ n, @(fn :: args)>

    | _ -> t

(* |simplify| -- recursively simplify an expression *)
let rec simplify <x, @ts> = simp <x, @(List.map simplify ts)>

(* |optimise| -- simplify a procedure body *)
let optimise prog = 
  List.map simplify prog

