(* ppcu/share.ml *)
(* Copyright (c) 2017--18 J. M. Spivey *)

open Print
open Optree
open Target

(* The function |traverse| transforms a list of optrees by finding 
   common subexpressions; for each of them, it allocates a temp with
   an initializing assignment, then replaces all occurrences of the
   subexpression with a use of the temp.  Procedure calls are also moved
   to the top level by a similar mechanism. *)

module F(Targ : Target.T) = struct
open Targ

let debug = false

(* |dagnode| -- node in DAG representation of an expression *)
type dagnode =
  { g_serial: int;			(* Serial number *)
    g_op: inst;				(* Operator *)
    mutable g_rands: dagnode list;	(* Operands *)
    mutable g_refct: int;		(* Reference count *)
    mutable g_temp: int; 		(* Temp, or -1 if none *)
    mutable g_inspected: bool }
    
(* |serial| -- fetch serial number of a node *)
let serial g = g.g_serial

(* |node_table| -- hash table for value numbering *)
let node_table = Hashtbl.create 129

(* |node_count| -- counter for numbering nodes *)
let node_count = ref 0

(* |newnode| -- create a new node *)
let newnode op rands = 
  incr node_count;
  List.iter (function g -> g.g_refct <- g.g_refct+1) rands;
  { g_serial = !node_count; g_op = op; g_rands = rands; 
    g_refct = 0; g_temp = -1; g_inspected = false }

(* |node| -- create a new node or share an existing one *)
let node op rands =
  let key = (op, List.map serial rands) in
  try Hashtbl.find node_table key with 
    Not_found -> 
      let n = newnode op rands in
      Hashtbl.add node_table key n; 
      n

(* |reset| -- clear the value numbering table *)
let reset () = 
  Hashtbl.clear node_table

(* Possible arena values *)
let a_regvar = 0x1
let a_global  = 0x2
let a_local = 0x4
let a_memory = a_global + a_local

let rec arena g =
  match g with
      G<LOCAL _> -> a_local
    | G<GLOBAL _> -> a_global
    | G<REGVAR _> -> a_regvar
    | G<OFFSET, base, _> -> arena base
    | _ -> a_memory

(* |disjoint| -- test if two arenas are free of overlap *)
let disjoint a b = ((a land b) = 0)

(* |alias| -- test if address g1 could be an alias for g2 *)
let alias g1 g2 =
  let simple =
    function LOCAL _ | GLOBAL _ | REGVAR _ -> true | _ -> false in

  if simple g1.g_op && simple g2.g_op then 
    (* Simple addresses that alias only if they are equal *)
    g1.g_op = g2.g_op 
  else
    (* Other addresses can alias only if their arenas intersect *)
    not (disjoint (arena g1) (arena g2))

(* |kill| -- remove LOAD nodes that satisfy a test *)
let kill p = 
  let deleted = Stack.create () in
  let f key g =
    match g with
	G<(LOADC|LOADW|LOADQ), a> -> 
	  if p a then Stack.push key deleted
      | _ -> () in
  Hashtbl.iter f node_table;
  Stack.iter (Hashtbl.remove node_table) deleted

let is_regvar = function <REGVAR _> -> true | _ -> false

(* |make_dag| -- convert an expression into a DAG *)
let rec make_dag t =
  match t with
      <STOREW, t1, t2> when not (is_regvar t2) -> 
        make_store STOREW LOADW t1 t2
    | <STOREC, t1, t2> when not (is_regvar t2) ->
        make_store STOREC LOADC t1 t2
    | <STOREQ, t1, t2> when not (is_regvar t2) ->
        make_store STOREQ LOADQ t1 t2
    | <LABEL lab> -> 
	reset (); node (LABEL lab) []
    | <(CALL n | CALLW n | CALLQ n) as op, @ts> -> 
        (* Never share procedure calls *)
        let gs = List.map make_dag ts in
        if Metrics.sharing > 1 then
          kill (fun g -> true)
        else
          reset ();
        newnode op gs
    | <(ARG _|STATLINK) as op, t> ->
        newnode op [make_dag t]
    | <JUMPC (w, lab) as op, @ts> ->
        let gs = List.map make_dag ts in
        if Metrics.sharing = 0 then reset ();
        newnode op gs
    | <w, @ts> ->
	node w (List.map make_dag ts)

and make_store st ld t1 t2 =
  let g1 = make_dag t1 in
  let g2 = make_dag t2 in
  (* Kill all nodes that might alias the target location *)
  kill (alias g2); 
  (* Add dummy argument to detect use of stored value *)
  if is_regvar t2 then
    node st [g1; g2]
  else begin
    let g3 = node ld [g2] in
    g2.g_refct <- g2.g_refct-1;	(* Ignore artificial ref from g3 *)
    node st [g1; g2; g3]
  end
  
let op_size =
  function
      LOADC ->
        Metrics.char_rep.r_size
    | CONST _ | LOADW | CALLW _ | MONOP _ | BINOP _ | BOUND | SYMBOL _ ->
        Metrics.int_rep.r_size
    | GLOBAL _ | LOCAL _ | NIL | LOADQ | CALLQ _ | OFFSET | NCHECK ->
        Metrics.addr_rep.r_size
    | i -> failwith (sprintf "op_size $" [fInst i])

let make_temp g =
    Alloc.inc_temp g.g_temp;
    if op_size g.g_op <= 4 then <TEMPW g.g_temp> else <TEMPQ g.g_temp>

(* |visit| -- convert dag to tree, sharing the root if worthwhile *)
let rec visit g root =
  match g.g_op with
      REGVAR _ | CONST _ | NIL -> 
        build g (* Trivial *)
    | GLOBAL _  when not Metrics.share_globals ->
        build g
    | CALLW _ | CALLQ _ ->
        share g
    | _ ->
	if root || g.g_refct = 1 then build g else share g

(* |build| -- convert dag to tree with no sharing at the root *)
and build g =
  match g with
      G<(CALL _ | CALLW _ | CALLQ _) as op, @(p::args)> ->
	(* Don't share constant procedure addresses *)
	let p' = 
	  match p.g_op with
              GLOBAL _ | LIBFUN _ -> build p
            | _ -> visit p false in
        let args' = List.map (fun g1 -> visit g1 true) args in
	<op, @(p'::args')>
    | G<(STOREC|STOREW|STOREQ) as op, g1, g2, g3> ->
        (* If dummy value is used, then make it share with g1 *)
	let t1 = 
          if g3.g_refct > 1 then share g1 else visit g1 false in
	g3.g_temp <- g1.g_temp;
	<op, t1, visit g2 false>
    | G<op, @rands> -> 
	<op, @(List.map (fun g1 -> visit g1 false) rands)>

(* |share| -- convert dag to tree, sharing the root *)
and share g =
  if g.g_temp >= 0 then
    make_temp g
  else begin
    let d' = build g in
    match d' with
        (* No point in sharing register variables *)
        <(LOADC|LOADW|LOADQ), <REGVAR _>> -> d'
      | _ ->
          let n = Alloc.new_temp 0 in 
          g.g_temp <- n;
          <AFTER, <DEFTEMP n, d'>, make_temp g>
  end

(* unshare -- duplicate a shared node that should be recomputed *)
let rec unshare g =
  g.g_refct <- g.g_refct-1;
  let g1 = newnode g.g_op g.g_rands in
  g1.g_refct <- 1; inspect g1; g1

(* inspect -- find shared nodes that can be recomputed at no cost *)
and inspect g =
  if not g.g_inspected then begin
    g.g_inspected <- true;
    match g with
        G<(LOADC|LOADW), G<LOCAL _> as g1> ->
          if g1.g_refct > 1 then
            g.g_rands <- [unshare g1]
      | G<(LOADC|LOADW), G<OFFSET, _, _> as g1> ->
          inspect g1;
          if Metrics.addrmode > 0 && g1.g_refct > 1 then
            g.g_rands <- [unshare g1]
      | G<(STOREC|STOREW), g1, G<LOCAL _> as g2, g3> ->
          inspect g1;
          if g2.g_refct > 1 then
            g.g_rands <- [g1; unshare g2; g3]
      | G<(STOREC|STOREW), g1, G<OFFSET, _, _> as g2, g3> ->
          inspect g1; inspect g2;
          if Metrics.addrmode > 0 && g2.g_refct > 1 then
            g.g_rands <- [g1; unshare g2; g3]
      | G<OFFSET, g1, G<BINOP Lsl, _, G<CONST _>> as g2> ->
          inspect g1; inspect g2;
          if Metrics.addrmode > 1 && g2.g_refct > 1 then
            g.g_rands <- [g1; unshare g2]
      | G<op, @rands> ->
          List.iter inspect rands
  end
  
let traverse ts = 
  reset (); 
  (* Convert the trees to a list of roots in a DAG *)
  let gs = List.map make_dag ts in
  (* Reverse excessive sharing *)
  List.iter inspect gs;
  (* Then convert the DAG roots back into trees *)
  canon <SEQ, @(List.map (fun g -> visit g true) gs)>

end
