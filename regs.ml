(* ppcu/regs.ml *)
(* Copyright (c) 2017--18 J. M. Spivey *)

open Print
open Target

module AllocF(Metrics : Target.MetricsT) = struct

open Metrics

(* |pool| -- list of allocatable registers *)
let pool = volatile @ stable

(* |temps| -- registers in a different order for allocating shared temps *)
let temps = stable @ volatile

(* |regmap| -- hash table giving refcount for each resister *)
let regmap = Util.make_hash 43 (List.map (fun r -> (r, ref 0)) pool)

(* |disabled| -- list of registers disabled from allocation unless suggested *)
let disabled = ref []

let enabled r = not (List.mem r !disabled)

let is_wildcard =
  function Reg i -> false | _ -> true

(* |is_free| -- test if register is free *)
let is_free r =
  try !(Hashtbl.find regmap r) = 0 with Not_found -> false

(* |refcount| -- apply function to refcount cell *)
let refcount f r =
  try f (Hashtbl.find regmap r) with Not_found -> ()

(* |reserve_reg| -- reserve a register *)
let reserve_reg r = refcount incr r

let cond_decr x =
  if !x = 0 then failwith "refcounts trashed";
  decr x

(* |release_reg| -- release a register *)
let release_reg r = refcount cond_decr r

(* |find_first| -- find first element of list passing a test *)
let rec find_first p =
  function
      [] -> raise Not_found
    | x::xs -> if p x then x else find_first p xs

let annd p q x = p x && q x

let find_reg set =
  try find_first (annd is_free enabled) set with
    Not_found -> failwith "Sorry, I ran out of registers"

(* |alloc| -- allocate register from specified set *)
let alloc set =
  let r = find_reg set in reserve_reg r; r

let alloc_suggest r = find_reg (r :: pool)

(* |get_reg| -- replace R_any or R_temp by specific register *)
let get_reg =
  function
      R_any -> find_reg pool
    | R_temp -> find_reg temps
    | R_suggest r -> find_reg (Reg r :: pool)
    | r -> r

(* |temp| -- data for temp variable *)
type temp =
  { t_id : int;				(* Name *)
    t_refct : int ref;			(* Number of references *)
    mutable t_reg : reg }		(* Allocated register *)

let ntemps = ref 0
let temptab = Hashtbl.create 131

(* |new_temp| -- create a temp variable *)
let new_temp c =
  incr ntemps; 
  let n = !ntemps in
  Hashtbl.add temptab n { t_id = n; t_refct = ref c; t_reg = R_none };
  n

(* |temp| -- get data for a temp variable *)
let temp n = Hashtbl.find temptab n

(* |inc_temp| -- increment refcount of a temp variable *)
let inc_temp n =
  let t = temp n in incr t.t_refct

(* |def_temp| -- specify register for a temp variable *)
let def_temp n r =
  let t = temp n in t.t_reg <- r

(* |temp_reg| -- return register allocated for temp, or R_none *)
let temp_reg n =
  let t = temp n in t.t_reg

(* |use_temp| -- use a temp variable *)
let use_temp n =
  let t = temp n in 
  decr t.t_refct; 
  if !(t.t_refct) > 0 then reserve_reg t.t_reg;
  t.t_reg

(* |spill_temps| -- move temp variables to callee-save registers *)
let spill_temps move rs =
  let spill n t =
    if !(t.t_refct) > 0 && t.t_reg <> R_none
        && List.mem t.t_reg rs then begin
      let r = find_reg stable in
      move r t.t_reg; t.t_reg <- r
    end in
  Hashtbl.iter spill temptab

let regvar i = List.nth stable i

let get_regvars nregv =
  disabled := Util.take nregv stable

let reset () = 
  ntemps := 0;
  Hashtbl.clear temptab;
  let zero x = (x := 0) in List.iter (refcount zero) pool;
  disabled := []

let outg = ref 0

let set_outgoing n =
  outg := n

let outgoing () = !outg

let fReg =
  function
      Reg i -> fStr reg_names.(i)
    | R_suggest i -> fMeta "*SUGG-$*" [fStr reg_names.(i)]
    | R_any -> fStr "*ANYREG*" 
    | R_temp -> fStr "*TEMPREG*"
    | R_none -> fStr "*NOREG*"

(* |dump_regs| -- dump register state *)
let dump_regs () =
  let dump prf =
    let begun = ref false in
    List.iter (fun r -> 
        let x = !(Hashtbl.find regmap r) in
        if x <> 0 then begin
          if not !begun then begin
            prf "regs" []; begun := true
          end;
          prf " $=$" [fReg r; fNum x]
        end) pool in
  sprintf "$" [fExt dump]

end

