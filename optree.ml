(* ppcu/optree.ml *)
(* Copyright (c) 2017--18 J. M. Spivey *)

open Print


let nsyms = ref 0

type symbol =
  { a_id: int;
    a_name: string;
    mutable a_val: int }

let symbol x =
  incr nsyms;
  { a_id = !nsyms; a_name = x; a_val = 0 }

let relative x n =
  incr nsyms;
  { a_id = !nsyms; a_name = x; a_val = n }

let fSym x = fStr x.a_name

let nosym =
  { a_id = 0; a_name = ""; a_val = 0 }

let gensym () =
  incr nsyms;
  { a_id = !nsyms; a_name = sprintf "g$" [fNum !nsyms]; a_val = 0 }

let is_none x = (x.a_id = 0)


type codelab = int

let nolab = -1

(* |lab| -- last used code label *)
let lab = ref 0

(* |label| -- allocate a code label *)
let label () = incr lab; !lab

(* |fLab| -- format a code label for printf *)
let fLab n = fMeta "L$" [fNum n]

(* |op| -- type of picoPascal operators *)
type op = Plus | Minus | Times | Div | Mod | Eq 
  | Uminus | Lt | Gt | Leq | Geq | Neq | EqA | NeqA
  | And | Or | Not | Lsl | Lsr | Asr | BitAnd | BitOr | BitNot

(* |inst| -- type of intermediate instructions *)
type inst =
    CONST of int32 		(* Constant (value) *)
  | SYMBOL of symbol * int      (* Symbolic constant *)
  | GLOBAL of symbol 		(* Global address (symbol) *)
  | LIBFUN of string		(* Library function *)
  | LOCAL of symbol * int	(* Local address (symbol, offset) *)
  | REGVAR of int		(* Register (index) *)
  | NIL				(* Null pointer *)
  | LOADC			(* Load char *)
  | LOADW			(* Load word *)
  | LOADQ			(* Load longword *)
  | STOREC			(* Store char *)
  | STOREW			(* Store word *)
  | STOREQ			(* Store longword *)
  | ARG of int			(* Pass argument (index) *)
  | STATLINK			(* Pass static link *)
  | CALL of int    		(* Call procedure (nparams) *)
  | CALLW of int    		(* Call procedure (nparams) *)
  | CALLQ of int    		(* Call procedure (nparams) *)
  | RESULTW			(* Procedure result *)
  | RESULTQ
  | MONOP of op			(* Perform unary operation (op) *)
  | BINOP of op			(* Perform binary operation (op) *)
  | OFFSET			(* Add address and offset *)
  | BOUND		  	(* Array bound check *)
  | NCHECK			(* Null pointer check *)
  | LABEL of codelab		(* Set code label *)
  | JUMP of codelab		(* Unconditional branch (dest) *)
  | JUMPC of op * codelab	(* Conditional branch (cond, dest) *)
  | JCASE of codelab list * codelab (* Jump table *)

  (* Extra instructions *)
  | LINE of int			(* Line number *)
  | NOP
  | SEQ
  | AFTER			(* Expression with side effect *)
  | DEFTEMP of int		(* Define temp *)
  | TEMPW of int		(* Temporary *)
  | TEMPQ of int		(* Temporary *)

let op_name =
  function
      Plus -> "PLUS" | Minus -> "MINUS" | Times -> "TIMES"
    | Div -> "DIV" | Mod -> "MOD" | Eq -> "EQ"
    | Uminus -> "UMINUS" | Lt -> "LT" | Gt -> "GT" 
    | Leq -> "LEQ" | Geq -> "GEQ" | Neq -> "NEQ" 
    | And -> "AND" | Or -> "OR" | Not -> "NOT"
    | Lsl -> "LSL" | Lsr -> "LSR" | Asr -> "ASR" 
    | BitAnd -> "BITAND" | BitOr -> "BITOR" | BitNot -> "BITNOT"
    | EqA -> "EQA" | NeqA -> "NEQA"

let fOp w = fStr (op_name w)

let fType1 =
  function 0 -> fStr "" | 1 -> fStr "W" | s -> fMeta "*$*" [fNum s]

let fOff (x, n) =
  if is_none x then fNum n
  else if n = 0 then fSym x
  else fMeta "$+$" [fSym x; fNum n]

let fInst =
  function
      CONST x ->	fMeta "CONST $" [fNum32 x]
    | SYMBOL (x, n) ->  fMeta "SYMBOL $" [fOff (x, n)]
    | GLOBAL a -> 	fMeta "GLOBAL $" [fSym a]
    | LIBFUN x ->	fMeta "LIBFUN $" [fStr x]
    | LOCAL (x, n) ->	fMeta "LOCAL $" [fOff (x, n)]
    | REGVAR i ->	fMeta "REGVAR $" [fNum i]
    | NIL ->            fStr "NIL"
    | LOADC -> 	        fStr "LOADC"
    | LOADW -> 	        fStr "LOADW"
    | LOADQ ->          fStr "LOADQ"
    | STOREC ->     	fStr "STOREC"
    | STOREW ->		fStr "STOREW"
    | STOREQ ->         fStr "STOREQ"
    | ARG n ->          fMeta "ARG $" [fNum n]
    | STATLINK ->       fStr "STATLINK"
    | CALL n ->		fMeta "CALL $" [fNum n]
    | CALLW n ->	fMeta "CALLW $" [fNum n]
    | CALLQ n ->	fMeta "CALLQ $" [fNum n]
    | RESULTW ->        fStr "RESULTW"
    | RESULTQ ->        fStr "RESULTQ"
    | MONOP w ->  	fStr (op_name w)
    | BINOP w ->  	fStr (op_name w)
    | OFFSET ->		fStr "OFFSET"
    | BOUND ->		fStr "BOUND"
    | NCHECK ->		fStr "NCHECK"
    | LABEL l ->	fMeta "LABEL $" [fLab l]
    | JUMP l ->		fMeta "JUMP $" [fLab l]
    | JUMPC (w, l) ->   fMeta "J$ $" [fStr (op_name w); fLab l]
    | JCASE (labs, def) -> fMeta "JCASE $ $" [fNum (List.length labs); fLab def]
    | LINE n ->		fMeta "LINE $" [fNum n]
    | NOP ->		fStr "NOP"
    | SEQ ->		fStr "SEQ"
    | AFTER ->		fStr "AFTER"
    | DEFTEMP n ->	fMeta "DEFTEMP $" [fNum n]
    | TEMPW n ->	fMeta "TEMPW $" [fNum n]
    | TEMPQ n ->	fMeta "TEMPQ $" [fNum n]

let int32_of_bool b = if b then Int32.one else Int32.zero

(* |do_monop| -- evaluate unary operators *)
let do_monop w x =
  match w with
      Uminus -> Int32.neg x
    | Not -> if x <> Int32.zero then Int32.zero else Int32.one
    | BitNot -> Int32.lognot x
    | _ -> failwith "do_monop"

(* |do_binop| -- evaluate binary operators *)
let do_binop w x y =
  match w with
      Plus -> Int32.add x y
    | Minus -> Int32.sub x y
    | Times -> Int32.mul x y
    | Div -> Int32.div x y
    | Mod -> Int32.rem x y
    | Eq -> int32_of_bool (x = y)
    | Lt -> int32_of_bool (x < y)
    | Gt -> int32_of_bool (x > y)
    | Leq -> int32_of_bool (x <= y)
    | Geq -> int32_of_bool (x >= y)
    | Neq -> int32_of_bool (x <> y)
    | And -> if x <> Int32.zero then y else Int32.zero
    | Or -> if x <> Int32.zero then Int32.one else y
    | BitAnd -> Int32.logand x y
    | BitOr -> Int32.logor x y
    | Lsl -> Int32.shift_left x (Int32.to_int y)
    | Lsr -> Int32.shift_right_logical x (Int32.to_int y)
    | Asr -> Int32.shift_right x (Int32.to_int y)
    | _ -> failwith "do_binop"

(* |negate| -- negation of a comparison *)
let negate = 
  function Eq -> Neq | Neq -> Eq | Lt  -> Geq
    | Leq -> Gt | Gt  -> Leq | Geq -> Lt
    | EqA -> NeqA | NeqA -> EqA
    | _ -> failwith "negate"


(* Operator trees *)

type optree = Node of inst * optree list

let rec canon_app t us =
  match t with
      <SEQ, @ts> -> List.fold_right canon_app ts us
    | <NOP> -> us
    | <LINE n> -> if n = 0 then us else <LINE n> :: set_line n us
    | _ -> effects t (result t :: us)

and set_line n ts =
  match ts with 
      [] -> []
    | <LINE m> :: us -> if n <> m then ts else us
    | u :: us -> u :: set_line n us

and effects t us =
  match t with
      <AFTER, t1, t2> -> canon_app t1 (effects t2 us)
    | <w, @ts> -> List.fold_right effects ts us

and result =
  function
      <AFTER, t1, t2> -> result t2
    | <w, @ts> -> <w, @(List.map result ts)>

let canon t = canon_app t []

let flat =
  function
      <CALL n, @(fn::args)> -> 
        List.rev args @ [<CALL n, fn>]
    | <DEFTEMP k, <CALLW n, @(fn::args)>> ->
        List.rev args @ [<DEFTEMP k, <CALLW n, fn>>]
    | <DEFTEMP k, <CALLQ n, @(fn::args)>> ->
        List.rev args @ [<DEFTEMP k, <CALLQ n, fn>>]
    | t -> [t]

let flatten ts = List.concat (List.map flat ts)

let rec fix_rel =
  function
      <SYMBOL (x, n)> -> <CONST (Int32.of_int (x.a_val + n))>
    | <LOCAL (x, n)> -> <LOCAL (nosym, x.a_val + n)>
    | <w, @ts> -> <w, @(List.map fix_rel ts)>

let fix_relative ts = List.map fix_rel ts


let fSeq(f) xs = 
  let g prf = List.iter (fun x -> prf "$" [f x]) xs in fExt g

let rec fTree <x, @ts> = 
  let op = sprintf "$" [fInst x] in
  fMeta "<$$>" [fStr op; fSeq(fun t -> fMeta ", $" [fTree t]) ts]

let print_optree pfx t =
  match t with
      <LINE n> ->
	Print.printf "$$\n" [fStr pfx; fStr (Source.get_line n)]
    | _ ->
        fgrindf stdout pfx "$" [fTree t];
