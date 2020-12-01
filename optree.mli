(* ppc/optree.mli *)
(* Copyright (c) 2017--18 J. M. Spivey *)

(* |op| -- type of picoPascal operators *)
type op = Plus | Minus | Times | Div | Mod | Eq 
  | Uminus | Lt | Gt | Leq | Geq | Neq | EqA | NeqA
  | And | Or | Not | Lsl | Lsr | Asr | BitAnd | BitOr | BitNot

val fOp : op -> Print.arg


type symbol =
  { a_id: int;
    a_name: string;
    mutable a_val: int }

val symbol: string -> symbol
val relative: string -> int -> symbol

val fSym: symbol -> Print.arg

val gensym: unit -> symbol

val nosym: symbol

val is_none : symbol -> bool



(* |codelab| -- type of code labels *)
type codelab

val nolab : codelab

(* |label| -- generate a code label *)
val label : unit -> codelab

val fLab : codelab -> Print.arg

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
  | RESULTQ			(* Procedure result *)
  | MONOP of op			(* Perform unary operation (op) *)
  | BINOP of op			(* Perform binary operation (op) *)
  | OFFSET			(* Add address and offset *)
  | BOUND 		  	(* Array bound check *)
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

(* |Inst| -- printf format for instructions *)
val fInst : inst -> Print.arg

(* |do_monop| -- evaluate unary operation *)
val do_monop : op -> int32 -> int32

(* |do_binop| -- evaluate binary operation *)
val do_binop : op -> int32 -> int32 -> int32

(* |negate| -- find opposite for comparison op *)
val negate : op -> op

(* Operator trees *)

(* |optree| -- type of operator trees *)
type optree = Node of inst * optree list

(* |canon| -- eliminate SEQ, NOP, AFTER nodes *)
val canon : optree -> optree list

(* |flatten| -- move args before calls *)
val flatten : optree list -> optree list

val fix_relative : optree list -> optree list

val fTree : optree -> Print.arg

(* |print_optree| -- output operator tree on stdout with line breaking *)
val print_optree : string -> optree -> unit
