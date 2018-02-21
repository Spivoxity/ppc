(* r86lex.mll *)

{
open R86parse
open Lexing

let lnum = ref 1
}

rule token = parse
    ['A'-'Z''a'-'z''_''$''.']['A'-'Z''a'-'z''0'-'9''_''$''.']* as s
				{ IDENT s }
  | ['0'-'9']+ as s		{ INTEGER s }
  | '%'['a'-'z''0'-'9']+ as s	{ REG s }
  | '('				{ LPAR }
  | ')'				{ RPAR }
  | ','				{ COMMA }
  | ':'				{ COLON }
  | '='				{ EQUAL }
  | '+'				{ PLUS }
  | '-'				{ MINUS }
  | '*'				{ TIMES }
  | [' ''\t']+			{ token lexbuf }
  | '!'[^'\n']*			{ token lexbuf }
  | '\n'			{ incr lnum; NL }
  | _				{ BADTOK }
  | eof				{ EOF }
