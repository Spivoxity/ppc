/* ncparse.mly */
/* Copyright (c) 2017 J. M. Spivey */

%token <string>		WORD OPEN
%token <char>		CHAR
%token			LPAREN RPAREN CLOSE EOF COMMA SPACE ATSIGN

%type <unit> 		text
%start 			text

%nonassoc		LOW
%nonassoc		COMMA SPACE ATSIGN
%nonassoc		HIGH

%{
open Print

let outch ch = print_char ch

let out s = print_string s

let out_frag f = printf "$" [f]

let fJoin f1 f2 = fExt (fun prf -> prf "$$" [f1; f2])
%}

%%

text :
    prog EOF			{ () } ;

prog :
    /* empty */			{ () }
  | prog WORD			{ out $2 }
  | prog SPACE			{ out " " }
  | prog CLOSE			{ out ">" }
  | prog LPAREN			{ out "(" }
  | prog RPAREN			{ out ")" }
  | prog COMMA			{ out "," }
  | prog ATSIGN			{ out "@" }
  | prog CHAR			{ outch $2 }
  | prog node			{ out_frag $2 } ;

node :
    OPEN bal1 args CLOSE	{ fMeta "(Node ($$, $))" [fStr $1; $2; $3] }

bal1 :
    bal %prec HIGH		{ $1 } ;

args :
    /* empty */			{ fStr "[]" }
  | COMMA blank arglist		{ fMeta "[$]" [$3] }
  | COMMA blank ATSIGN bal	{ $4 } ;

arglist :
    bal %prec HIGH		{ $1 }
  | arglist COMMA bal %prec HIGH  { fMeta "$;$" [$1; $3] } ;

bal :
    /* empty */ %prec LOW	{ fStr "" }
  | bal item			{ fJoin $1 $2 }
  | bal LPAREN bal RPAREN	{ fMeta "$($)" [$1; $3] } ;

item :
    WORD			{ fStr $1 }
  | CHAR			{ fChr $1 }
  | SPACE			{ fStr " " }
  | COMMA			{ fStr "," }
  | ATSIGN			{ fStr "@" }
  | node			{ $1 } ;

blank :
    /* empty */ %prec LOW	{ () }
  | SPACE			{ () } ;
