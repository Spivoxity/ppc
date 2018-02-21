/* r86parse.mly */

%token<string> IDENT REG INTEGER
%token PLUS MINUS TIMES LPAR RPAR COMMA COLON NL EQUAL BADTOK EOF

%type<unit> program
%start program

%{
open R86ops
open Print
%}

%%

program : 
    /* EMPTY */			{ () }
  | program NL			{ () }
  | program line NL		{ () } ;

line :
    IDENT COLON			{ emit_lab $1 }
  | IDENT			{ emit_instr $1 [] }
  | IDENT rands			{ emit_instr $1 $2 }
  | IDENT EQUAL rand		{ put "$ = $" [fStr $1; fRand $3] } ;

rands :
    rand			{ [$1] }
  | rand COMMA rands		{ $1::$3 } ;

rand :
    REG				{ Reg (find_reg $1) }
  | const			{ Const $1 }
  | LPAR REG RPAR		{ Indir (find_reg $2) } 
  | LPAR REG TIMES INTEGER RPAR { Index ("", find_reg $2, int_of_string $4) }
  | const LPAR index RPAR	{ let (r, s) = $3 in Index ($1, r, s) }
  | LPAR REG PLUS index RPAR    { let (r, s) = $4 in 
				  Index2 (find_reg $2, r, s) } ;

index :
    REG                         { (find_reg $1, 1) }
  | REG TIMES INTEGER		{ (find_reg $1, int_of_string $3) } ;

const :
    IDENT			{ $1 }
  | IDENT PLUS INTEGER		{ $1^"+"^$3 }
  | IDENT MINUS INTEGER		{ $1^"-"^$3 }
  | INTEGER			{ $1 } 
  | MINUS INTEGER		{ "-"^$2 } ;
