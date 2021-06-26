/* ncparse.mly */
/* Copyright (c) 2017 J. M. Spivey */

/* The lexer scans commas together with any following spaces to help
   with layout in the output, and returns the entire token as a
   string. */
   
%token <string>		OPEN GOPEN COMMA WORD
%token <char>		CHAR
%token			LPAREN RPAREN CLOSE ATSIGN EOF

%type <unit> 		text
%start 			text

%right ATSIGN

%{
open Print

let outch ch = print_char ch

let out s = print_string s

let out_frag f = printf "$" [f]

let fJoin f1 f2 = fMeta "$$" [f1; f2]
%}

%%

text :
    prog EOF			{ () } ;

prog :
    /* empty */			{ () }
  | prog CHAR			{ outch $2 }
  | prog WORD			{ out $2 }
  | prog LPAREN			{ out "(" }
  | prog RPAREN			{ out ")" }
  | prog COMMA			{ out $2 }
  | prog ATSIGN			{ out "@" }
  | prog CLOSE			{ out ">" }
  | prog node			{ out_frag $2 } ;

/* Fragments of output text are represented as arguments that can be
passed to our version of printf.  What's particularly helpful about
these 'recursive metaformats' (TM) is the function fMeta, which takes
the same arguments as printf and denotes the string that printf would
ouput given those arguments.  Thus printf "$" [fMeta fmt args] is
equivalent to printf fmt args, and the full power of printf is
available nested inside one argument to printf. */

node :
    OPEN expr args CLOSE
      { fMeta "(Node ($$, $))" [fStr $1; $2; $3] }
  | GOPEN expr args CLOSE
      { fMeta "{ g_op = $$; g_rands = $ }" [fStr $1; $2; $3] } ;

/* The grammar is ambiguous at this point, because while there is a
specific production for args that begin with an @ sign, the general
case also allows an @ sign as the first item in an expr that begins an
arglist.  We prefer the specific interpretation, and enforce it by
making ATSIGN right-associative and giving the same precedence to the
production expr -> EMPTY.  According to yacc's conventions, that makes
the parser shift the @ and head for the specific production.  That's
what yacc would have done anyway, but an explicit predence annotation
saves an error message. */

args :
    /* empty */			{ fStr "[]" }
  | COMMA arglist		{ fMeta "[$]" [$2] }
  | COMMA ATSIGN expr           { $3 } ;

arglist :
    expr			{ $1 }
  | arglist COMMA expr		{ fMeta "$; $" [$1; $3] } ;

/* We need to distinguish outer expressions, which occur in a context
where commas are significant, from inner expressions that are protected
by parentheses and can contain commas with no significance.  The
distinction could be made with further tricky precedence annotations,
but it's saner to do it explicitly in the grammar. */

expr :
    /* empty */ %prec ATSIGN	{ fStr "" }
  | expr item			{ fJoin $1 $2 }

inner :
    /* empty */			{ fStr "" }
  | inner item			{ fJoin $1 $2 }
  | inner COMMA			{ fJoin $1 (fStr $2) } ;

item :
    CHAR			{ fChr $1 }
  | WORD			{ fStr $1 }
  | ATSIGN			{ fStr "@" }
  | LPAREN inner RPAREN         { fMeta "($)" [$2] }
  | node			{ $1 } ;

