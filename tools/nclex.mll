(* nclex.mll *)
(* Copyright (c) 2017 J. M. Spivey *)

{
open Ncparse 
open String 
open Lexing

let line_no = ref 1
}

rule token =
  parse
      "<"(['A'-'Z''a'-'z']+|'('[^')']+')' as s)  { OPEN s }
    | [^'<''>''('')'',''@'' ''\n']+ as s  { WORD s }
    | "(*"[^'\n']*"*)" as s	{ WORD s }
    | "->" as s			{ WORD s }
    | " "			{ SPACE }
    | ">"			{ CLOSE }
    | "("			{ LPAREN }
    | ")"			{ RPAREN }
    | ","			{ COMMA }
    | "@"			{ ATSIGN }
    | "\n"			{ incr line_no; CHAR '\n' }
    | _ as c			{ CHAR c }
    | eof			{ EOF }

