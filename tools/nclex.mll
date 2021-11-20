(* nclex.mll *)
(* Copyright (c) 2017 J. M. Spivey *)

{
open Ncparse 
open String 
open Lexing

let line_no = ref 1

let note_lfs s =
  for i = 0 to String.length s - 1 do
    if s.[i] = '\n' then incr line_no
  done
}

rule token =
  parse
      "<"(['A'-'Z''a'-'z']+|'('[^')']+')' as s)  { OPEN s }
    | "#<"(['A'-'Z''a'-'z']+|'('[^')']+')' as s)  { GOPEN s }
    | [^'<''>''('')'',''@'' ''#''\n']+ as s  { WORD s }
    | "(*"[^'\n']*"*)" as s	{ WORD s }
    | "->" as s			{ WORD s }
    | ">"			{ CLOSE }
    | "("			{ LPAREN }
    | ")"			{ RPAREN }
    | ","[' ''\t''\n']* as s	{ note_lfs s; COMMA s }
    | "@"			{ ATSIGN }
    | "\n"			{ incr line_no; CHAR '\n' }
    | _ as c			{ CHAR c }
    | eof			{ EOF }

