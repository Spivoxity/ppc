(* ncmain.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

(* The 'nodexp' program is a preprocessor that adds one new notation to
OCaml.  An expression like <LOAD addr_size, <LOCAL stat_link>> is shorthand
for Node (LOAD addr_size, [Node (LOCAL stat_link, [])]) -- a bit easier to
read, but a lot easier to type.  You can also write, e.g., 
<SEQ, @(List.map gen_stmt ss)> as shorthand for the expression
Node (SEQ, List.map gen_stmt ss), which is no longer, but at least gives 
a consistent look to the program.

A program that's written in this notation ought to contain the type
declaration

type optree = Node of inst * optree list

to make the expressions properly typed.  The <...> expressions can be 
used both as patterns and as proper expressions.  Ambiguity with the use of
< and > as comparison operators is removed by the rule that the construct
must begin <ident with no space. *)

open Print

let qflag = ref false

let spec = Arg.align ["-q", Arg.Unit (fun () -> qflag := true),
  " disable #line directives"]

let main () =
  let fname = ref "standard input" in
  let chan = ref stdin in
  let seen = ref false in
  let open_it fn =
    if not !seen then begin
      fname := fn; chan := open_in fn; seen := true
    end in
  Arg.parse spec open_it "Usage: nodexp [-q] file";
  let lexbuf = Lexing.from_channel !chan in
  begin 
    if not !qflag then printf "# 1 \"$\"\n" [fStr !fname];
    try Ncparse.text Nclex.token lexbuf with
      Parsing.Parse_error ->
	fprintf stderr "\"$\", line $: syntax error\n" 
	  [fStr !fname; fNum !Nclex.line_no];
	exit 1
  end;
  exit 0

let nodexp = main ()
