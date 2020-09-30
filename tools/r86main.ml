(* r86main.ml *)

(* Risc86 is an invented RISC machine that shares the registers of the
x86 but has a load/store instruction set with 3-address arithmetic
instructions and no restrictions on which registers can be used in
each instruction.  It's implemented by translation into real x86 code,
with the restrictions of the x86 removed by expanding difficult
instructions into multiple x86 instructions, and inserting a move
instruction where an operation's result is in a different register
from its inputs.  The cost of this could, in extreme cases, be high.
But provided the register allocator in the compiler allocates the
result of an operation when it can to the same register as an input,
and provided the other restriction affect only uncommon instructions
-- as tends to be true -- the results are generally good enough to be
usable. *)

open Print

let main () =
  let lexbuf = Lexing.from_channel stdin in
  begin
    try R86parse.program R86lex.token lexbuf with
      Parsing.Parse_error -> 
	fprintf stderr 
	  "risc86: syntax error on line $\n" [fNum !R86lex.lnum];
	exit 1
  end;
  exit 0

let risc86 = main ()
