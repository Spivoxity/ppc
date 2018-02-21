(* ppcu/main.ml *)
(* Copyright (c) 2017--18 J. M. Spivey *)

open Print
open Source

(* The main program of the compiler gathers together everything into
   one big functor, waiting to be applied to a target description. *)

let debug = Coder.debug
let boundchk = Tgen.boundchk
let optlevel = Coder.optlevel
let regvars = Check.regvars
let pic_mode = ref false

module F(Tgt : Target.T) = struct
  module Types = Dict.TypesF(Tgt.Metrics)
  module Check = Check.F(Types)
  module Tgen = Tgen.F(Tgt)
  module Emitter = Tgt.Emitter

  let usage = "Usage: ppc [options] file"

  let spec =
    Arg.align
      (["-b", Arg.Unit (fun () -> boundchk := true), 
          " enable bound checks";
        "-d", Arg.Int (function x -> debug := x), "n set debug level";
        "-O", Arg.Unit (fun () -> optlevel := 1),
          " enable optimiser (simplifier, jump opt)";
        "-O2", Arg.Unit (fun () -> optlevel := 2),
          " more optimisation (common subexpressions)";
        "-noregvars", Arg.Unit (fun () -> regvars := false),
          " disable register variables"] @ Tgt.options)

  let main () =
    let fns = ref [] in
    Arg.parse spec (function s -> fns := !fns @ [s]) usage;
    if List.length !fns <> 1 then begin
      fprintf stderr "$\n" [fStr usage]; exit 2
    end;
    let in_file = List.hd !fns in
    let in_chan = open_in in_file in
    let lexbuf = Lexing.from_channel in_chan in
    Source.init in_file in_chan;
    ignore (Parsing.set_trace (!debug > 2));

    (* Parse the program *)
    let prog = 
      try Parser.program Lexer.token lexbuf with
        Parsing.Parse_error ->
          let tok = Lexing.lexeme lexbuf in
          Source.err_message "syntax error at token '$'"
            [fStr tok] !Lexer.lineno;
          exit 1 in

    if !debug > 2 then begin
      printf "$Abstract syntax tree:\n" [fStr Emitter.comment];
      Tree.print_tree stdout Emitter.comment prog;
      printf "\n" []
    end;

    (* Semantic analysis *)
    begin try Check.annotate prog with
      Check.Sem_error (fmt, args, ln) ->
        Source.err_message fmt args ln;
        exit 1
    end;

    (* Translate the program *)
    Tgen.translate prog;
    exit 0

  let _ = main ()
end
