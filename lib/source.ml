(* lib/source.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

open Print

let filename = ref ""
let chan = ref stdin
let linetab = Hashtbl.create 100

let note_line n lexbuf =
  Hashtbl.add linetab n (Lexing.lexeme_end lexbuf)

let get_line n =
  let pos0 = pos_in !chan in
  let line =
    try seek_in !chan (Hashtbl.find linetab n); input_line !chan with
	Not_found -> ""
      | End_of_file -> "" in
  seek_in !chan pos0;
  line

let init fn ch =
  filename := fn; chan := ch;
  Hashtbl.add linetab 1 0

let err_message fmt args ln =
  fprintf stderr "\"$\", line $: $\n" 
    [fStr !filename; fNum ln; fMeta fmt args];
  flush stderr

