(* lib/print.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

type arg = vtable -> unit

and vtable = { outch : char -> unit; prf : string -> arg list -> unit }

(* |do_print| -- the guts of printf and friends *)
let rec do_print outch fmt args0 =
  let vtab = { outch = outch; prf = do_print outch } in
  let args = ref args0 in
  for i = 0 to String.length fmt - 1 do
    if fmt.[i] <> '$' then
      outch fmt.[i]
    else begin
      try 
	List.hd !args vtab;
	args := List.tl !args 
      with
        Invalid_argument _ -> 
	  outch '*'; outch '*'; outch '*'
    end
  done

let fChr ch vt = vt.outch ch
let fStr s vt = 
  for i = 0 to String.length s - 1 do vt.outch s.[i] done

let fNum n = fStr (string_of_int n)
let fFlo x = fStr (string_of_float x)
let fBool b = fStr (if b then "true" else "false")
let fExt g vt = g vt.prf

let fFix (n, w) =
  let digits = string_of_int n in
  let w0 = String.length digits in
  let padding = if w0 >= w then "" else String.make (w-w0) ' ' in
  fStr (padding ^ digits)

(* |fMeta| -- insert output of recursive call to printf *)
let fMeta fmt args = fExt (function prf -> prf fmt args)

(* |fList| -- format a comma-separated list *)
let fList cvt xs = 
  let f prf =
    if xs <> [] then begin
      prf "$" [cvt (List.hd xs)];
      List.iter (function y -> prf ", $" [cvt y]) (List.tl xs)
    end in
  fExt f

(* |fprintf| -- print to a file *)
let fprintf fp fmt args = do_print (output_char fp) fmt args

(* |printf| -- print on standard output *)
let printf fmt args = fprintf stdout fmt args; flush stdout

(* |sprintf| -- print to a string *)
let sprintf fmt args =
  let buf = Buffer.create 16 in
  do_print (Buffer.add_char buf) fmt args;
  Buffer.contents buf

open Format

let rec do_grind fmt args0 =
  let vtab = { outch = print_char; prf = do_grind } in
  let args = ref args0 in
  for i = 0 to String.length fmt - 1 do
    let ch = fmt.[i] in
    match ch with
	'$' ->
	  begin try 
	    List.hd !args vtab;
	    args := List.tl !args 
	  with
	    Invalid_argument _ -> print_string "***"
	  end
      | ' ' -> print_space ()
      | '_' -> print_char ' '
      | '(' | '<' | '[' -> open_hvbox 2; print_char ch
      | ')' | '>' | ']' -> print_char ch; close_box ()
      | ch -> print_char ch
  done

(* |fgrindf| -- pretty-printer *)
let rec fgrindf fp pfx fmt args =
  let plen = String.length pfx in
  set_formatter_out_channel fp;
  let funs = get_formatter_out_functions () in
  let newline1 () = funs.out_newline (); funs.out_string pfx 0 plen in
  set_formatter_out_functions { funs with out_newline = newline1 };
  funs.out_string pfx 0 plen; 
  open_hvbox 2; 
  do_grind fmt args; 
  close_box(); 
  print_flush ();
  set_formatter_out_functions funs;
  print_newline ()
