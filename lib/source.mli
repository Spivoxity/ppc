(* lib/source.mli *)
(* Copyright (c) 2017 J. M. Spivey *)

val note_line : int -> Lexing.lexbuf -> unit

val get_line : int -> string

val init : string -> in_channel -> unit

val err_message : string -> Print.arg list -> int -> unit
