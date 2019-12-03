(* lib/print.mli *)
(* Copyright (c) 2017 J. M. Spivey *)

type arg

(* Basic formats *)
val fNum : int -> arg		(* Decimal number *)
val fNum32 : int32 -> arg       (* Decimal number, 32 bits *)
val fFix : int * int -> arg	(* Fixed-width number (val, width) *)
val fFlo : float -> arg		(* Floating-point number *)
val fStr : string -> arg	(* String *)
val fChr : char -> arg		(* Character *)
val fBool : bool -> arg		(* Boolean *)

(* |fMeta| -- insert output of recursive call to |printf| *)
val fMeta : string -> arg list -> arg

(* |fExt| -- higher-order extension *)
val fExt : ((string -> arg list -> unit) -> unit) -> arg

(* |fList| -- format a comma-separated list *)
val fList : ('a -> arg) -> 'a list -> arg

(* |printf| -- print on standard output *)
val printf : string -> arg list -> unit

(* |fprintf| -- print to a file *)
val fprintf : out_channel -> string -> arg list -> unit

(* |sprintf| -- print to a string *)
val sprintf : string -> arg list -> string

(* |fgrindf| -- pretty-printer *)
val fgrindf : out_channel -> string -> string -> arg list -> unit

val do_print : (char -> unit) -> string -> arg list -> unit
