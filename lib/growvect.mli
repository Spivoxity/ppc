(* growvect.mli *)
(* Copyright (c) 2017 J. M. Spivey *)

(* This module provides an alternative implementation of arrays that
grow as needed to accommodate the indices used in assignments. *)

type 'a t

(* create -- make an empty vector with a specified initial capacity *)
val create : int -> 'a t

(* make -- make a vector of specified size *)
val make : int -> 'a -> 'a t

(* clear -- make vector empty again *)
val clear : 'a t -> unit

(* get -- fetch element at given index *)
val get : 'a t -> int -> 'a

(* size -- get current number of elements *)
val size : 'a t -> int

(* set -- change existing element at given index *)  
val set : 'a t -> int -> 'a -> unit

(* append -- add new element at the end *)
val append : 'a t -> 'a -> unit

(* to_list -- convert to list *)
val to_list : 'a t -> 'a list

(* iter -- apply function to each element *)
val iter : ('a -> unit) -> 'a t -> unit
