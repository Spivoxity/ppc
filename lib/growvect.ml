(* lib/growvect.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

type 'a t = 
  { mutable size: int; 
    mutable elements: 'a array }

let create n = 
  { size = 0; elements = Array.make n (Obj.magic ()) }

let make n v =
  { size = n; elements = Array.make n v }

let clear v =
  v.size <- 0

let size v = v.size

let get v i = 
  if i >= v.size then raise (Invalid_argument "index out of bounds");
  Array.get v.elements i

let set v i x =
  if i >= v.size then raise (Invalid_argument "index out of bounds");
  Array.set v.elements i x

let append v x =
  let n = Array.length v.elements in
  if v.size >= n then begin
    let newv = Array.make (2*n) (Obj.magic ()) in
    Array.blit v.elements 0 newv 0 n;
    v.elements <- newv
  end;
  Array.set v.elements v.size x;
  v.size <- v.size+1

let iter f v =
  for i = 0 to v.size-1 do f v.elements.(i) done

let to_list v =
  let rec loop n xs =
    if n = 0 then xs else loop (n-1) (v.elements.(n-1)::xs) in
  loop v.size []
