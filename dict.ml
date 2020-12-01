(* ppcu/dict.ml *)
(* Copyright (c) 2017--18 J. M. Spivey *)

open Print
open Optree
open Target

(* Identifiers are represented by integers, with a hash table that maps
   strings to the corresponding integer, and a vector that maps the
   other way. *)

type ident = int

let nids = ref 0
let idhash = Hashtbl.create 100
let idvec = Growvect.create 100

let intern s =
  try Hashtbl.find idhash s with
    Not_found ->
      let x = !nids in
      incr nids;
      Hashtbl.add idhash s x;
      Growvect.append idvec s;
      x

let spelling x = Growvect.get idvec x

let fId x = fStr (spelling x)

(* |location| -- runtime locations *)
type location =
    Symbol of Optree.symbol	(* Global or local (label) *)
  | Absolute of int32		(* Hardware register (address) *)
  | Register of int		(* Register *)
  | Nowhere			(* Compile-time only *)

(* |libid| -- type of picoPascal library procedures *)
type libid = ChrFun | OrdFun | PrintNum | PrintChar | PrintString 
  | NewLine | ReadChar | ExitProc | NewProc | ArgcFun | ArgvProc
  | OpenIn | CloseIn | Operator of Optree.op

(* |lib_name| -- name of a library procedure *)
let lib_name x = 
  match x with
      PrintNum -> "print_num" | PrintChar -> "print_char" 
    | PrintString -> "print_string" | NewLine -> "newline"
    | ReadChar -> "read_char" | ChrFun -> "chr" | OrdFun -> "ord"
    | ExitProc -> "exit" | NewProc -> "new"
    | ArgcFun -> "argc" | ArgvProc -> "argv"
    | OpenIn -> "open_in" | CloseIn -> "close_in"
    | Operator op -> sprintf "$" [Optree.fOp op]

let fLibId l = fStr (lib_name l)


(*
Environments are represented using O'Caml's library module that
implements mappings using balanced binary trees.

The top block is also kept separately as a list, to check for multiple
declarations, and so that it can be returned by the top_block function.
This is used for formal parameter lists and lists of fields in
record types.  The list is kept in reverse order internally, so that
an element can be added in constant time.
*)

(* |def_kind| -- kinds of definition *)
type def_kind = 
    ConstDef of int32 		(* Constant (value) *)
  | StringDef 			(* String *)
  | TypeDef 			(* Type *)
  | VarDef			(* Variable *)
  | CParamDef 			(* Value parameter *)
  | VParamDef 			(* Var parameter *)
  | FieldDef 			(* Field of record *)
  | ProcDef			(* Procedure *)
  | PParamDef   		(* Proc parameter *)
  | LibDef of libproc		(* Lib proc (data) *)
  | HoleDef of ptype ref	(* Pending type *)
  | DummyDef			(* Dummy *)

(* |def| -- definitions in environment *)
and def = 
  { d_tag: ident; 		(* Name *)
    d_kind: def_kind; 		(* Kind of object *)
    d_type: ptype;		(* Type *)
    d_level: int;		(* Static level *)
    mutable d_mem: bool;	(* Whether addressible *)
    mutable d_addr: location }	(* Run-time location *)

and basic_type =
  VoidType | IntType | CharType | BoolType | AddrType | NoType

(* |ptype| -- picoPascal types *)
and ptype = 
  { t_id: int;			(* Unique identifier *)
    t_guts: type_guts; 		(* Shape of the type *)
    t_rep: metrics }

and type_guts =
    BasicType of basic_type
  | ArrayType of int * ptype
  | RecordType of def list
  | ProcType of proc_data
  | PointerType of ptype ref

(* |proc_data| -- data about a procedure type *)
and proc_data =
  { p_fparams: def list;
    p_pcount: int;
    p_result: ptype }

(* |libproc| -- data about a library procedure *)
and libproc =
  { q_id: libid;
    q_nargs: int;
    q_argtypes: ptype list }

module IdMap = 
  Map.Make(struct 
    type t = ident  
    let compare = compare 
  end)

type environment = Env of (def list * def IdMap.t)

let add_def d m = IdMap.add d.d_tag d m

let add_block b (Env (b0, m)) =
  Env (List.rev b, Util.accum add_def b m)

let top_block (Env (b, m)) = List.rev b

let new_block (Env (b0, m)) = Env ([], m)

let find_def x ds =
  let rec search =
    function
	[] -> raise Not_found
      | d::ds -> 
	  if x = d.d_tag then d else search ds in
  search ds

let can f x = try f x; true with Not_found -> false

let define d (Env (b, m)) = 
  if can (find_def d.d_tag) b then raise Exit;
  Env (d::b, add_def d m)

let replace d (Env (b, m)) =
  let rec repl =
    function
	[] -> failwith "replace"
      | d'::ds -> 
	  if d.d_tag = d'.d_tag then d::ds else d' :: repl ds in
  Env (repl b, add_def d m)

let lookup x (Env (b, m)) = IdMap.find x m

let empty = Env ([], IdMap.empty)

let n_types = ref 0

let mk_type t r =
  incr n_types;
  { t_id = !n_types; t_guts = t; t_rep = r }

let row n t =
  let r = t.t_rep in 
  mk_type (ArrayType (n, t)) { r_size = n * r.r_size; r_align = r.r_align }

let discrete t =
  match t.t_guts with
      BasicType (IntType | CharType | BoolType) -> true
    | _ -> false

let scalar t =
  match t.t_guts with
      BasicType (IntType | CharType | BoolType) -> true
    | PointerType _ -> true
    | _ -> false

let is_pointer t =
  match t.t_guts with
      PointerType t1 -> true
    | _ -> false

let bound t =
  match t.t_guts with
      ArrayType (n, t1) -> n
    | _ -> failwith "bound"

let base_type t =
  match t.t_guts with
      PointerType t1 -> !t1
    | ArrayType (n, t1) -> t1
    | _ -> failwith "base_type"

let get_proc t =
  match t.t_guts with
      ProcType p -> p
    | _ -> failwith "get_proc"

let rec same_type t1 t2 = 
  match (t1.t_guts, t2.t_guts) with
      (ProcType p1, ProcType p2) ->
	match_args p1.p_fparams p2.p_fparams 
	&& same_type p1.p_result p2.p_result
    | (ArrayType (n1, u1), ArrayType(n2, u2)) ->
	n1 = n2 && same_type u1 u2
    | (PointerType _, BasicType x) -> x = AddrType
    | (BasicType x, PointerType _) -> x = AddrType
    | (_, _) -> t1.t_id = t2.t_id

and match_args fp1 fp2 = 
  match (fp1, fp2) with
      ([], []) -> true
    | (f1::fp1', f2::fp2') ->
	f1.d_kind = f2.d_kind && same_type f1.d_type f2.d_type
	  && match_args fp1' fp2'
    | _ -> false

let is_string t =
  match t.t_guts with
      ArrayType (n, t1) ->
        (match t1.t_guts with BasicType CharType -> true
          | _ -> false)
    | _ -> false

let symbol_of d =
  match d.d_addr with
      Symbol x -> x
    | _ -> failwith "symbol_of"
    
let notype = mk_type (BasicType NoType) { r_size = 0; r_align = 0 }

module type Platform = sig
end

module type TypesT = sig
 module Metrics : MetricsT
 val integer : ptype
 val character : ptype
 val boolean : ptype
 val voidtype : ptype
 val addrtype : ptype
end

module TypesF(M : MetricsT) : TypesT = struct
  module Metrics = M
  let voidtype =  mk_type (BasicType VoidType) M.void_rep
  let integer =   mk_type (BasicType IntType) M.int_rep
  let character = mk_type (BasicType CharType) M.char_rep
  let boolean =   mk_type (BasicType BoolType) M.bool_rep
  let addrtype =  mk_type (BasicType AddrType) M.addr_rep
end

