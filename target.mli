(* ppcu/target.mli *)
(* Copyright (c) 2017--18 J. M. Spivey *)

(* Types and module interfaces for linking in the back-ends. *)

(* |metrics| -- target representation of data object *)
type metrics = 
  { r_size: int; 		(* Size of object *)
    r_align: int }		(* Address must be multiple of this *)

(* Type used to represent registers *)
type reg = Reg of int | R_any | R_temp | R_suggest of int | R_none

(* Type metrics and other layout data *)
module type MetricsT = sig
  val int_rep : metrics		(* Integer type *)
  val char_rep : metrics	(* Char type *)
  val bool_rep : metrics	(* Boolean type *)
  val void_rep : metrics	(* Void type *)
  val addr_rep : metrics	(* All addresses *)
  val proc_rep : metrics	(* Closures *)
  val param_rep : metrics	(* Procedure parameters *)
  val max_align : int

  val param_base : int		(* worst case offset of first param from fp *)
  val local_base : int -> int	(* -ve offset of bottom of frame head *)
  val stat_link : int		(* Offset of static link *)
  val nregvars : int		(* Number of register variables *)
  val share_globals : bool      (* Whether to use CSE on <GLOBAL x> *)
  val sharing : int		(* Whether to use CSE across jumps or calls *)
  val fixed_frame : bool        (* Whether to fix relative addresses early *)

  val reg_names : string array
  val volatile : reg list
  val stable : reg list
end

(* Interface provided by the register allocator *)
module type AllocT = sig
  (* |init| -- reset for new procedure with regvar count *)
  val init : int -> unit

  (* |set_outgoing| -- note maximum outgoing parameters *)
  val set_outgoing : int -> unit

  val outgoing : unit -> int

  val is_wildcard : reg -> bool

  (* |is_free| -- test if register is free *)
  val is_free : reg -> bool

  val regvar : int -> reg

  (* |get_reg| -- use specified register or allocate one if R_any *)
  val get_reg : reg -> reg

  (* |reserve_reg| -- reserve a register *)
  val reserve_reg : reg -> unit

  (* |release_reg| -- decrement reference count of register *)
  val release_reg : reg -> unit

  (* |dump_regs| -- make one-line summary of register state *)
  val dump_regs : unit -> string

  (* |alloc_suggest| -- allocate suggested register, or any other *)
  val alloc_suggest : reg -> reg

  (* Temps *)

  (* |new_temp| -- allocate a temp with specified reference count *)
  val new_temp : int -> int

  (* |inc_temp| -- increment refcount of a temp variable *)
  val inc_temp : int -> unit

  (* |temp_reg| -- return register allocated for temp, or R_none *)
  val temp_reg : int -> reg

  (* |use_temp| -- use a temp variable *)
  val use_temp : int -> reg

  (* |def_temp| -- define a temp variable *)
  val def_temp : int -> reg -> unit

  (* |spill_temps| -- move any temps that use volatile registers to safety *)
  val spill_temps : (reg -> reg -> unit) -> reg list -> unit

  val fReg : reg -> Print.arg
end

(* Interface to be provided by the code output module *)
module type EmitterT = sig
  type operand

  val reg_of : operand -> reg

  val map_regs : (reg -> reg) -> operand -> operand

  val use_reg : reg -> unit

  val param_offset : unit -> int

  (* |preamble| -- emit first part of assembly language output *)
  val preamble : unit -> unit

  (* |postamble| -- emit last part of assembly language output *)
  val postamble : unit -> unit

  (* |put_string| -- emit assembler code for string constant *)
  val put_string : Optree.symbol -> string -> unit

  (* |put_global| -- emit assembler code to define global variable *)
  val put_global : Optree.symbol -> int -> unit

  val start_proc : Optree.symbol -> int -> int -> int -> unit
  val prelude : unit -> unit
  val postlude : unit -> unit

  val put_inst : string -> operand list -> unit
  
  (* |comment| -- Prefix for line-long comments *)
  val comment : string
end  

(* Interface provided by the code buffer, used by instruction selection *)
module type IQueueT = sig
  type operand

  val emit : string -> operand list -> unit

  val emit_lab : Optree.codelab -> unit

  val emit_comment : string -> unit
  
  val gen : string -> operand list -> unit

  val gen_reg : string -> operand list -> operand

  val gen_move : string -> operand list -> operand

  val fix_reg : operand -> operand

  val reserve : operand -> unit

  val release : operand -> unit

  val line : int ref

  (* Instructions are interspersed with labels and comments *)
  type item = 
      Instr of string * operand list 
    | Label of Optree.codelab
    | Comment of string
    | Tree of Optree.optree

  val set_rewriter : (item Queue.t -> item Queue.t) -> unit
end

(* Interface to the back-end instruction selector *)
module type SelectorT = sig
  val tran_stmt : Optree.optree -> unit
end

(* All the back-end interfaces packaged together *)
module type T = sig
  module Metrics : MetricsT

  module Alloc : AllocT

  module Emitter : EmitterT

  module Selector(IQueue : IQueueT
    with type operand = Emitter.operand) : SelectorT

  val options : (string * Arg.spec * string) list
end
