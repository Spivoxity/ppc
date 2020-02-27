(* ppcu/coder.ml *)
(* Copyright (c) 2017--18 J. M. Spivey *)

(* A template that drives the code generation process.  The machine-specific
   parts are an Emitter that formats the output, and a Selector that
   chooses instructions.  Each procedure body is buffered, so the Emitter
   can tailor that prelude and postlude in light of the code generated. *)

open Optree
open Print

let debug = ref 0
let optlevel = ref 0

let outgoing = ref 0

module F(Tgt : Target.T) = struct
  module Alloc = Tgt.Alloc
  module Emitter = Tgt.Emitter

  (* A queue of instructions for the body of the current procedure.
     This depends on the targets Emitter, and provides an interface
     for code selection that implicitly manages registers. *)
  module IQueue = struct
    type operand = Emitter.operand

    (* Instructions are interspersed with labels and comments *)
    type item = 
        Instr of string * Emitter.operand list 
      | Label of Optree.codelab
      | Comment of string
      | Tree of Optree.optree

  (* Output a queue item *)
    let put_item =
      function
          Instr (op, rands) -> Emitter.put_inst op rands
        | Label lab -> printf ".$:\n" [fLab lab]
        | Comment cmnt -> printf "$$\n" [fStr Emitter.comment; fStr cmnt] 
        | Tree t -> Optree.print_optree Emitter.comment t

    let code = Queue.create ()

    let emit_tree t = Queue.add (Tree t) code

    let emit_comment s = Queue.add (Comment s) code

    (* |line| -- current line number *)
    let line = ref 0

    let set_line n =
      if !line <> n then
        emit_comment (Source.get_line n);
      line := n

    let rewriter = ref (fun q -> q)

    let set_rewriter f =
      rewriter := f

    (* |end_proc| -- output procedure fragment, perhaps after error *)
    let end_proc () =
      let code' = !rewriter code in
      Emitter.prelude ();
      Queue.iter put_item code';
      Emitter.postlude();
      Queue.clear code

    (* |emit| -- emit an assembly language instruction *)
    let emit inst rands =
      Queue.add (Instr (inst, rands)) code

    (* |emit_lab| -- emit a label *)
    let emit_lab lab =
      Queue.add (Label lab) code

    (* Replace wildcards by an allocated register *)
    let fix_reg v =
      Emitter.map_regs (function r ->
        let r' = Alloc.get_reg r in Alloc.reserve_reg r'; r') v

    (* |release| -- release any register used by a value *)
    let release v =
      let f r = Alloc.release_reg r; r in
      ignore (Emitter.map_regs f v)

    (* |reserve| -- reserve any registers used by a value *)
    let reserve v =
      let f r = Alloc.reserve_reg r; r in
      ignore (Emitter.map_regs f v)

    (* |split_op| -- map "*mov/add" to the pair ("mov", "add") *)
    let split_op op =
      let n = String.length op in
      let k = String.index op '/' in
      (String.sub op 1 (k-1), String.sub op (k+1) (n-k-1))

    (* |gen_reg| -- emit instruction with result in a register *)
    let gen_reg op rands =
      if op.[0] <> '*' then begin
        List.iter release (List.tl rands);
        let v1 = fix_reg (List.hd rands) in
        Emitter.use_reg (Emitter.reg_of v1);
        emit op (v1 :: List.tl rands); v1
      end else begin
        (* Use a two-address instruction to implement a three-address
           operation (or a one-address instruction for a unary op).
           The opcode in op has the form "*mov/add", where "mov" is an
           instruction that can be used to move the first operand into
           the right place, and "add" combines it with the second operand. *)
        let (mov, op1) = split_op op in
        let v1 = List.nth rands 0 and v2 = List.nth rands 1 in
        let rest = List.tl (List.tl rands) in
        let r1 = Emitter.reg_of v1 and r2 = Emitter.reg_of v2 in
        (* Only release the first input now.  Others may hold on to
           the result register, forcing us to use a different register. *)
        Alloc.release_reg r2;
        (* Try to move the first input to its final destination before
           performing the operation; or failing that, try to leave it
           where it is; or move it to a fresh register if we must. *)
        let v1' = Emitter.map_regs (fun r ->
          if Alloc.is_free r then r else Alloc.alloc_suggest r2) v1 in
        let r1' = Emitter.reg_of v1' in
        Alloc.reserve_reg r1';
        (* Now we can release the rest of the inputs. *)
        List.iter release rest;
        Emitter.use_reg r1';
        (* Move the first operand if necessary *)
        if v1' <> v2 then emit mov [v1'; v2];
        (* Now perform the operation *)
        emit op1 (v1' :: rest);
        (* Finally, move the result to the requested destination. *)
        if Alloc.is_wildcard r1 || r1 = r1' then
          v1'
        else begin
          Alloc.release_reg r1'; Alloc.reserve_reg r1;
          Emitter.use_reg r1; emit mov [v1; v1'];
          v1
        end
        (* The hardest case is something like
             <STOREW,
               <BINOP Minus, <TEMPW a>, <LOADW, <REGVAR b>>>,
               <REGVAR b>>
           where we must move <TEMPW a> to a fresh register, perform
           the subtraction, and only then overwrite the result *)
      end

    (* |gen_move| -- move value to specific register *)
    let gen_move mov rands =
      match rands with
          [dst; src] ->
            if Alloc.is_wildcard (Emitter.reg_of dst)
                  || Emitter.reg_of dst = Emitter.reg_of src then
              src
            else
              gen_reg mov [dst; src]
        | _ -> failwith "gen_move"

    (* |gen| -- emit an instruction *)
    let gen op rands =
      List.iter release rands;
      emit op rands
  end

  (* The generic CSE pass specialised to this target. *)
  module Share = Share.F(Tgt.Metrics)(Alloc)

  (* The instruction selector for the target. *)  
  module Select = Tgt.Selector(IQueue)

  (* |process| -- generate code for a statement, or note a line number *)
  let process =
    function
        <LINE n> ->
          IQueue.set_line n
      | t ->
          if !debug > 0 then
            IQueue.emit_tree t;
          Select.tran_stmt t;
          if !debug > 1 then
            IQueue.emit_comment (Alloc.dump_regs ())

  (* unnest -- move procedure calls to top level *)
  let unnest code =
    let rec do_tree =
      function
          <CALLW n, @args> ->
            let t = Alloc.new_temp 1 in
            <AFTER, 
              <DEFTEMP t, <CALLW n, @(List.map do_tree args)>>, 
              <TEMPW t>>
        | <CALLQ n, @args> ->
            let t = Alloc.new_temp 1 in
            <AFTER, 
              <DEFTEMP t, <CALLQ n, @(List.map do_tree args)>>, 
              <TEMPQ t>>
        | <w, @args> ->
            <w, @(List.map do_tree args)> in
    let do_root =
      function <op, @args> -> <op, @(List.map do_tree args)> in
    Optree.canon <SEQ, @(List.map do_root code)>

  let rec max_params =
    function
      <w, @args> ->
        let x = match w with CALL n | CALLW n | CALLQ n -> n | _ -> 0 in
        max x (Util.maximum (List.map max_params args))

  let show label code =
    if !debug > 1 then begin
      printf "$$:\n" [fStr Emitter.comment; fStr label];
      List.iter (Optree.print_optree Emitter.comment) code;
      printf "\n" []
    end;
    code

  (* |translate| -- translate a procedure body *)
  let translate lab level nargs fsize nregv code =
    Alloc.get_regvars nregv;
    Alloc.set_outgoing (Util.maximum (List.map max_params code));

    let code0 = show "Initial code" code in
    let code1 = if !optlevel < 1 then code0 else
        show "After simplification"
          (Jumpopt.optimise (Simp.optimise code0)) in
    let code2 = if !optlevel < 2 then 
        show "After unnesting" (unnest code1) 
      else
        show "After sharing" (Share.traverse code1) in

    Emitter.start_proc lab level nargs fsize;
    (try List.iter process (flatten code2) with exc -> 
      (* Code generation failed, but let's see how far we got *)
      IQueue.end_proc (); raise exc);
    IQueue.end_proc ()
end
