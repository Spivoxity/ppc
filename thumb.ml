(* ppcu/thumb.ml *)
(* Copyright (c) 2017--18 J. M. Spivey *)

(* Code generator for ARM/Thumb *)

open Target
open Print
open Optree

module Thumb = struct
  module Metrics = struct
    let int_rep = { r_size = 4; r_align = 4 }
    let char_rep = { r_size = 1; r_align = 1 }
    let bool_rep = { r_size = 1; r_align = 1 }
    let void_rep = { r_size = 0; r_align = 1 }
    let addr_rep = { r_size = 4; r_align = 4 }
    let proc_rep = { r_size = 8; r_align = 4 }
    let param_rep = { r_size = 4; r_align = 4 }
    let max_align = 4

    (* 
    Frame layout:

          arg n  \
          ...     > Stored by caller
          arg 4  /
          arg 3  \
                  > Saved by prolog
     +20  arg 0  /
          ----------------
     +16  return address
     +12  saved r7
      +8  saved r6
      +4  saved r5
   vfp:	  saved r4
          ----------------
      -4  local 1
          ...
          local m
          ----------------
          outgoing arg a
          ...
      +4  outgoing arg 5
    sp:   outgoing arg 4
    *)

    let param_base = 20
    let local_base lev = 0
    let stat_link = 12
    let nregvars = 2
    let share_globals = true
    let sharing = 0

    (* ARM register assignments:

       R0-3   arguments + scratch
       R4-R6  callee-save temps
       R7     static link
       R12=sp stack pointer
       R14=lr link register
       R15=pc program counter 

    *)

    let reg_names =
      [| "r0"; "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7";
          "r8"; "r9"; "r10"; "fp"; "sp"; "ip"; "lr"; "pc" |]

    let reg i = Reg i
    let r_sp = Reg 12
    let r_lr = Reg 14
    let r_pc = Reg 15

    let volatile = [reg 0; reg 1; reg 2; reg 3; reg 7]
    let stable = [reg 4; reg 5; reg 6]
  end

  module Alloc = Regs.AllocF(Metrics)

  open Metrics
  open Alloc

  module Emitter = struct
    let space = ref 0

    (* |operand| -- type of operands for assembly instructions *)
    type operand =		  (* VALUE	  ASM SYNTAX       *)
        Const of int 		  (* val	  #val	           *)
      | Register of reg		  (* [reg]	  reg	           *)
      | Shift of reg * int        (* [reg]<<n     reg, LSL #n      *)
      | Index of reg * int   	  (* [reg]+val    [reg, #val]      *)
      | Index2 of reg * reg       (* [r1]+[r2]    [r1, r2]         *)
      | Global of symbol	  (* lab	  lab	           *)
      | Label of codelab	  (* lab	  lab              *)
      | Literal of symbol * int   (* lab+val	  =lab+val         *)

    let reg_of =
      function
          Register r -> r
        | _ -> failwith "reg_of"

    let anyreg = Register R_any
    let anytemp = Register R_temp
    let suggest r =
      match reg_of r with
          Reg i -> Register (R_suggest i)
        | R_temp -> anytemp
        | _ -> anyreg

    let map_regs f =
      function
          Const n -> Const n
        | Register r -> Register (f r)
        | Shift (r, n) -> Shift (f r, n)
        | Index (r, n) -> Index (f r, n)
        | Index2 (r1, r2) -> Index2 (f r1, f r2)
        | Global x -> Global x
        | Label lab -> Label lab
        | Literal (x, n) -> Literal (x, n)

    let use_reg _ = ()

    (* |fRand| -- format operand for printing *)
    let fRand =
      function
          Const v -> fMeta "#$" [fNum v]
        | Register reg -> fReg reg
        | Shift (reg, n) ->
            fMeta "$, LSL #$" [fReg reg; fNum n]
        | Index (reg, off) ->
            if off = 0 then fMeta "[$]" [fReg reg]
            else fMeta "[$, #$]" [fReg reg; fNum off]
        | Index2 (r1, r2) ->
            fMeta "[$, $]" [fReg r1; fReg r2]
        | Global lab -> fStr lab
        | Label lab -> fMeta ".$" [fLab lab]
        | Literal (x, n) ->
            if x = "" then fMeta "=$" [fNum n]
            else if n = 0 then fMeta "=$" [fStr x]
            else if n > 0 then fMeta "=$+$" [fStr x; fNum n]
            else fMeta "=$-$" [fStr x; fNum (-n)]

    (* |preamble| -- emit start of assembler file *)
    let preamble () =
      printf "@ picoPascal compiler output\n" [];
      printf "\t.syntax unified\n" [];
      printf "\t.thumb\n" [];
      printf "\t.global pmain\n\n" [];

      printf "\t.macro caselab x, y\n" [];
      printf "\t.hword \\x-\\y-4\n" [];
      printf "\t.endm\n\n" []

    (* |postamble| -- finish the assembler file *)
    let postamble () =
      printf "@ End\n" []

    (* |seg| -- type of assembler segments *)
    type seg = Text | Data | RoData | Unknown

    (* |current_seg| -- current output segment *)
    let current_seg = ref Unknown

    (* |segment| -- emit segment directive if needed *)
    let segment s =
      if !current_seg <> s then begin
        let seg_name = 
          match s with 
              Text -> ".text" | Data -> ".data" | RoData -> ".rodata"
            | Unknown -> "*unknown*" in
        printf "\t.section $\n" [fStr seg_name];
        current_seg := s
      end

    (* |put_string| -- output a string constant *)
    let put_string lab s =
      segment RoData;
      printf "$:" [fStr lab];
      let n = String.length s in
      for k = 0 to n-1 do
        let c = int_of_char s.[k] in
        if k mod 10 = 0 then 
          printf "\n\t.byte $" [fNum c]
        else
          printf ", $" [fNum c]
      done;
      printf "\n\t.byte 0\n" []

    (* |put_global| -- output a global variable *)
    let put_global lab n =
      printf "\t.comm $, $, 4\n" [fStr lab; fNum n]

    let proclab = ref ""
    let frame = ref 0
    let stack = ref 0
    let nargs = ref 0

    (* |start_proc| -- emit start of procedure *)
    let start_proc lab lev na fram =
      proclab := lab; frame := fram; nargs := na; stack := 0; 
      let ns = Alloc.outgoing () in
      if ns > 4 then stack := 4*ns-16;
      space := 4 * ((!frame + !stack + 3)/4)

    let adjsp op =
      if !space <= 1024 then
        (* Since space is a multiple of 8, we can fit values up to 1024 *)
        (if !space > 0 then printf "\t$ sp, #$\n" [fStr op; fNum !space])
      else begin
        printf "\tldr r3, =$\n" [fNum !space];
        if op = "sub" then printf "\tnegs r3, r3\n" [];
        printf "\tadd sp, r3\n" []
      end

    let prelude () =
      segment Text;
      printf "\t.thumb_func\n" [];
      printf "$:\n" [fStr !proclab];
      if !nargs = 1 then
        printf "\tpush {r0}\n" []
      else if !nargs > 1 then
        printf "\tpush {r0-r$}\n" [fNum (min 3 (!nargs-1))];
      printf "\tpush {r4-r7, lr}\n" [];
      adjsp "sub"

    let postlude () =
      adjsp "add";
      if !nargs = 0 then
        printf "\tpop {r4-r7, pc}\n" []
      else begin
        printf "\tpop {r4-r7}\n" [];
        printf "\tpop {r3}\n" [];
        printf "\tadd sp, sp, #$\n" [fNum (min 16 (!nargs*4))];
        printf "\tbx r3\n" [];
      end;
      printf "\t.ltorg\n" [];               (* Output the literal table *)
      printf "\n" []

    let put_inst op rands =
      match rands with
          [] -> printf "\t$\n" [fStr op]
        | _ ->  printf "\t$ $\n" [fStr op; fList(fRand) rands]

    let comment = "@ "
  end (* Emitter *)

  module Selector(IQueue : Target.IQueueT
      with type operand = Emitter.operand) = struct
    open Emitter
    open IQueue

    let move_reg dst src =
      ignore (gen_reg "movs" [Register dst; Register src])

    (* Tests for fitting in various immediate fields *)

    (* |fits_immed3| -- test for fitting in immediate field *)
    let fits_immed3 x = (x >= 0 && x < 4)

    (* |fits_immed8| -- test for fitting in immediate field *)
    let fits_immed8 x = (x >= 0 && x < 256)

    (* The main part of the code generator consists of a family of functions
       eval_X t, each generating code for a tree t, leaving the value in
       a register, or as an operand for another instruction, etc. *)

    (* |eval_reg| -- evaluate expression with result in specified register *)
    let rec eval_reg t r =
      (* returns |Register| *)

      (* Binary operation *)
      let binary op t1 t2 =
        let v1 = eval_reg t1 anyreg in
        let v2 = eval_reg t2 anyreg in
        gen_reg ("*movs/" ^ op) [r; v1; v2]

      and add_sub op t1 t2 =
        match t2 with
            <CONST n> when fits_immed3 n ->
              let v1 = eval_reg t1 anyreg in
              gen_reg op [r; v1; Const n]
          | <CONST n> when fits_immed8 n ->
              let v1 = eval_reg t1 (suggest r) in
              gen_reg ("*movs/" ^ op) [r; v1; Const n]
          | _ ->
              let v1 = eval_reg t1 anyreg in
              let v2 = eval_reg t2 anyreg in
              gen_reg op [r; v1; v2]

      (* Unary operation *)
      and unary op t1 =
        let v1 = eval_reg t1 anyreg in
        gen_reg op [r; v1]

      (* Comparison with boolean result *)
      and compare op t1 t2 =
        let lab = label () in
        let v = gen_reg "movs" [r; Const 1] in
        let v1 = eval_reg t1 anyreg in
        let v2 = eval_reg t2 anyreg in
        gen "cmp" [v1; v2];
        emit op [Label lab];
        emit "movs" [v; Const 0];
        emit_lab lab;
        v

      and shift op t1 t2 =
        match t2 with
            <CONST n> when n >= 0 && n < 32 ->
              let v1 = eval_reg t1 anyreg in
              gen_reg op [r; v1; Const n]
          | _ ->
              let v1 = eval_reg t1 (suggest r) in
              let v2 = eval_reg t2 anyreg in
              gen_reg ("*movs/" ^ op) [r; v1; v2] in

      match t with
          <CONST k> when fits_immed8 k -> 
            gen_reg "movs" [r; Const k]
        | <CONST k> ->
            gen_reg "ldr" [r; Literal ("", k)]
        | <NIL> ->
            gen_reg "movs" [r; Const 0]
        | <LOCAL n> when fits_immed8 ((!space+n) / 4) ->
            let a = (!space+n) mod 4 in
            if a = 0 then
              gen_reg "add" [r; Register r_sp; Const (!space+n)]
            else begin
              let v1 =
                gen_reg "add" [anyreg; Register r_sp; Const (!space+n-a)] in
              gen_reg "adds" [r; v1; Const a]
            end
        | <LOCAL n> ->
            let v1 = gen_reg "ldr" [r; Literal ("", !space+n)] in
            emit "add" [v1; Register r_sp];
            v1
        | <GLOBAL x> ->
            gen_reg "ldr" [r; Literal (x, 0)]
        | <TEMPW n> ->
            gen_move "mov" [r; Register (Alloc.use_temp n)]
        | <(LOADW|LOADC), <REGVAR i>> ->
            let rv = regvar i in
            reserve_reg rv; gen_move "mov" [r; Register rv]
        | <LOADW, <LOCAL n>> when !space+n < 1024 ->
            gen_reg "ldr" [r; Index (r_sp, !space+n)]
        | <LOADW, t1> -> 
            let v1 = eval_addr 128 t1 in
            gen_reg "ldr" [r; v1]
        | <LOADC, t1> -> 
            let v1 = eval_addr 32 t1 in
            gen_reg "ldrb" [r; v1]

        | <MONOP Uminus, t1> -> unary "negs" t1
        | <MONOP Not, t1> -> 
            let v1 = eval_reg t1 (suggest r) in
            let v2 = gen_reg "movs" [anyreg; Const 1] in
            gen_reg "*movs/eors" [r; v1; v2]
        | <MONOP BitNot, t1> -> unary "mvns" t1

        | <OFFSET, t1, t2> -> add_sub "adds" t1 t2
        | <BINOP Plus, t1, t2> -> add_sub "adds" t1 t2
        | <BINOP Minus, t1, t2> -> add_sub "subs" t1 t2

        | <BINOP And, t1, t2> -> binary "ands" t1 t2
        | <BINOP Or, t1, t2> -> binary "orrs" t1 t2
        | <BINOP BitAnd, t1, t2> -> binary "ands" t1 t2
        | <BINOP BitOr, t1, t2> -> binary "orrs" t1 t2
        | <BINOP Times, t1, t2> -> binary "muls" t1 t2

        | <BINOP Lsl, t1, t2> -> shift "lsls" t1 t2
        | <BINOP Lsr, t1, t2> -> shift "lsrs" t1 t2
        | <BINOP Asr, t1, t2> -> shift "asrs" t1 t2

        | <BINOP Eq, t1, t2> -> compare "beq" t1 t2
        | <BINOP Neq, t1, t2> -> compare "bne" t1 t2
        | <BINOP Gt, t1, t2> -> compare "bgt" t1 t2
        | <BINOP Geq, t1, t2> -> compare "bge" t1 t2
        | <BINOP Lt, t1, t2> -> compare "blt" t1 t2
        | <BINOP Leq, t1, t2> -> compare "ble" t1 t2

        | <BOUND, t1, t2> ->
            let lab = label () in
            let v1 = eval_reg t1 r in
            let v2 = eval_reg t2 anyreg in
            release v2;
            emit "cmp" [v1; v2];
            emit "blo" [Label lab];
            emit "ldr" [Register (reg 0); Literal ("", !line)];
            emit "bl" [Global "check"];
            emit_lab lab;
            v1

        | <NCHECK, t1> ->
            let lab = label () in
            let v1 = eval_reg t1 r in
            emit "cmp" [v1; Const 0];
            emit "bne" [Label lab];
            emit "ldr" [Register (reg 0); Literal ("", !line)];
            emit "bl" [Global "nullcheck"];
            emit_lab lab;
            v1

        | <w, @args> ->
            failwith (sprintf "eval $" [fInst w])

    (* |eval_addr| -- evaluate to form an address for ldr or str *)
    and eval_addr limit =
      (* returns |Index| or |Index2| *)
      function
          <LOCAL n> ->
            let a = (!space+n) mod 4 in
            let v1 = eval_reg <LOCAL (n-a)> anyreg in
            Index (reg_of v1, a)
        | <OFFSET, t1, <CONST n>> when n >= 0 && n < limit ->
            let v1 = eval_reg t1 anyreg in
            Index (reg_of v1, n)
        | <OFFSET, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_reg t2 anyreg in
            Index2 (reg_of v1, reg_of v2)
        | t ->
            let v1 = eval_reg t anyreg in
            Index (reg_of v1, 0)

    (* |eval_call| -- execute procedure call *)
    let eval_call =
      function
          <GLOBAL f> | <LIBFUN f> -> 
            gen "bl" [Global f]
        | t -> 
            let v1 = eval_reg t anyreg in
            gen "blx" [v1]

    let compare t1 t2 =
      match t2 with
          <CONST n> when fits_immed8 n ->
            let v1 = eval_reg t1 anyreg in
            gen "cmp" [v1; Const n]
        | _ ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_reg t2 anyreg in
            gen "cmp" [v1; v2]

    let statlink = ref R_none

    (* |tran_stmt| -- generate code to execute a statement *)
    let tran_stmt t =
      (* Conditional jump *)
      let condj op lab t1 t2 =
        compare t1 t2; gen op [Label lab] in

      (* Procedure call *)
      let call k t =
        spill_temps move_reg volatile;     (* Spill any remaining temps *)
        eval_call t;                       (* Call the function *)
        for i = 0 to min 3 (k-1) do        (* Release argument registers *)
          release_reg (Reg i)
        done;
        release_reg !statlink;
        statlink := R_none in

      match t with
          <CALL k, t1> -> 
            call k t1

        | <DEFTEMP n, <CALLW k, t1>> ->
            call k t1;
            reserve_reg (reg 0); 
            Alloc.def_temp n (reg 0)

        | <DEFTEMP n, t1> ->
            let r = temp_reg n in
            if r = R_none then begin
              let v1 = eval_reg t1 anytemp in
              Alloc.def_temp n (reg_of v1)
            end else begin
              (* A short-circuit condition: assume no spills *)
              let v1 = eval_reg t1 (Register r) in
              release v1
            end

        | <(STOREW|STOREC), t1, <REGVAR i>> ->
            let rv = regvar i in
            release (eval_reg t1 (Register rv))
        | <STOREW, t1, <LOCAL n>> when !space+n < 1024 ->
            let v1 = eval_reg t1 anyreg in
            gen "str" [v1; Index (r_sp, !space+n)]
        | <STOREW, t1, t2> -> 
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr 128 t2 in
            gen "str" [v1; v2]
        | <STOREC, t1, t2> -> 
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr 32 t2 in
            gen "strb" [v1; v2]

        | <RESULTW, t1> ->
            release (eval_reg t1 (Register (reg 0)))

        | <LABEL lab> -> emit_lab lab

        | <JUMP lab> -> gen "b" [Label lab]

        | <JUMPC (Eq, lab), t1, t2> -> condj "beq" lab t1 t2
        | <JUMPC (Lt, lab), t1, t2> -> condj "blt" lab t1 t2
        | <JUMPC (Gt, lab), t1, t2> -> condj "bgt" lab t1 t2
        | <JUMPC (Leq, lab), t1, t2> -> condj "ble" lab t1 t2
        | <JUMPC (Geq, lab), t1, t2> -> condj "bge" lab t1 t2
        | <JUMPC (Neq, lab), t1, t2> -> condj "bne" lab t1 t2

        | <JCASE (table, deflab), t1> ->
            let base = label () and tbl = label () in
            let v1 = eval_reg t1 anyreg in
            emit "cmp" [v1; Const (List.length table)];
            emit "bhs" [Label deflab];
            let v2 = gen_reg "lsls" [anyreg; v1; Const 1] in
            let v3 = gen_reg "adr" [anyreg; Label tbl] in
            let v4 = gen_reg "ldrsh" [anyreg; Index2 (reg_of v3, reg_of v2)] in
            emit_lab base;
            gen "add" [Register r_pc; v4];
            emit ".align 2" [];
            emit_lab tbl;
            List.iter (fun l -> emit "caselab" [Label l; Label base]) table

        | <ARG i, <TEMPW k>> when i < 4 ->
            (* Avoid annoying spill and reload if the value is a temp
               already in the correct register: e.g. in f(g(x)). *)
            let r = reg i in
            let r1 = Alloc.use_temp k in
            spill_temps move_reg [r];
            ignore (gen_move "mov" [Register r; Register r1])
        | <ARG i, t1> when i < 4 ->
            let r = reg i in
            spill_temps move_reg [r];
            ignore (eval_reg t1 (Register r))
        | <ARG i, t1> when i >= 4 ->
            let v1 = eval_reg t1 anyreg in
            gen "str" [v1; Index (r_sp, 4*i-16)]

        | <STATLINK, t1> ->
            let r = reg 7 in
            spill_temps move_reg [r];
            ignore (eval_reg t1 (Register r));
            statlink := r

        | <w, @ts> -> 
            failwith (sprintf "tran_stmt $" [fInst w])

    let relax_branches q0 =
      let changed = ref true in

      let instr_size =
        function "bl" -> 2 | _ -> 1 in

      (* Find opposite of a branch or raise Not_found *)
      let opposite =
        function
          | "beq" -> "bne" | "bne" -> "beq"
          | "blt" -> "bge" | "bge" -> "blt"
          | "bgt" -> "ble" | "ble" -> "bgt"
          | "bhi" -> "bls" | "bls" -> "bhi"
          | "blo" -> "bhs" | "bhs" -> "blo"
          | _ -> raise Not_found in

      let is_branch w = Util.can opposite w in

      (* Make a map of label addresses *)
      let make_map q =
        let m = Hashtbl.create 37 in
        let g addr =
          function
              Instr (w, _) -> addr + instr_size w
            | Label lab -> Hashtbl.add m lab addr; addr
            | _ -> addr in
        ignore (Queue.fold g 0 q); m in

      (* Then use the map to expand overlong branches *)
      let pass q = begin
        let m = make_map q in
        let reach addr lab d =
          let dest = Hashtbl.find m lab in
          dest >= addr + 2 - d && dest < addr + 2 + d in
        let q1 = Queue.create () in
        let h addr x =
          match x with
              Instr (w, [Label lab]) when is_branch w ->
                if reach addr lab 128 then
                  Queue.add x q1
                else begin
                  changed := true;
                  let lab1 = label () in
                  Queue.add (Instr (opposite w, [Label lab1])) q1;
                  Queue.add (Instr ("b", [Label lab])) q1;
                  Queue.add (Label lab1) q1
                end;
                addr+1
(*
            | Instr ("b", [Label lab]) ->
                if reach addr lab 1024 then
                  Queue.add x q1
                else
                  (* Use bl and assume it's OK to trash lr *)
                  Queue.add (Instr ("bl", [Label lab])) q1;
                addr+1
*)
            | Instr (w, _) ->
                Queue.add x q1; addr + instr_size w
            | _ -> Queue.add x q1; addr in
        ignore (Queue.fold h 0 q); q1
      end in
      
      (* Iterate until no further change *)
      let rec loop q =
        if not !changed then q else begin
          changed := false;
          let q1 = pass q in loop q1
        end in
      loop q0

    (* Install the rewriter *)
    let _ = IQueue.set_rewriter relax_branches
  end (* Selector *)

  let options = []
end (* Thumb *)

module Compiler = Main.F(Thumb)
