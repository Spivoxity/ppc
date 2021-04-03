(* ppcu/mips.ml *)
(* Copyright (c) 2017--18 J. M. Spivey *)

open Target
open Print
open Optree

let zero = Int32.zero
let one = Int32.one
let int32 x = Int32.of_int x

module MIPS = struct
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
    +4    ...     > Saved by prolog
  fp:	  arg 0  /
          ----------------
    -4    possible static link (passed in t8) A
    -8	  local 1                             frame
   -12	  ...                                 |
          local m                             V
          ----------------
          return address                      A
          saved s7                            |
          saved s6                           4*nsaved + 4
          saved s5                            |
    +16   saved s4                            |
    +12   saved s3                            |
    +8    saved s2                            |
    +4    saved s1                            |
  sbase:  saved s0                            V
          ----------------
          padding
          ----------------
          outgoing arg a-1
    +8        ...
    +4    outgoing arg 1
  sp:     outgoing arg 0
  *)

    let param_base = 0
    let local_base lev = if lev > 1 then -4 else 0
    let local_align = 1
    let stat_link = -4
    let nregvars = 3
    let share_globals = true
    let sharing = 2
    let fixed_frame = true

    (* MIPS register assignments:

       0     R0      Zero
       1             Assembler temp
       2-3   V0..1   Results
       4-7   A0..4   Arguments
       8-15  T0..7   Temps
       16-23 S0..7   Saved regs
       24-25 T8..9   More temps (T8 used to pass stat link)
       26-7  (K0, K1)
       28    (GP)
       29    SP      Stack pointer
       30    FP      Frame pointer
       31    RA      Return address *)

    let reg_names =
      [| "$0"; "$at"; "$v0"; "$v1"; "$a0"; "$a1"; "$a2"; "$a3";
          "$t0"; "$t1"; "$t2"; "$t3"; "$t4"; "$t5"; "$t6"; "$t7";
          "$s0"; "$s1"; "$s2"; "$s3"; "$s4"; "$s5"; "$s6"; "$s7";
          "$t8"; "$t9"; "$k0"; "$k1"; "$gp"; "$sp"; "$fp"; "$ra";
          "$vfp" |]

    let r0 = Reg 0
    let rV i = Reg (2+i)
    let rA i = Reg (4+i)
    let rT i = if i < 8 then Reg (8+i) else Reg (16+i)
    let rS i = Reg (16+i)
    let r_sp = Reg 29
    let r_fp = Reg 30
    let r_ra = Reg 31
    let r_vfp = Reg 32

    let volatile = [rT 0; rT 1; rT 2; rT 3; rT 4; rT 5; rT 6; rT 7; rV 0]
    let stable = [rS 0; rS 1; rS 2; rS 3; rS 4; rS 5; rS 6; rS 7]
  end

  module Alloc = Regs.AllocF(Metrics)

  open Metrics
  open Alloc

  module Emitter = struct
    (* |space| -- offset between $sp and virtual frame pointer *)
    let space = ref 0

    (* |operand| -- type of operands for assembly instructions *)
    type operand =		     (* VALUE         ASM SYNTAX       *)
        Const of int32 		     (* val	      val	       *)
      | Register of reg		     (* [reg]	      $reg	       *)
      | Global of symbol 	     (* lab+val	      lab+val	       *)
      | Offset of int * reg          (* val+[reg]     val($reg)        *)
      | Index of symbol * reg        (* lab+val+[reg] lab+val($reg)    *)
      | Label of codelab	     (* lab	      lab              *)

    let anyreg = Register R_any
    let anytemp = Register R_temp

    let map_regs f =
      function
          Const n -> Const n
        | Register r -> Register (f r)
        | Global x -> Global x
        | Offset (n, r) -> Offset (n, f r)
        | Index (x, r) -> Index (x, f r)
        | Label lab -> Label lab

    (* |reg_of| -- extract register from operand *)
    let reg_of =
      function
          Register reg -> reg
        | _ -> failwith "reg_of"

    let fOff n =
      if n = 0 then fStr "" else fNum n

    (* |fRand| -- format operand for printing *)
    let fRand =
      function
          Const v -> fNum32 v
        | Register reg -> fReg reg
        | Global x -> fSym x
        | Offset (n, r) ->
            if r = r_vfp then
              fMeta "$($)" [fOff (!space+n); fReg r_sp]
            else
              fMeta "$($)" [fOff n; fReg r]
        | Index (x, r) -> fMeta "$($)" [fSym x; fReg r]
        | Label lab -> fMeta ".$" [fLab lab]

    (* |preamble| -- emit start of assembler file *)
    let preamble () =
      printf "# picoPascal compiler output\n" [];
      printf "\t.set noreorder\n" [];
      printf "\t.global pmain\n\n" []

    (* |postamble| -- finish the assembler file *)
    let postamble () =
      printf "# End\n" []

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
      printf "$:" [fSym lab];
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
      printf "\t.comm $, $, 4\n" [fSym lab; fNum n]

    let put_jumptab lab table =
      segment RoData;
      printf "\t.align 2\n" [];
      printf "$:\n" [fSym lab];
      List.iter (fun l -> printf "\t.word .$\n" [fLab l]) table

    let proclab = ref nosym
    let level = ref 0
    let nargs = ref 0
    let frame = ref 0
    let stack = ref 0
    let sbase = ref 0
    let max_reg = ref 0
    let nsaved = ref 0

    let use_reg =
      function
          Reg i ->
            if i >= 16 && i < 24 then max_reg := max i !max_reg
        | _ ->
            failwith "use_reg"

    let param_offset () = Metrics.param_base

    (* |start_proc| -- emit start of procedure *)
    let start_proc lab lev na fram =
      proclab := lab; level := lev; nargs := na;
      stack := 4 * Alloc.outgoing ();
      if !stack < 16 then stack := 16;
      frame := if lev > 0 then fram+4 else fram;
      max_reg := 0

    let put_inst op rands =
      match rands with
          [] -> printf "\t$\n" [fStr op]
        | _ ->  printf "\t$ $\n" [fStr op; fList(fRand) rands]

    let prelude () =
      (* Round up frame space for stack alignment *)
      nsaved := if !max_reg >= 16 then !max_reg - 15 else 0;
      let locspace = !frame + !nsaved*4 + 4 in
      space := 8 * ((!stack + locspace + 7)/8);
      sbase := !space - locspace;
      segment Text;
      printf "$:\n" [fSym !proclab];
      put_inst "addu" [Register r_sp; Register r_sp; Const (int32 (- !space))];
      put_inst "sw" [Register r_ra; Offset (!sbase + !nsaved*4, r_sp)];
      for i = !nsaved-1 downto 0 do
        put_inst "sw" [Register (rS i); Offset (!sbase + 4*i, r_sp)]
      done;
      if !nargs > 0 then begin
        for i = min !nargs 4 - 1 downto 0 do
          put_inst "sw" [Register (rA i); Offset (4*i, r_vfp)]
        done;
      end;
      if !level > 0 then
        put_inst "sw" [Register (rT 8); Offset (-4, r_vfp)]

    let postlude () =
      for i = 0 to !nsaved-1 do
        put_inst "lw" [Register (rS i); Offset (!sbase + 4*i, r_sp)]
      done;
      put_inst "lw" [Register r_ra; Offset (!sbase + !nsaved*4, r_sp)];
      put_inst "addu" [Register r_sp; Register r_sp; Const (int32 !space)];
      put_inst "jr" [Register r_ra];
      put_inst "nop" [];
      printf "\n" []

    let comment = "# "
  end (* Emitter *)

  module Selector(IQueue : Target.IQueueT
      with type operand = Emitter.operand) = struct
    open Emitter
    open IQueue

    let move_reg dst src =
      ignore (gen_reg "move" [Register dst; Register src])

    (* The main part of the code generator consists of a family of functions
       eval_X t, each generating code for a tree t, leaving the value in
       a register, or as an operand for another instruction, etc. *)

    let const n = Const (int32 n)

    (* |eval_reg| -- evaluate expression with result in specified register *)
    let rec eval_reg t r =

      let binary op t1 t2 =
        let v1 = eval_reg t1 anyreg in
        let v2 = eval_rand t2 in
        gen_reg op [r; v1; v2] in

      let shift op t1 t2 =
        let v1 = eval_reg t1 anyreg in
        let v2 = eval_rand t2 in (* Should check < 32 *)
        gen_reg op [r; v1; v2] in

      match t with
          <CONST k> when k = Int32.zero ->
            gen_move "move" [r; Register r0]
        | <CONST k> ->
            gen_reg "li" [r; Const k]
        | <LOCAL (_, n)> ->
            gen_reg "la" [r; Offset(n, r_vfp)]
        | <GLOBAL x> ->
            gen_reg "la" [r; Global x]
        | <NIL> ->
            gen_move "move" [r; Register r0]
        | <TEMPW n> ->
            gen_move "move" [r; Register (Alloc.use_temp n)]
        | <(LOADW|LOADC), <REGVAR i>> ->
            let rv = regvar i in
            reserve_reg rv; gen_move "move" [r; Register rv]
        | <LOADW, t1> ->
            let v1 = eval_addr t1 in
            gen_reg "lw" [r; v1]
        | <LOADC, t1> ->
            let v1 = eval_addr t1 in
            gen_reg "lbu" [r; v1]

        | <MONOP Uminus, t1> ->
            let v1 = eval_reg t1 anyreg in
            gen_reg "subu" [r; Register (r0); v1]
        | <MONOP Not, t1> ->
            let v1 = eval_reg t1 anyreg in
            gen_reg "xor" [r; v1; Const one]
        | <MONOP BitNot, t1> ->
            let v1 = eval_reg t1 anyreg in
            gen_reg "nor" [r; v1; Register (r0)]

        | <OFFSET, t1, t2> -> binary "addu" t1 t2

        | <BINOP Plus, t1, t2> -> binary "addu" t1 t2
        | <BINOP Minus, t1, t2> -> binary "subu" t1 t2
        | <BINOP And, t1, t2> -> binary "and" t1 t2
        | <BINOP Or, t1, t2> -> binary "or" t1 t2
        | <BINOP Lsl, t1, t2> -> shift "sll" t1 t2
        | <BINOP Lsr, t1, t2> -> shift "srl" t1 t2
        | <BINOP Asr, t1, t2> -> shift "sra" t1 t2
        | <BINOP BitAnd, t1, t2> -> binary "and" t1 t2
        | <BINOP BitOr, t1, t2> -> binary "or" t1 t2

        | <BINOP Times, t1, t2> ->
            (* The mul instruction needs both operands in registers *)
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_reg t2 anyreg in
            gen_reg "mul" [r; v1; v2]

        | <BINOP Eq, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_rand t2 in
            let v3 = gen_reg "subu" [anyreg; v1; v2] in
            gen_reg "sltu" [r; v3; Const one]

        | <BINOP Neq, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_rand t2 in
            let v3 = gen_reg "subu" [anyreg; v1; v2] in
            gen_reg "sltu" [r; Register r0; v3]

        | <BINOP Lt, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_rand t2 in
            gen_reg "slt" [r; v1; v2]

        | <BINOP Gt, t2, t1> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_rand t2 in
            gen_reg "slt" [r; v1; v2]

        | <BINOP Geq, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_rand t2 in
            let v3 = gen_reg "slt" [anyreg; v1; v2] in
            gen_reg "xor" [r; v3; Const one]

        | <BINOP Leq, t2, t1> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_rand t2 in
            let v3 = gen_reg "slt" [anyreg; v1; v2] in
            gen_reg "xor" [r; v3; Const one]

        | <BOUND, t1, t2> ->
            let lab1 = label () in
            let v1 = eval_reg t1 r in
            let v2 = eval_rand t2 in
            release v2;
            emit "bltu" [v1; v2; Label lab1];
            emit "li" [Register (rA 0); const !line];
            emit "jal" [Global (symbol "check")]; emit "nop" [];
            emit_lab lab1;
            v1

        | <NCHECK, t1> ->
            let lab1 = label () in
            let v1 = eval_reg t1 r in
            emit "bne" [v1; Register r0; Label lab1];
            emit "li" [Register (rA 0); const !line];
            emit "jal" [Global (symbol "nullcheck")]; emit "nop" [];
            emit_lab lab1;
            v1

        | <w, @args> ->
            failwith (sprintf "eval $" [fInst w])

    (* |eval_rand| -- evaluate to form second operand *)
    and eval_rand =
      (* returns |Const| or |Register| *)
      function
          <CONST k> when k = zero -> Register r0
        | <CONST k> -> Const k
        | <NIL> -> Register r0
        | t -> eval_reg t anyreg

    (* |eval_addr| -- evaluate to form an address for lw or sw *)
    and eval_addr =
      function
          <GLOBAL x> -> Global x
        | <LOCAL (_, n)> -> Offset (n, r_vfp)
        | <OFFSET, <GLOBAL x>, t2> ->
            let v2 = eval_reg t2 anyreg in
            Index (x, reg_of v2)
        | <OFFSET, t1, <CONST n>> ->
            let v1 = eval_reg t1 anyreg in
            Offset (Int32.to_int n, reg_of v1)
        | t ->
            let v1 = eval_reg t anyreg in
            Offset (0, reg_of v1)

    (* |eval_call| -- execute procedure call *)
    let eval_call =
      function
          <GLOBAL f> ->
            gen "jal" [Global f]; gen "nop" []
        | <LIBFUN f> ->
            gen "jal" [Global (symbol f)]; gen "nop" []
        | t ->
            let v1 = eval_reg t anyreg in
            gen "jalr" [v1]; gen "nop" []

    (* |tran_stmt| -- generate code to execute a statement *)
    let tran_stmt t =

      (* Conditional jump *)
      let condj op lab t1 t2 =
        let v1 = eval_reg t1 anyreg in
        let v2 = eval_rand t2 in
        gen op [v1; v2; Label lab]; gen "nop" [] in

      match t with
          <CALL k, t1> ->
            spill_temps move_reg volatile;
            eval_call t1

        | <DEFTEMP n, <CALLW k, t1>> ->
            spill_temps move_reg volatile;
            eval_call t1;
            reserve_reg (rV 0);
            Alloc.def_temp n (rV 0)

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
        | <STOREW, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr t2 in
            gen "sw" [v1; v2]
        | <STOREC, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr t2 in
            gen "sb" [v1; v2]

        | <RESULTW, t1> ->
            release (eval_reg t1 (Register (rV 0)))

        | <LABEL lab> -> emit_lab lab

        | <JUMP lab> -> gen "b" [Label lab]; gen "nop" []

        | <JUMPC (Eq, lab), t1, t2> -> condj "beq" lab t1 t2
        | <JUMPC (Neq, lab), t1, t2> -> condj "bne" lab t1 t2
        | <JUMPC (Lt, lab), t1, t2> -> condj "blt" lab t1 t2
        | <JUMPC (Gt, lab), t1, t2> -> condj "bgt" lab t1 t2
        | <JUMPC (Leq, lab), t1, t2> -> condj "ble" lab t1 t2
        | <JUMPC (Geq, lab), t1, t2> -> condj "bge" lab t1 t2

        | <JCASE (table, deflab), t1> ->
            let n = List.length table in
            let lab1 = gensym () in
            let v1 = eval_reg t1 anyreg in
            reserve v1;
            gen "bgeu" [v1; const n; Label deflab]; gen "nop" [];
            let v2 = gen_reg "sll" [anyreg; v1; const 2] in
            let v3 = gen_reg "lw" [anyreg; Index (lab1, reg_of v2)] in
            gen "j" [v3]; gen "nop" [];
            put_jumptab lab1 table

        | <ARG i, t1> when i < 4 ->
            ignore (eval_reg t1 (Register (rA i)))
        | <ARG i, t1> when i >= 4 ->
            let v1 = eval_reg t1 anyreg in
            gen "sw" [v1; Offset (4*i, r_sp)]

        | <STATLINK, t1> ->
            ignore (eval_reg t1 (Register (rT 8)))

        | <w, @ts> ->
            failwith (sprintf "tran_stmt $" [fInst w])
  end (* Selector *)

  let options = []
end (* MIPS *)

module Compiler = Main.F(MIPS)
