(* ppcu/amd64.ml *)
(* Copyright (c) 2017--18 J. M. Spivey *)

open Target
open Print
open Optree

let pic_mode = ref false

module AMD64 = struct
  module Metrics = struct
    let int_rep = { r_size = 4; r_align = 4 }
    let char_rep = { r_size = 1; r_align = 1 }
    let bool_rep = { r_size = 1; r_align = 1 }
    let void_rep = { r_size = 0; r_align = 1 }
    let addr_rep = { r_size = 8; r_align = 8 }
    let proc_rep = { r_size = 16; r_align = 8 }
    let param_rep = { r_size = 8; r_align = 8 }
    let max_align = 8

    (* 
    Frame layout:

            arg n
    +24     ...
    +16     arg 7
            ----------------
    +8      return address
   B:       dynamic link
    -8      saved rbx    nsaved registers
            saved r12
            saved r13
            saved r14
    -40     saved r15
            ----------------
    -48     arg n copy   If there are more than 6 arguments,
                         copy them here so arguments can be
    +48     arg 7 copy   addressed contiguously.
    +40     arg 6
            arg 5
            ...
   bp:      arg 1
            ----------------
     -8     poss static link (pass in r10)
     -16    local 1
            ...
            local m
            ---------------
   sp :     gap to make B - sp a mult 0f 16


    base = bp - B = -8 * (nsaved + nargs)
    space = bp - sp = base + frame rounded up to 16
    -- including statlink in frame
    (Args 1 to 6 are passed in RDI, RSI, RDX, RCX, R8, R9)

    *)

    let param_base = 0
    let local_base lev = if lev > 1 then -8 else 0
    let stat_link = -8
    let nregvars = 2
    let share_globals = false

    (* Names the the 64-bit registers *)
    let reg_names =
      [| "%rax"; "%rcx"; "%rdx"; "%rbx"; "%rsp"; "%rbp"; "%rsi"; "%rdi";
         "%r8"; "%r9"; "%r10"; "%r11"; "%r12"; "%r13"; "%r14"; "%r15";
         "%rip" |]

    (* Names for the lower-order 32-bit halves *)
    let reg32_names =
      [| "%eax"; "%ecx"; "%edx"; "%ebx"; "%esp"; "%ebp"; "%esi"; "%edi";
         "%r8d"; "%r9d"; "%r10d"; "%r11d"; "%r12d"; "%r13d"; "%r14d"; "%r15d" |]
         
    (* Names for the bottom 8 bits of each register *)
    let reg8_names =
      [| "%al"; "%cl"; "%dl"; "%bl"; "%spl"; "%bpl"; "%sil"; "%dil";
         "%r8b"; "%r9b"; "%r10b"; "%r11b"; "%r12b"; "%r13b"; "%r14b"; "%r15b" |]
         
    let rAX = Reg 0
    let rCX = Reg 1
    let rSP = Reg 4
    let rBP = Reg 5
    let r10 = Reg 10
    let rIP = Reg 16
    
    let argregs = [Reg 7; Reg 6; Reg 2; Reg 1; Reg 8; Reg 9]
    let volatile =
      [Reg 0; Reg 1; Reg 2; Reg 6; Reg 7; Reg 8; Reg 9; Reg 10; Reg 11]
    let stable = [Reg 3; Reg 12; Reg 13; Reg 14; Reg 15]
  end

  module Alloc = Regs.AllocF(Metrics)

  open Metrics
  open Alloc

  module Emitter = struct
    (* Addressing modes:       
    MODE     FIELDS           VALUE                       SYNTAX

    Const        val          val                         $val
    Register         reg      [reg]                       reg
    Addr     lab val          mem[lab+val]                lab+val
    AddrR    lab val reg      mem[lab+val+[reg]]          lab+val(reg)
    AddrRS   lab val reg s    mem[lab+val+[reg]*2^s]      lab+val(,reg,2^s)
    AddrRRS  lab val r1 r2 s  mem[lab+val+[r1]+[r2]*2^s]  lab+val(r1,r2,2^s)
    Label    lab              lab                         lab   
    Indir            reg      [reg]                       *reg            *)

    (* |operand| -- type of operands for assembly instructions *)
    type operand =                  
        Const of int
      | Register of reg * int
      | Addr of symbol * int
      | AddrR of symbol * int * reg
      | AddrRS of symbol * int * reg * int
      | AddrRRS of symbol * int * reg * reg * int
      | Label of codelab
      | Indir of reg

    let reg64 r = Register (r, 64)
    let reg32 r = Register (r, 32)
    let reg r = Register (r, 0)

    let reg_of =
      function
          Register (r, w) -> r
        | _ -> failwith "reg_of"

    let use_reg _ = ()

    let anyreg = Register (R_any, 0)
    let anytemp = Register (R_temp, 0)
    let suggest r =
      match reg_of r with
          Reg i -> Register (R_suggest i, 0)
        | R_temp -> anytemp
        | _ -> anyreg

    let map_regs f =
      function
          Const n -> Const n
        | Register (r, w) -> Register (f r, w)
        | AddrR (x, n, r) -> AddrR (x, n, f r)
        | AddrRS (x, n, r, s) -> AddrRS (x, n, f r, s)
        | AddrRRS (x, n, r1, r2, s) -> AddrRRS (x, n, f r1, f r2, s)
        | Addr (x, n) -> Addr (x, n)
        | Label lab -> Label lab
        | Indir reg -> Indir (f reg)

    let fBase lab off =
      if off = 0 then fStr lab 
      else if lab = "" then fNum off
      else if off > 0 then fMeta "$+$" [fStr lab; fNum off]
      else fMeta "$-$" [fStr lab; fNum (-off)]

    let fScaled reg s =
      if s = 0 then fReg reg else 
        fMeta "$,$" [fReg reg; fNum (1 lsl s)]

    (* |fRand| -- format operand for printing *)
    let fRand =
      function
          Const v -> fMeta "$$" [fChr '$'; fNum v]
        | Register (r, w) ->
            begin match (r, w) with
                (Reg i, 8) -> fStr reg8_names.(i)
              | (Reg i, 32) -> fStr reg32_names.(i)
              | (Reg i, 64) -> fStr reg_names.(i)
              | (Reg i, _) -> fMeta "$-$" [fStr reg_names.(i); fNum w]
              | _ -> fMeta "$-$" [fReg r; fNum w]
            end
        | AddrR (lab, off, reg) ->
            fMeta "$($)" [fBase lab off; fReg reg]
        | AddrRS (lab, off, reg, s) ->
            fMeta "$(,$)" [fBase lab off; fScaled reg s]
        | AddrRRS (lab, off, r1, r2, s) ->
            fMeta "$($,$)" [fBase lab off; fReg r1; fScaled r2 s]
        | Addr (lab, off) -> fBase lab off
        | Label lab -> fMeta ".$" [fLab lab]
        | Indir reg -> fMeta "*$" [fReg reg]

    (* |preamble| -- emit start of assembler file *)
    let preamble () =
      printf "# picoPascal compiler output\n" [];
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
              Text -> ".text" | Data -> ".data"
            | RoData -> ".rodata"
            | Unknown -> "*unknown-seg*" in
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

    let put_jumptab lab table =
      segment RoData;
      printf "\t.align 4\n" [];
      printf "$:\n" [fStr lab];
      List.iter (fun l ->
        if !pic_mode then
          printf "\t.long .$-$\n" [fLab l; fStr lab]
        else
          printf "\t.long .$\n" [fLab l]) table

    (* |put_global| -- output a global variable *)
    let put_global lab n =
      printf "\t.comm $, $, 8\n" [fStr lab; fNum n]

    let proclab = ref nosym
    let level = ref 0
    let nargs = ref 0
    let frame = ref 0
    let maxreg = ref (-1)
    let nsaved = ref 0

    let use_reg r =
      if List.mem r stable then begin
        match r with
            Reg i -> maxreg := max i !maxreg
          | _ -> failwith "use_reg"
      end

    (* |start_proc| -- emit start of procedure *)
    let start_proc lab lev na fsize =
      proclab := lab; level := lev; nargs := na;
      frame := if lev > 0 then fsize+8 else fsize;
      maxreg := -1

    let put_inst op rands =
      match rands with
          [] -> printf "\t$\n" [fStr op]
        | _ ->  printf "\t$ $\n" [fStr op; fList(fRand) (List.rev rands)]

    let prelude () =
      nsaved :=
        if !maxreg <= 0 then 0 else if !maxreg = 3 then 1 else !maxreg-10;
      if (!frame + 8*(!nsaved + !nargs)) mod 16 <> 0 then frame := !frame + 8;
      segment Text;
      printf "$:\n" [fStr !proclab];
      put_inst "pushq" [reg64 rBP];
      if !nargs > 6 then
        put_inst "movq" [reg64 rBP; reg64 rSP];
      List.iter (function r -> put_inst "pushq" [reg64 r])
        (Util.take !nsaved stable);
      for i = !nargs-1 downto 6 do
        put_inst "movq" [reg64 rAX; AddrR ("", 8*i-32, rBP)];
        put_inst "pushq" [reg64 rAX]
      done;
      for i = min (!nargs-1) 5 downto 0 do
        put_inst "pushq" [reg64 (List.nth argregs i)]
      done;
      put_inst "movq" [reg64 rBP; reg64 rSP];
      if !frame > 0 then
        put_inst "subq" [reg64 rSP; Const !frame];
      if !level > 0 then
        put_inst "movq" [AddrR ("", -8, rBP); reg64 r10]

    let postlude () =
      put_inst "addq" [reg64 rSP; Const (!frame + 8 * !nargs)];
      List.iter (function r -> put_inst "popq" [reg64 r])
        (List.rev (Util.take !nsaved stable));
      put_inst "popq" [reg64 rBP];
      put_inst "ret" []

    let put_label lab =
      printf ".$:\n" [fLab lab]

    let comment = "# "
  end (* Emitter *)

  module Selector(IQueue : Target.IQueueT
      with type operand = Emitter.operand) = struct
    open Emitter
    open IQueue

    let resize w =
      function
          Register (r, _) -> Register (r, w)
        | v -> v

    let move_reg dst src =
      (* A 64-bit move is safe *)
      ignore (gen_reg "movq" [reg64 dst; reg64 src])

    let gen_move32 dst src = gen_move "movl" [resize 32 dst; src]
    let gen_move64 dst src = gen_move "movq" [resize 64 dst; src]

    let is_addr (Node (op, _)) =
      match op with
          LOCAL _ | GLOBAL _ | OFFSET | LOADQ | NIL -> true
        | _ -> false

    let gen_reg_w w op rands =
      gen_reg op (resize w (List.hd rands) :: List.tl rands)

    let gen_reg64 op rands = gen_reg_w 64 op rands
    let gen_reg32 op rands = gen_reg_w 32 op rands

    (* |eval_reg| -- evaluate expression with result in a register *)
    let rec eval_reg t r =
      match t with
          <CONST 0> ->
            let v1 = resize 32 (fix_reg r) in
            emit "xorl" [v1; v1]; v1
        | <CONST k> ->
            gen_reg32 "movl" [r; Const k]
        | <NIL> ->
            let v1 = resize 64 (fix_reg r) in
            emit "xorq" [v1; v1]; v1
        | <TEMPW n> ->
            gen_move32 r (reg32 (Alloc.use_temp n))
        | <TEMPQ n> ->
            gen_move64 r (reg64 (Alloc.use_temp n))
        | <(LOADC|LOADW), <REGVAR n>> ->
            let r1 = eval_regvar n in
            gen_move32 r (reg32 r1)
        | <LOADQ, <REGVAR n>> ->
            let r1 = eval_regvar n in
            gen_move64 r (reg64 r1)
        | <LOADW, t1> ->
            let v1 = eval_addr t1 in
            gen_reg32 "movl" [r; v1]
        | <LOADC, t1> ->
            let v1 = eval_addr t1 in
            gen_reg32 "movzbl" [r; v1]
        | <LOADQ, t1> ->
            let v1 = eval_addr t1 in
            gen_reg64 "movq" [r; v1]
        | <MONOP Plus, t1> ->
            eval_reg t1 r

        | <MONOP Uminus, t1> ->
            (* If r is anyreg, then v1 may be a shared temp, in
               which case we will want to move the value before
               negating it. *)
            let v1 = eval_reg t1 r in
            gen_reg32 "*movl/negl" [r; v1]
        | <MONOP Not, t1> ->
            let v1 = eval_reg t1 r in
            gen_reg32 "*movl/xorl" [r; v1; Const 1]
        | <MONOP BitNot, t1> ->
            let v1 = eval_reg t1 r in
            gen_reg32 "*movl/notl" [r; v1]
        | <BINOP Plus, t1, <CONST 1>> ->
            let v1 = eval_reg t1 r in
            gen_reg32 "*movl/incl" [r; v1]     
        | <BINOP Minus, t1, <CONST 1>> ->
            let v1 = eval_reg t1 r in
            gen_reg32 "*movl/decl" [r; v1]

        | <BINOP Plus, t1, t2> -> binary "*movl/addl" r t1 t2
        | <BINOP Minus, t1, t2> -> binary "*movl/subl" r t1 t2
        | <BINOP Times, t1, t2> -> binary "*movl/imull" r t1 t2
        | <BINOP And, t1, t2> -> binary "*movl/andl" r t1 t2
        | <BINOP Or, t1, t2> -> binary "*movl/orl" r t1 t2
        | <BINOP Eq, t1, t2> -> boolean "sete" r t1 t2
        | <BINOP Neq, t1, t2> -> boolean "setne" r t1 t2
        | <BINOP Gt, t1, t2> -> boolean "setg" r t1 t2
        | <BINOP Geq, t1, t2> -> boolean "setge" r t1 t2
        | <BINOP Lt, t1, t2> -> boolean "setl" r t1 t2
        | <BINOP Leq, t1, t2> -> boolean "setle" r t1 t2
        | <BINOP Lsl, t1, t2> -> shift "*movl/shll" r t1 t2
        | <BINOP Lsr, t1, t2> -> shift "*movl/shrl" r t1 t2
        | <BINOP Asr, t1, t2> -> shift "*movl/sarl" r t1 t2
        | <BINOP BitAnd, t1, t2> -> binary "*movl/andl" r t1 t2
        | <BINOP BitOr, t1, t2> -> binary "*movl/orl" r t1 t2

        | _ when is_addr t ->
            let v1 = eval_addr t in
            gen_reg64 "leaq" [r; v1]

        | <w, @args> ->
            failwith (sprintf "eval_reg $" [fInst w])

    and binary op r t1 t2 =
      let v2 = eval_rand t2 in
      let v1 = eval_reg t1 (suggest r) in
      gen_reg32 op [r; v1; v2]

    and shift op r t1 t2 =
      match t2 with
          <CONST n> when 0 <= n && n < 32 ->
              let v1 = eval_reg t1 r in
              gen_reg32 op [r; v1; Const n]
        | _ ->
            if is_free rCX then begin
              let v2 = eval_reg t2 (reg rCX) in
              let v1 = eval_reg t1 (suggest r) in
              gen_reg32 op [r; v1; resize 8 v2]
            end else begin
              (* Care needed: t1 could be a temp living in rCX, and
                 r could also be rCX. *)
              let r1 = get_reg R_any in
              let v1 = eval_reg t1 (reg r1) in
              (* Move the value in CX out of the way *)
              let r2 = get_reg R_any in
              reserve_reg r2;
              emit "movq" [reg64 r2; reg64 rCX];
              (* But it is still in CX if needed to evaluate t2 *)
              let v2 = eval_reg t2 (reg rCX) in
              (* Do the shift and leave the result in r1 *)
              ignore (gen_reg32 op [v1; v1; resize 8 v2]);
              (* Restore CX: its reference count is unchanged *)
              release_reg r2;
              emit "movq" [reg64 rCX; reg64 r2];
              (* Move the result if necessary *)
              gen_move32 r v1
            end

    and boolean op r t1 t2 =
      let v1 = eval_reg t1 anyreg in
      let v2 = eval_rand t2 in
      gen "cmpl" [v1; v2];
      let v = gen_reg op [resize 8 r] in
      gen_reg "movzbl" [resize 32 v; v]

    and eval_regvar n =
      let rv = regvar n in
      reserve_reg rv; rv

    (* |eval_rand| -- evaluate expression as operand *)
    and eval_rand =
      function
          <CONST k> -> Const k
        | <LOADW, <REGVAR i>> -> reg32 (eval_regvar i)
        | <LOADW, t1> -> eval_addr t1
        | t -> eval_reg t anyreg

    and eval_rand64 =
      function
          <NIL> -> Const 0
        | <LOADQ, <REGVAR i>> -> reg64 (eval_regvar i)
        | <LOADQ, t1> -> eval_addr t1
        | t -> eval_reg t anyreg

    (* |eval_addr| -- evaluate expression to address value *)
    and eval_addr =
      function
          <LOCAL n> ->
            AddrR ("", n, rBP)
        | <GLOBAL x> ->
            if !pic_mode then AddrR (x, 0, rIP) else Addr (x, 0)
        | <OFFSET, t1, <CONST n>> ->
            let v1 = eval_addr t1 in
            (match v1 with
                Addr (x, a) -> Addr (x, a+n)
              | AddrR (x, a, r) -> AddrR (x, a+n, r)
              | AddrRS (x, a, r, s) -> AddrRS (x, a+n, r, s)
              | AddrRRS (x, a, r1, r2, s) -> AddrRRS(x, a+n, r1, r2, s)
              | _ -> failwith (sprintf "address $" [fRand v1]))
        | <OFFSET, t1, <BINOP Lsl, t2, <CONST s>>> when s <= 3 ->
            let v1 = eval_addr t1 in
            let v2 = eval_offset t2 in
            (match v1 with
                Addr (x, a) -> AddrRS (x, 0, reg_of v2, s)
              | AddrR (x, a, r) when r != rIP ->
                  AddrRRS (x, a, r, reg_of v2, s)
              | _ ->
                  let v1' = gen_reg64 "leaq" [anyreg; v1] in
                  AddrRRS ("", 0, reg_of v1', reg_of v2, s))
        | <OFFSET, t1, t2> ->
            let v1 = eval_addr t1 in
            let v2 = eval_offset t2 in
            (match v1 with
                Addr (x, a) -> AddrR (x, a, reg_of v2)
              | AddrR (x, a, r) when r != rIP ->
                  AddrRRS (x, a, r, reg_of v2, 0)
              | AddrRS (x, a, r, s) -> AddrRRS (x, a, reg_of v2, r, s)
              | _ ->
                  let v1' = gen_reg64 "leaq" [anyreg; v1] in
                  AddrRRS ("", 0, reg_of v1', reg_of v2, 0))
        | <LOADQ, <REGVAR n>> ->
            let rv = eval_regvar n in AddrR ("", 0, rv)
        | <LOADQ, t1> ->
            let v1 = eval_addr t1 in
            let v2 = gen_reg64 "movq" [anyreg; v1] in
            AddrR ("", 0, reg_of v2)
        | <TEMPQ n> ->
            AddrR ("", 0, Alloc.use_temp n)
        | <w, @args> ->
            failwith (sprintf "eval_addr $" [fInst w])

    and eval_offset t1 =
      let v1 = eval_rand t1 in
      gen_reg64 "movslq" [anyreg; v1]

    (* |branch_op| -- determine branch instruction for comparison operator *)
    let branch_op = 
      function (Eq|EqA) -> "je" | Lt -> "jl" | Gt -> "jg" 
        | Leq -> "jle" | Geq -> "jge" | (Neq|NeqA) -> "jne" 
        | _ -> failwith "bad comparison"

    let in_call = ref false
    let statlink = ref R_none

    let gen_call =
      function
          <GLOBAL x> ->
            gen "call" [Addr (x, 0)]
        | <LIBFUN x> ->
            gen "call"
              [if !pic_mode then Addr (x ^ "@PLT", 0) else Addr (x, 0)]
        | t1 ->
            let v1 = eval_reg t1 anyreg in
            gen "call" [Indir (reg_of v1)]

    let call n t1 =
      Alloc.spill_temps move_reg volatile;
      gen_call t1;
      for i = 0 to min 5 (n-1) do
        release_reg (List.nth argregs i)
      done;
      release_reg !statlink;
      if (n > 6) then
        gen "addq" [reg64 rSP; Const (16*((n+1) / 2)-48)];
      in_call := false; statlink := R_none

    (* |tran_stmt| -- generate code to execute a statement *)
    let tran_stmt =
      function
          <CALL k, t1> -> call k t1

        | <DEFTEMP n, <(CALLW k | CALLQ k), t1>> ->
            call k t1;
            reserve_reg rAX;
            Alloc.def_temp n rAX

        | <DEFTEMP n, t1> ->
            let v1 = eval_reg t1 anytemp in
            Alloc.def_temp n (reg_of v1)

        | <(STOREW|STOREC), t1, <REGVAR n>> ->
            let rv = regvar n in
            let v1 = eval_reg t1 (reg32 rv) in
            release v1
        | <STOREQ, t1, <REGVAR n>> ->
            let rv = regvar n in
            let v1 = eval_reg t1 (reg64 rv) in
            release v1

        | <STOREW, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr t2 in
            gen "movl" [v2; v1]
        | <STOREC, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr t2 in
            gen "movb" [v2; resize 8 v1]
        | <STOREQ, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr t2 in
            gen "movq" [v2; v1]

        | <RESULTW, t1> ->
            let v1 = eval_reg t1 (reg rAX) in
            release v1
        | <RESULTQ, t1> ->
            let v1 = eval_reg t1 (reg rAX) in
            release v1

        | <LABEL lab> ->
            emit_lab lab
        | <JUMP lab> ->
            gen "jmp" [Label lab]
        | <JUMPC ((EqA|NeqA) as w, lab), t1, <NIL>> ->
            let v1 = eval_reg t1 anyreg in
            release v1;
            emit "testq" [v1; v1];
            gen (branch_op w) [Label lab]
        | <JUMPC ((EqA|NeqA) as w, lab), t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_rand64 t2 in
            gen "cmpq" [v1; v2];
            gen (branch_op w) [Label lab]
        | <JUMPC (w, lab), t1, <CONST 0>> ->
            let v1 = eval_reg t1 anyreg in
            release v1;
            emit "testl" [v1; v1];
            gen (branch_op w) [Label lab]
        | <JUMPC (w, lab), t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_rand t2 in
            gen "cmpl" [v1; v2];
            gen (branch_op w) [Label lab]

        | <JCASE (table, deflab), t1> ->
            let n = List.length table in
            let lab1 = gensym () in
            let v1 = eval_reg t1 anyreg in
            emit "cmpl" [v1; Const n];
            emit "jae" [Label deflab];
            if !pic_mode then begin
              let v2a = gen_reg64 "leaq" [anyreg; AddrR (lab1, 0, rIP)] in
              reserve v2a;
              let v2 =
                gen_reg64 "movslq"
                  [anyreg; AddrRRS ("", 0, reg_of v2a, reg_of v1, 2)] in
              release v2a;
              emit "addq" [resize 64 v2; resize 64 v2a];
              gen "jmp" [Indir (reg_of v2)];
            end else begin
              let v2 =
                gen_reg32 "movl" [anyreg; AddrRS (lab1, 0, reg_of v1, 2)] in
              gen "jmp" [Indir (reg_of v2)];
            end;
            put_jumptab lab1 table;

        | <ARG i, t1> when i < 6 ->
            let r = List.nth argregs i in
            spill_temps move_reg [r];
            ignore (eval_reg t1 (reg r))
        | <ARG i, t1> ->
            if not !in_call && i mod 2 = 0 then
              gen "pushq" [Const 0];
            in_call := true;
            let v1 = eval_reg t1 anyreg in
            gen "pushq" [v1]

        | <STATLINK, t1> ->
            Alloc.spill_temps move_reg [r10];
            ignore (eval_reg t1 (reg64 r10));
            statlink := r10

        | <w, @ts> -> 
            failwith (sprintf "tran_stmt $" [fInst w])
  end (* Selector *)

  let options =
    ["-pic", Arg.Unit (fun () -> pic_mode := true), " generate PIC code"]

end (* AMD64 *)

module Compiler = Main.F(AMD64)
