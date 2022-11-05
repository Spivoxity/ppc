(* ppcu/arm64.ml *)
(* Copyright (c) 2017--22 J. M. Spivey *)

(* Code generator for ARM64 *)

open Target
open Print
open Optree

let zero = Int32.zero
let one = Int32.one
let int32 n = Int32.of_int n

module ARM64 = struct
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

          arg n  \
          ...     > Stored by caller
old sp:   arg 8  /
          arg 7  \
     +88          > Saved by prolog
     +80  arg 0  /
          ----------------
     +72  saved r28   (all saved according to need)
          saved r27
          saved r26
          saved r25   
          saved r24
          saved r23
          saved r22
     +16  saved r21
      +8  saved r20
   vfp:   saved r19 (static link -- points to vfp of parent)
          ----------------
          local 1
          ...
     +16  local m
          ----------------
      +8  return address (LR)
    fp:   dynamic link (FP)
          outgoing arg a
          ...
      +8  outgoing arg 9  
    sp:   outgoing arg 8

    vfp - fp = m+16
    *)

    let param_base = 80
    let local_base lev = 0
    let local_align = 1
    let stat_link = 0
    let nregvars = 4
    let share_globals = true
    let sharing = 2
    let addrmode = 2
    let fixed_frame = false

    (* ARM register assignments:

       R0-7   arguments + scratch
       R8-15  scratch
       R19    static link
       R20-28 callee-save temps
       R29=fp frame pointer
       R30=lr link register
       R31=sp stack pointer

    *)

    let reg32_names =
      [| "w0"; "w1"; "w2"; "w3"; "w4"; "w5"; "w6"; "w7";
          "w8"; "w9"; "w10"; "w11"; "w12"; "w13"; "w14"; "w15";
          "w16"; "w17"; "w18"; "w19"; "w20"; "w21"; "w22"; "w23";
          "w24"; "w25"; "w26"; "w27"; "w28"; "?fp"; "?lr"; "?sp"; "wzr" |]

    let reg64_names =
      [| "x0"; "x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7";
          "x8"; "x9"; "x10"; "x11"; "x12"; "x13"; "x14"; "x15";
          "x16"; "ip0"; "ip1"; "x19"; "x20"; "x21"; "x22"; "x23";
          "x24"; "x25"; "x26"; "x27"; "x28"; "fp"; "lr"; "sp"; "xzr" |]

    let reg_names = reg64_names

    let r_fp = Reg 29
    let r_sp = Reg 31
    let r_lr = Reg 30
    let r_ip0 = Reg 17
    let r_zero = Reg 32

    let volatile = [Reg 0; Reg 1; Reg 2; Reg 3; Reg 4; Reg 5; Reg 6; Reg 7;
        Reg 8; Reg 9; Reg 10; Reg 11; Reg 12; Reg 13; Reg 14; Reg 15]
    let stable = [Reg 20; Reg 21; Reg 22; Reg 23; Reg 24; Reg 25; Reg 26;
        Reg 27; Reg 28]
  end

  module Alloc = Regs.AllocF(Metrics)

  open Metrics
  open Alloc

  module Emitter = struct
    (* |operand| -- type of operands for assembly instructions *)
    type operand =		  (* VALUE	  ASM SYNTAX       *)
        Const of symbol * int32   (* lab+val	  #lab+val         *)
      | Register of reg * int	  (* [reg]	  reg	           *)
      | Shift of reg * int        (* [reg]<<n     reg, LSL #n      *)
      | Index of reg * symbol * int  (* [reg]+val    [reg, #val]   *)
      | Index2 of reg * reg * int (* [r1]+[r2]<<n [r1, r2, SXTW #n] *)
      | Global of symbol 	  (* lab	  lab	           *)
      | Label of codelab	  (* lab	  lab              *)
      | LitSym of symbol          (* lab          =lab             *)
      | LitVal of symbol * int32  (* lab+val	  =(lab+val)       *)
      | Quote of string

    let reg64 r = Register (r, 64)
    let reg32 r = Register (r, 32)
    let reg r = Register (r, 0)

    let anyreg = reg R_any
    let anytemp = reg R_temp

    let map_regs f =
      function
          Const (x, n) -> Const (x, n)
        | Register (r, w) -> Register (f r, w)
        | Shift (r, n) -> Shift (f r, n)
        | Index (r, x, n) -> Index (f r, x, n)
        | Index2 (r1, r2, n) -> Index2 (f r1, f r2, n)
        | Global x -> Global x
        | Label lab -> Label lab
        | LitSym x -> LitSym x
        | LitVal (x, n) -> LitVal (x, n)
        | Quote q -> Quote q

    let const n = Const (nosym, n)
    let literal n = LitVal (nosym, n)
    let quote fmt args = Quote (sprintf fmt args)

    let reg_of =
      function
          Register (r, w) -> r
        | _ -> failwith "reg_of"

    let sym_value x n = Int32.add (int32 x.a_val) n
    
    let fReg32 =
      function Reg i -> fStr (reg32_names.(i)) | r -> fReg r

    (* |fRand| -- format operand for printing *)
    let fRand =
      function
          Const (x, n) ->
            fMeta "#$" [fNum32 (sym_value x n)]
        | Register (r, w) ->
            if w = 32 then fReg32 r else fReg r
        | Shift (reg, n) ->
            fMeta "$, LSL #$" [fReg32 reg; fNum n]
        | Index (reg, sym, off) ->
            let n = sym_value sym (int32 off) in
            if n = zero then fMeta "[$]" [fReg reg]
            else fMeta "[$, #$]" [fReg reg; fNum32 n]
        | Index2 (r1, r2, n) ->
            if n = 0 then
              fMeta "[$, $, SXTW]" [fReg r1; fReg32 r2]
            else
              fMeta "[$, $, SXTW #$]" [fReg r1; fReg32 r2; fNum n]
        | Global lab -> fSym lab
        | Label lab -> fMeta ".$" [fLab lab]
        | LitSym x -> fMeta "=$" [fSym x]
        | LitVal (x, n) -> fMeta "=$" [fNum32 (sym_value x n)]
        | Quote q -> fStr q

    (* |preamble| -- emit start of assembler file *)
    let preamble () =
      printf "// picoPascal compiler output\n" [];
      printf "\t.global pmain\n\n" []

    (* |postamble| -- finish the assembler file *)
    let postamble () =
      printf "// End\n" []

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

    let frame = ref 0
    let max_reg = ref 0
    let argcnt = ref 0

    let use_reg =
      function
          Reg i ->
            if i >= 19 && i < 29 && !max_reg < i then begin
              max_reg := i;
              if !max_reg mod 2 == 1 then incr max_reg
            end
        | _ -> failwith "use_reg"

    let param_offset () =
       (* Save r19 .. rmax plus fp, lr *)
       8 * (!max_reg - 18)

    let roundup x n = n * ((x+n-1)/n)

    (* |start_proc| -- emit start of procedure *)
    let start_proc lab lev nargs fram =
      frame := roundup fram 16; argcnt := nargs;
      max_reg := if lev > 1 then 20 else 18; 

      segment Text;
      printf "$:\n" [fSym lab];
      for i = min 3 ((nargs+1)/2 - 1) downto 0 do
        printf "\tstp x$, x$, [sp, -16]!\n" [fNum (2*i); fNum (2*i+1)]
      done

    let adjust_sp op delta =
      if delta < 4096 then begin
        if delta > 0 then
          printf "\t$ sp, sp, #$\n" [fStr op; fNum delta]
      end else begin
        printf "\tldr ip0, =$\n" [fNum delta];
        printf "\t$ sp, sp, ip0\n" [fStr op]
      end

    let prelude () =
      for i = !max_reg/2-1 downto 9 do
        printf "\tstp x$, x$, [sp, -16]!\n" [fNum (2*i+1); fNum (2*i+2)]
      done;
      adjust_sp "sub" !frame;
      printf "\tstp fp, lr, [sp, -16]!\n" [];
      printf "\tmov fp, sp\n" [];
      let nout = Alloc.outgoing () in
      adjust_sp "sub" (max 0 (8 * roundup nout 2 - 64))

    let postlude () =
      let nargs = !argcnt in
      if Alloc.outgoing () > 8 then printf "\tmov sp, fp\n" [];
      printf "\tldp fp, lr, [sp], #16\n" [];
      adjust_sp "add" !frame;
      for i = 9 to !max_reg/2 - 1 do
        printf "\tldp x$, x$, [sp], #16\n" [fNum (2*i+1); fNum (2*i+2)]
      done;
      adjust_sp "add" (min 64 (16 * ((nargs+1)/2)));
      printf "\tret\n" [];
      printf "\t.pool\n" [];               (* Output the literal table *)
      printf "\n" []

    let put_inst op rands =
      match rands with
          [] -> printf "\t$\n" [fStr op]
        | _ ->  printf "\t$ $\n" [fStr op; fList(fRand) rands]

    let comment = "// "
  end (* Emitter *)

  module Selector(IQueue : Target.IQueueT
      with type operand = Emitter.operand) = struct
    open Emitter
    open IQueue

    (* Register sizes *)

    let resize w =
      function
          Register (r, _) -> Register (r, w)
        | v -> v

    let move_reg dst src =
      ignore (gen_reg "mov" [reg64 dst; reg64 src])

    let gen_reg_w w op rands =
      gen_reg op (resize w (List.hd rands) :: List.tl rands)

    let gen_reg32 op rands = gen_reg_w 32 op rands
    let gen_reg64 op rands = gen_reg_w 64 op rands
    
    let gen_move_w w rands =
      gen_move "mov" (resize w (List.hd rands) :: List.tl rands)

    let gen_move32 rands = gen_move_w 32 rands
    let gen_move64 rands = gen_move_w 64 rands


    (* Tests for fitting in various immediate fields *)

    (* |fits_offset| -- test for fitting in offset field of address *)
    let fits_offset shamt x =
      (-256 < x && x < 4096 lsl shamt)

    (* |fits_immed| -- test for fitting in immediate field *)
    let fits_immed x =
      (Int32.zero <= x && x < int32 4096)

    (* |fits_move| -- test for fitting in immediate move *)
    let fits_move x = fits_immed x || fits_immed (Int32.lognot x)

    (* |fits_add| -- test for fitting in immediate add *)
    let fits_add x = fits_immed x || fits_immed (Int32.neg x)

    (* The main part of the code generator consists of a family of functions
       eval_X t, each generating code for a tree t, leaving the value in
       a register, or as an operand for another instruction, etc. *)

    (* |eval_reg| -- evaluate expression with result in specified register *)
    let rec eval_reg t r =
      (* returns |Register| *)

      (* Binary operation *)
      let binary op t1 t2 =
        let v1 = eval_reg t1 anyreg in
        let v2 = eval_rand t2 fits_immed in
        gen_reg32 op [r; v1; v2]

      (* Unary operation *)
      and unary op t1 =
        let v1 = eval_reg t1 anyreg in
        gen_reg32 op [r; v1]

      (* Comparison with boolean result *)
      and compare op t1 t2 =
        let v1 = eval_reg t1 anyreg in
        let v2 = eval_rand t2 fits_add in
        gen "cmp" [v1; v2];
        gen_reg32 "cset" [r; Quote op] in

      match t with
          <CONST k> when k = int32 0 ->
            gen_move32 [r; reg32 r_zero]
        | <CONST k> when fits_move k -> 
            gen_reg32 "mov" [r; const k]
        | <CONST k> ->
            gen_reg32 "ldr" [r; literal k]
        | <SYMBOL (x, n)> ->
            gen_reg32 "ldr" [r; LitVal (x, int32 n)]
        | <NIL> ->
            gen_move64 [r; reg64 r_zero]
        | <LOCAL (x, n)> ->
            let off = int32 (!frame + n + 16) in
            if fits_add (Int32.add (int32 x.a_val) off) then
              gen_reg64 "add" [r; reg64 r_fp; Const (x, off)]
            else begin
              emit "ldr" [reg64 r_ip0; LitVal (x, off)];
              gen_reg64 "add" [r; reg64 r_fp; reg64 r_ip0]
            end
        | <GLOBAL x> ->
            gen_reg64 "ldr" [r; LitSym x]
        | <TEMPW n> ->
            gen_move32 [r; reg32 (Alloc.use_temp n)]
        | <TEMPQ n> ->
            gen_move64 [r; reg64 (Alloc.use_temp n)]
        | <(LOADW|LOADC), <REGVAR i>> ->
            let rv = regvar i in
            reserve_reg rv;
            gen_move32 [r; reg32 rv]
        | <LOADQ, <REGVAR i>> ->
            let rv = regvar i in
            reserve_reg rv;
            gen_move64 [r; reg64 rv]
        | <LOADW, t1> -> 
            let v1 = eval_addr 2 t1 in
            gen_reg32 "ldr" [r; v1]
        | <LOADC, t1> -> 
            let v1 = eval_addr 0 t1 in
            gen_reg32 "ldrb" [r; v1]
        | <LOADQ, t1> ->
            let v1 = eval_addr 3 t1 in
            gen_reg64 "ldr" [r; v1]

        | <MONOP Uminus, t1> -> unary "neg" t1
        | <MONOP Not, t1> -> 
            let v1 = eval_reg t1 anyreg in
            gen_reg32 "eor" [r; v1; const one]
        | <MONOP BitNot, t1> -> unary "mvn" t1

        | <OFFSET, t1, <CONST n>> when fits_add n ->
            let v1 = eval_reg t1 anyreg in
            gen_reg64 "add" [r; v1; const n]
        | <OFFSET, t1, <BINOP Lsl, t2, <CONST n>>> when n <= int32 4 ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_reg t2 anyreg in
            gen_reg "add" [r; v1; v2; quote "SXTW $" [fNum32 n]]
        | <OFFSET, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_reg t2 anyreg in
            gen_reg64 "add" [r; v1; v2; Quote "SXTW"]

        | <BINOP Plus, t1, t2> -> binary "add" t1 t2
        | <BINOP Minus, t1, t2> -> binary "sub" t1 t2
        | <BINOP And, t1, t2> -> binary "and" t1 t2
        | <BINOP Or, t1, t2> -> binary "orr" t1 t2
        | <BINOP Lsl, t1, t2> -> binary "lsl" t1 t2
        | <BINOP Lsr, t1, t2> -> binary "lsr" t1 t2
        | <BINOP Asr, t1, t2> -> binary "asr" t1 t2
        | <BINOP BitAnd, t1, t2> -> binary "and" t1 t2
        | <BINOP BitOr, t1, t2> -> binary "orr" t1 t2

        | <BINOP Times, t1, t2> ->
            (* The mul instruction needs both operands in registers *)
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_reg t2 anyreg in
            gen_reg32 "mul" [r; v1; v2]

        | <BINOP Eq, t1, t2> -> compare "eq" t1 t2
        | <BINOP Neq, t1, t2> -> compare "ne" t1 t2
        | <BINOP Gt, t1, t2> -> compare "gt" t1 t2
        | <BINOP Geq, t1, t2> -> compare "ge" t1 t2
        | <BINOP Lt, t1, t2> -> compare "lt" t1 t2
        | <BINOP Leq, t1, t2> -> compare "le" t1 t2
        | <BINOP EqA, t1, t2> -> compare "eq" t1 t2
        | <BINOP NeqA, t1, t2> -> compare "ne" t1 t2

        | <BOUND, t1, t2> ->
            let lab = label () in
            let v1 = eval_reg t1 r in
            let v2 = eval_rand t2 fits_add in
            release v2;
            emit "cmp" [v1; v2];
            emit "blo" [Label lab];
            emit "ldr" [reg32 (Reg 0); literal (int32 !line)];
            emit "bl" [Global (symbol "check")];
            emit_lab lab;
            v1

        | <NCHECK, t1> ->
            let lab = label () in
            let v1 = eval_reg t1 r in
            emit "cmp" [v1; const zero];
            emit "bne" [Label lab];
            emit "ldr" [reg32 (Reg 0); literal (int32 !line)];
            emit "bl" [Global (symbol "nullcheck")];
            emit_lab lab;
            v1

        | <w, @args> ->
            failwith (sprintf "eval $" [fInst w])

    (* |eval_rand| -- evaluate to form second operand *)
    and eval_rand t fits =
      (* returns |Const| or |Register| *)
      match t with
          <CONST k> when fits k -> const k
        | <NIL> -> const zero
        | <BINOP Lsl, t1, <CONST n>> when n < int32 32 ->
            let v1 = eval_reg t1 anyreg in
            Shift (reg_of v1, Int32.to_int n)
        | _ -> eval_reg t anyreg

    (* |eval_addr| -- evaluate to form an address for ldr or str *)
    and eval_addr shamt =
      (* returns |Index| or |Index2| *)
      function
          <LOCAL (x, n)> ->
            let off = !frame + n + 16 in
            if fits_offset shamt (x.a_val + off) then
              Index (r_fp, x, off)
            else begin
              let r = gen_reg "ldr" [anyreg; LitVal (x, int32 off)] in
              Index2 (r_fp, reg_of r, 0)
            end
        | <OFFSET, t1, <CONST n>> when fits_offset shamt (Int32.to_int n) ->
            let v1 = eval_reg t1 anyreg in
            Index (reg_of v1, nosym, Int32.to_int n)
        | <OFFSET, t1, <SYMBOL (x, n)>>
                when fits_offset shamt (x.a_val + n) ->
            let v1 = eval_reg t1 anyreg in
            Index (reg_of v1, x, n)
        | <OFFSET, t1, <BINOP Lsl, t2, <CONST n>>> when n = int32 shamt ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_reg t2 anyreg in
            Index2 (reg_of v1, reg_of v2, Int32.to_int n)
        | <OFFSET, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_reg t2 anyreg in
            Index2 (reg_of v1, reg_of v2, 0)
        | t ->
            let v1 = eval_reg t anyreg in
            Index (reg_of v1, nosym, 0)

    (* |eval_call| -- execute procedure call *)
    let eval_call =
      function
          <GLOBAL f> ->
            gen "bl" [Global f]
        | <LIBFUN f> -> 
            gen "bl" [Global (symbol f)]
        | t -> 
            let v1 = eval_reg t anyreg in
            gen "blr" [v1]

    let statlink = ref R_none

    (* |tran_stmt| -- generate code to execute a statement *)
    let tran_stmt t =
      (* Conditional jump *)
      let condj op lab t1 t2 =
        let v1 = eval_reg t1 anyreg in
        let v2 = eval_rand t2 fits_add in
        gen "cmp" [v1; v2];
        gen op [Label lab] in

      (* Procedure call *)
      let call k t =
        spill_temps move_reg volatile;     (* Spill any remaining temps *)
        eval_call t;                       (* Call the function *)
        for i = 0 to min 7 (k-1) do        (* Release argument registers *)
          release_reg (Reg i)
        done;
        release_reg !statlink;
        statlink := R_none in

      match t with
          <CALL k, t1> -> 
            call k t1

        | <DEFTEMP n, <(CALLW k|CALLQ k), t1>> ->
            call k t1;
            reserve_reg (Reg 0); 
            Alloc.def_temp n (Reg 0)

        | <DEFTEMP n, t1> ->
            let r = temp_reg n in
            if r = R_none then begin
              let v1 = eval_reg t1 anytemp in
              Alloc.def_temp n (reg_of v1)
            end else begin
              (* A short-circuit condition: assume no spills *)
              let v1 = eval_reg t1 (reg r) in
              release v1
            end

        | <(STOREW|STOREC), t1, <REGVAR i>> ->
            let rv = regvar i in
            release (eval_reg t1 (reg32 rv))
        | <STOREQ, t1, <REGVAR i>> ->
            let rv = regvar i in
            release (eval_reg t1 (reg64 rv))
        | <STOREW, t1, t2> -> 
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr 2 t2 in
            gen "str" [v1; v2]
        | <STOREC, t1, t2> -> 
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr 0 t2 in
            gen "strb" [v1; v2]
        | <STOREQ, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr 3 t2 in
            gen "str" [v1; v2]

        | <(RESULTW|RESULTQ), t1> ->
            release (eval_reg t1 (reg (Reg 0)))

        | <LABEL lab> -> emit_lab lab

        | <JUMP lab> -> gen "b" [Label lab]

        | <JUMPC (Eq, lab), t1, <CONST z>> when z = Int32.zero ->
            let v1 = eval_reg t1 anyreg in
            gen "cbz" [v1; Label lab]
        | <JUMPC (Neq, lab), t1, <CONST z>> when z = Int32.zero ->
            let v1 = eval_reg t1 anyreg in
            gen "cbnz" [v1; Label lab]
        | <JUMPC (EqA, lab), t1, <NIL>> ->
            let v1 = eval_reg t1 anyreg in
            gen "cbz" [v1; Label lab]
        | <JUMPC (NeqA, lab), t1, <NIL>> ->
            let v1 = eval_reg t1 anyreg in
            gen "cbnz" [v1; Label lab]
        | <JUMPC (Eq, lab), t1, t2> -> condj "beq" lab t1 t2
        | <JUMPC (Lt, lab), t1, t2> -> condj "blt" lab t1 t2
        | <JUMPC (Gt, lab), t1, t2> -> condj "bgt" lab t1 t2
        | <JUMPC (Leq, lab), t1, t2> -> condj "ble" lab t1 t2
        | <JUMPC (Geq, lab), t1, t2> -> condj "bge" lab t1 t2
        | <JUMPC (Neq, lab), t1, t2> -> condj "bne" lab t1 t2
        | <JUMPC (EqA, lab), t1, t2> -> condj "beq" lab t1 t2
        | <JUMPC (NeqA, lab), t1, t2> -> condj "bne" lab t1 t2

        | <JCASE (table, deflab), t1> ->
            let tbl = label () in
            let v1 = eval_reg t1 anyreg in
            emit "cmp" [v1; const (int32 (List.length table))];
            gen "bhs" [Label deflab];
            emit "adr" [reg64 r_ip0; Label tbl];
            gen "ldr" [reg64 r_ip0; Index2 (r_ip0, reg_of v1, 3)];
            emit "br" [reg64 r_ip0];
            emit ".p2align 3" [];
            emit_lab tbl;
            List.iter (fun lab -> emit ".quad" [Label lab]) table

        | <ARG i, <(TEMPW k|TEMPQ k)>> when i < 8 ->
            (* Avoid annoying spill and reload if the value is a temp
               already in the correct register: e.g. in f(g(x)). *)
            let r = Reg i in
            let r1 = Alloc.use_temp k in
            spill_temps move_reg [r];
            ignore (gen_move64 [reg64 r; reg64 r1])
        | <ARG i, t1> when i < 8 ->
            let r = Reg i in
            spill_temps move_reg [r];
            ignore (eval_reg t1 (reg r))
        | <ARG i, t1> when i >= 8 ->
            let v1 = eval_reg t1 anyreg in
            gen "str" [v1; Index (r_sp, nosym, 8*i-64)]

        | <STATLINK, t1> ->
            let r = Reg 19 in
            spill_temps move_reg [r];
            ignore (eval_reg t1 (reg64 r));
            statlink := r

        | <w, @ts> -> 
            failwith (sprintf "tran_stmt $" [fInst w])
  end (* Selector *)

  let options = []
end (* ARM64 *)

module Compiler = Main.F(ARM64)
