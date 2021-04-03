(* ppcu/arm.ml *)
(* Copyright (c) 2017--18 J. M. Spivey *)

(* Code generator for ARM *)

open Target
open Print
open Optree

let zero = Int32.zero
let one = Int32.one
let int32 n = Int32.of_int n

module ARM = struct
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
     +40  arg 0  /
          ----------------
     +36  return address
     +32  saved sp
     +28  dynamic link
     +24  saved r10
     +20  saved r9
          ...
      +4  saved r5
    fp:	  saved r4 (static link)
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

    let param_base = 40
    let local_base lev = 0
    let local_align = 1
    let stat_link = 0
    let nregvars = 3
    let share_globals = true
    let sharing = 2
    let fixed_frame = false

    (* ARM register assignments:

       R0-3   arguments + scratch
       R4     static link
       R5-R10 callee-save temps
       R11=fp frame pointer
       R12=ip temp for linkage
       R13=sp stack pointer
       R14=lr link register
       R15=pc program counter 

    *)

    let reg_names =
      [| "r0"; "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7";
          "r8"; "r9"; "r10"; "fp"; "ip"; "sp"; "lr"; "pc" |]

    let reg i = Reg i
    let r_fp = Reg 11
    let r_ip = Reg 12
    let r_sp = Reg 13
    let r_lr = Reg 14
    let r_pc = Reg 15

    let volatile = [reg 0; reg 1; reg 2; reg 3; reg 4]
    let stable = [reg 5; reg 6; reg 7; reg 8; reg 9; reg 10]
  end

  module Alloc = Regs.AllocF(Metrics)

  open Metrics
  open Alloc

  module Emitter = struct
    (* |operand| -- type of operands for assembly instructions *)
    type operand =		  (* VALUE	  ASM SYNTAX       *)
        Const of symbol * int32   (* lab+val	  #lab+val         *)
      | Register of reg		  (* [reg]	  reg	           *)
      | Shift of reg * int        (* [reg]<<n     reg, LSL #n      *)
      | Index of reg * symbol * int  (* [reg]+val    [reg, #val]   *)
      | Index2 of reg * reg * int (* [r1]+[r2]<<n [r1, r2, LSL #n] *)
      | Global of symbol 	  (* lab	  lab	           *)
      | Label of codelab	  (* lab	  lab              *)
      | LitSym of symbol          (* lab          =lab             *)
      | LitVal of symbol * int32  (* lab+val	  =lab+val         *)

    let anyreg = Register R_any
    let anytemp = Register R_temp

    let map_regs f =
      function
          Const (x, n) -> Const (x, n)
        | Register r -> Register (f r)
        | Shift (r, n) -> Shift (f r, n)
        | Index (r, x, n) -> Index (f r, x, n)
        | Index2 (r1, r2, n) -> Index2 (f r1, f r2, n)
        | Global x -> Global x
        | Label lab -> Label lab
        | LitSym x -> LitSym x
        | LitVal (x, n) -> LitVal (x, n)

    let const n = Const (nosym, n)
    let literal n = LitVal (nosym, n)

    let reg_of =
      function
          Register r -> r
        | _ -> failwith "reg_of"

    let sym_value x n = Int32.add (int32 x.a_val) n
    
    (* |fRand| -- format operand for printing *)
    let fRand =
      function
          Const (x, n) ->
            fMeta "#$" [fNum32 (sym_value x n)]
        | Register reg -> fReg reg
        | Shift (reg, n) ->
            fMeta "$, LSL #$" [fReg reg; fNum n]
        | Index (reg, sym, off) ->
            let n = sym_value sym (int32 off) in
            if n = zero then fMeta "[$]" [fReg reg]
            else fMeta "[$, #$]" [fReg reg; fNum32 n]
        | Index2 (r1, r2, n) ->
            if n = 0 then
              fMeta "[$, $]" [fReg r1; fReg r2]
            else
              fMeta "[$, $, LSL #$]" [fReg r1; fReg r2; fNum n]
        | Global lab -> fSym lab
        | Label lab -> fMeta ".$" [fLab lab]
        | LitSym x -> fMeta "=$" [fSym x]
        | LitVal (x, n) -> fMeta "=$" [fNum32 (sym_value x n)]

    (* |preamble| -- emit start of assembler file *)
    let preamble () =
      printf "@ picoPascal compiler output\n" [];
      printf "\t.global pmain\n\n" []

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
    let max_reg = ref 4  (* Always save r4 *)

    let use_reg =
      function
          Reg i ->
            if i < 11 && !max_reg < i then begin
              max_reg := i;
              if !max_reg mod 2 == 1 then incr max_reg
            end
        | _ -> failwith "use_reg"

    let param_offset () =
       (* Save r4 .. rmax plus fp, sp, lr *)
       4 * (!max_reg - 3) + 12

    (* |start_proc| -- emit start of procedure *)
    let start_proc lab lev nargs fram =
      frame := fram; max_reg := 4;

      segment Text;
      printf "$:\n" [fSym lab];
      printf "\tmov ip, sp\n" [];
      if nargs > 2 then
        printf "\tstmfd sp!, {r0-r3}\n" []
      else if nargs > 0 then
        printf "\tstmfd sp!, {r0-r1}\n" []

    let prelude () =
      let nout = Alloc.outgoing () in
      let stack = max (4*nout - 16) 0 in
      (* Round up frame space for stack alignment *)
      let space = 8 * ((!frame + stack + 7)/8) in

      if !max_reg = 4 then
        printf "\tstmfd sp!, {r4, fp, ip, lr}\n" []
      else
        printf "\tstmfd sp!, {r4-r$, fp, ip, lr}\n" [fNum !max_reg];
      printf "\tmov fp, sp\n" [];
      if space <= 1024 then
        (* Since space is a multiple of 8, we can fit values up to 1024 *)
        (if space > 0 then printf "\tsub sp, sp, #$\n" [fNum space])
      else begin
        printf "\tldr ip, =$\n" [fNum space];
        printf "\tsub sp, sp, ip\n" []
      end

    let postlude () =
      if !max_reg = 4 then
        printf "\tldmfd fp, {r4, fp, sp, pc}\n" []
      else
        printf "\tldmfd fp, {r4-r$, fp, sp, pc}\n" [fNum !max_reg];
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
      ignore (gen_reg "mov" [Register dst; Register src])

    (* Tests for fitting in various immediate fields *)

    (* |fits_offset| -- test for fitting in offset field of address *)
    let fits_offset x = (int32 (-4096) < x && x < int32 4096)

    (* |fits_immed| -- test for fitting in immediate field *)
    let fits_immed x =
      (* A conservative approximation, using shifts instead of rotates *)
      let rec reduce r =
        if Int32.logand r (int32 3) <> zero then r
        else reduce (Int32.shift_right_logical r 2) in
      x = zero || x > zero && reduce x < int32 256

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
        gen_reg op [r; v1; v2]

      (* Unary operation *)
      and unary op t1 =
        let v1 = eval_reg t1 anyreg in
        gen_reg op [r; v1]

      (* Comparison with boolean result *)
      and compare op t1 t2 =
        let v1 = eval_reg t1 anyreg in
        let v2 = eval_rand t2 fits_add in
        gen "cmp" [v1; v2];
        let v = gen_reg "mov" [r; const zero] in
        emit op [v; const one];
        v in

      match t with
          <CONST k> when fits_move k -> 
            gen_reg "mov" [r; const k]
        | <CONST k> ->
            gen_reg "ldr" [r; literal k]
        | <SYMBOL (x, n)> ->
            gen_reg "ldr" [r; LitVal (x, int32 n)]
        | <NIL> ->
            gen_reg "mov" [r; const zero]
        | <LOCAL (x, 0)> when is_none x ->
            gen_move "mov" [r; Register r_fp]
        | <LOCAL (x, n)> when fits_add (int32 (x.a_val + n)) ->
            gen_reg "add" [r; Register r_fp; Const (x, int32 n)]
        | <LOCAL (x, n)> ->
            emit "ldr" [Register r_ip; LitVal (x, int32 n)];
            gen_reg "add" [r; Register r_fp; Register r_ip]
        | <GLOBAL x> ->
            gen_reg "ldr" [r; LitSym x]
        | <TEMPW n> ->
            gen_move "mov" [r; Register (Alloc.use_temp n)]
        | <(LOADW|LOADC), <REGVAR i>> ->
            let rv = regvar i in
            reserve_reg rv; gen_move "mov" [r; Register rv]
        | <LOADW, t1> -> 
            let v1 = eval_addr t1 in
            gen_reg "ldr" [r; v1]
        | <LOADC, t1> -> 
            let v1 = eval_addr t1 in
            gen_reg "ldrb" [r; v1]

        | <MONOP Uminus, t1> -> unary "neg" t1
        | <MONOP Not, t1> -> 
            let v1 = eval_reg t1 anyreg in
            gen_reg "eor" [r; v1; const one]
        | <MONOP BitNot, t1> -> unary "mvn" t1

        | <OFFSET, t1, <CONST n>> when fits_add n ->
            (* Allow add for negative constants *)
            let v1 = eval_reg t1 anyreg in
            gen_reg "add" [r; v1; const n]
        | <OFFSET, t1, t2> -> binary "add" t1 t2

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
            gen_reg "mul" [r; v1; v2]

        | <BINOP Eq, t1, t2> -> compare "moveq" t1 t2
        | <BINOP Neq, t1, t2> -> compare "movne" t1 t2
        | <BINOP Gt, t1, t2> -> compare "movgt" t1 t2
        | <BINOP Geq, t1, t2> -> compare "movge" t1 t2
        | <BINOP Lt, t1, t2> -> compare "movlt" t1 t2
        | <BINOP Leq, t1, t2> -> compare "movle" t1 t2

        | <BOUND, t1, t2> ->
            let v1 = eval_reg t1 r in
            let v2 = eval_rand t2 fits_add in
            release v2;
            emit "cmp" [v1; v2];
            emit "ldrhs" [Register (reg 0); literal (int32 !line)];
            emit "blhs" [Global (symbol "check")];
            v1

        | <NCHECK, t1> ->
            let v1 = eval_reg t1 r in
            emit "cmp" [v1; const zero];
            emit "ldreq" [Register (reg 0); literal (int32 !line)];
            emit "bleq" [Global (symbol "nullcheck")];
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
    and eval_addr =
      (* returns |Index| or |Index2| *)
      function
          <LOCAL (x, n)> when fits_offset (int32 (x.a_val + n)) ->
            Index (r_fp, x, n) 
        | <LOCAL (x, n)> ->
            let r = gen_reg "ldr" [anyreg; LitVal (x, int32 n)] in
            Index2 (r_fp, reg_of r, 0)
        | <OFFSET, t1, <CONST n>> when fits_offset n ->
            let v1 = eval_reg t1 anyreg in
            Index (reg_of v1, nosym, Int32.to_int n)
        | <OFFSET, t1, <SYMBOL (x, n)>>
                when fits_offset (int32 (x.a_val + n)) ->
            let v1 = eval_reg t1 anyreg in
            Index (reg_of v1, x, n)
        | <OFFSET, t1, <BINOP Lsl, t2, <CONST n>>> when n < int32 32 ->
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
            gen "blx" [v1]

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
        | <STOREW, t1, t2> -> 
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr t2 in
            gen "str" [v1; v2]
        | <STOREC, t1, t2> -> 
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr t2 in
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
            (* This jump table code exploits the fact that on ARM, reading
               the pc gives a value 8 bytes beyond the current instruction,
               so in the ldrlo instruction below, pc points to the branch
               table itself. *)
            let v1 = eval_reg t1 anyreg in
            emit "cmp" [v1; const (int32 (List.length table))];
            gen "ldrlo" [Register r_pc; Index2 (r_pc, reg_of v1, 2)];
            gen "b" [Label deflab];
            List.iter (fun lab -> emit ".word" [Label lab]) table

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
            gen "str" [v1; Index (r_sp, nosym, 4*i-16)]

        | <STATLINK, t1> ->
            let r = reg 4 in
            spill_temps move_reg [r];
            ignore (eval_reg t1 (Register r));
            statlink := r

        | <w, @ts> -> 
            failwith (sprintf "tran_stmt $" [fInst w])
  end (* Selector *)

  let options = []
end (* ARM *)

module Compiler = Main.F(ARM)
