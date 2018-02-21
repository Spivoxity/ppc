(* ppcu/risc86.mli *)
(* Copyright (c) 2017--18 J. M. Spivey *)

open Target
open Print
open Optree

module RISC86 = struct
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

            arg n
            ...
    20      arg 1
            ----------------
    16      return address
    12      saved ebx
     8      saved esi
     4      saved edi
    bp:     dynamic link
    -4      static link
            ----------------
    -8      local 1
            ...
            local m
    *)

    let param_base = 20
    let local_base lev = if lev > 1 then -4 else 0
    let stat_link = -4
    let nregvars = 0
    let share_globals = false

    let reg_names =
      [| "%0"; "%1"; "%2"; "%3"; "%4"; "%5"; "%bp"; "%sp" |]

    let r_bp = Reg 6
    let r_sp = Reg 7

    let volatile = [Reg 0; Reg 1; Reg 2]
    let stable = [Reg 3; Reg 4; Reg 5]
  end

  module Alloc = Regs.AllocF(Metrics)

  open Metrics
  open Alloc

  module Emitter = struct
    (* |operand| -- type of operands for assembly instructions *)
    type operand =                  (* VALUE              RISC86 SYNTAX    *)
        Const of int                (* val                val              *)
      | Register of reg             (* [reg]              reg              *)
      | Offset of int * reg         (* val+[reg]<<s       val(reg)         *)
      | Index of symbol * int * reg * int 
                                    (* lab+val+[reg]<<s   lab+val(reg*scl) *)
      | Index2 of reg * reg * int   (* [r1]+[r2]<<s       (r1+r2*scl)      *)
      | Global of symbol * int      (* lab+val            lab+val          *)
      | Label of codelab            (* lab                lab              *)

    let anyreg = Register R_any
    let anytemp = Register R_temp

    let map_regs f =
      function
          Const n -> Const n
        | Register r -> Register (f r)
        | Offset (n, r) -> Offset (n, f r)
        | Index (x, n, r, s) -> Index (x, n, f r, s)
        | Index2 (r1, r2, s) -> Index2 (f r1, f r2, s)
        | Global (x, n) -> Global (x, n)
        | Label lab -> Label lab

    let reg_of =
      function
          Register r -> r
        | _ -> failwith "reg_of"

    let use_reg _ = ()

    let fBase lab off =
      if off = 0 then fStr lab 
      else if off > 0 then fMeta "$+$" [fStr lab; fNum off]
      else fMeta "$-$" [fStr lab; fNum (-off)]

    let fScaled reg s =
      if s = 0 then fReg reg else 
        fMeta "$*$" [fReg reg; fNum (1 lsl s)]

    (* |fRand| -- format operand for printing *)
    let fRand =
      function
          Const v -> fNum v
        | Register reg -> fReg reg
        | Offset (n, reg) -> 
            if n = 0 then fMeta "($)" [fReg reg]
            else fMeta "$($)" [fNum n; fReg reg]
        | Index (lab, off, reg, s) ->
            fMeta "$($)" [fBase lab off; fScaled reg s]
        | Index2 (r1, r2, s) ->
            fMeta "($+$)" [fReg r1; fScaled r2 s]
        | Global (lab, off) -> fBase lab off
        | Label lab -> fMeta ".$" [fLab lab]

    (* |preamble| -- emit start of assembler file *)
    let preamble () =
      printf "! picoPascal compiler output\n" [];
      printf "\t.global pmain\n\n" []

    (* |postamble| -- finish the assembler file *)
    let postamble () =
      printf "! End\n" []

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

    let put_jumptab lab table =
      segment RoData;
      printf "\t.align 4\n" [];
      printf "$:\n" [fStr lab];
      List.iter (fun l -> printf "\t.long .$\n" [fLab l]) table

    (* |put_global| -- output a global variable *)
    let put_global lab n =
      printf "\t.comm $, $, 4\n" [fStr lab; fNum n]

    let proclab = ref nosym
    let level = ref 0
    let frame = ref 0

    (* |start_proc| -- emit start of procedure *)
    let start_proc lab lev nargs fsize =
      proclab := lab; level := lev; frame := fsize

    let prelude () =
      segment Text;
      printf "$:\n" [fStr !proclab];
      printf "\tprolog\n" [];
      if !level > 0 then
        printf "\tpush %0\n" []; (* Save static link *)
      if !frame > 0 then
        printf "\tsub %sp, %sp, $\n" [fNum !frame]

    let postlude () =
      printf "\tepilog\n" [];
      printf "\tret\n" []

    let put_inst op rands =
      match rands with
          [] -> printf "\t$\n" [fStr op]
        | _ ->  printf "\t$ $\n" [fStr op; fList(fRand) rands]

    let put_label lab =
      printf ".$:\n" [fLab lab]

    let comment = "! "
  end (* Emitter *)

  module Selector(IQueue : Target.IQueueT
      with type operand = Emitter.operand) = struct
    open Emitter
    open IQueue

    let move_reg dst src =
      ignore (gen_reg "mov" [Register dst; Register src])

    (* |eval_reg| -- evaluate expression with result in a register *)
    let rec eval_reg t r =
      match t with
          <CONST k> ->
            gen_reg "mov" [r; Const k]
        | <NIL> ->
            gen_reg "mov" [r; Const 0]
        | <LOCAL 0> ->
            gen_move "mov" [r; Register r_bp]
        | <LOCAL n> ->
              gen_reg "add" [r; Register r_bp; Const n]
        | <GLOBAL x> ->
            gen_reg "mov" [r; Global (x, 0)]
        | <TEMPW n> ->
            gen_move "mov" [r; Register (Alloc.use_temp n)]
        | <LOADW, t1> ->
            let v1 = eval_addr t1 in
            gen_reg "ldw" [r; v1]
        | <LOADC, t1> ->
            let v1 = eval_addr t1 in
            gen_reg "ldc" [r; v1]

        | <MONOP Plus, t1> ->
            eval_reg t1 r
        | <MONOP Uminus, t1> ->
            let v1 = eval_reg t1 anyreg in
            gen_reg "neg" [r; v1]
        | <MONOP Not, t1> ->
            let v1 = eval_reg t1 anyreg in
            gen_reg "xor" [r; v1; Const 1]
        | <MONOP BitNot, t1> ->
            let v1 = eval_reg t1 anyreg in
            gen_reg "not" [r; v1]

        | <OFFSET, <GLOBAL x>, t2> ->
            let v2 = eval_reg t2 anyreg in
            gen_reg "add" [r; v2; Global (x, 0)]
        | <OFFSET, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_rand t2 in
            gen_reg "add" [r; v1; v2]

        | <BINOP Plus, t1, t2> -> binary "add" r t1 t2
        | <BINOP Minus, t1, t2> -> binary "sub" r t1 t2
        | <BINOP Times, t1, t2> -> binary "mul" r t1 t2
        | <BINOP And, t1, t2> -> binary "and" r t1 t2
        | <BINOP Or, t1, t2> -> binary "or" r t1 t2
        | <BINOP Eq, t1, t2> -> binary "seteq" r t1 t2
        | <BINOP Neq, t1, t2> -> binary "setne" r t1 t2
        | <BINOP Gt, t1, t2> -> binary "setgt" r t1 t2
        | <BINOP Geq, t1, t2> -> binary "setge" r t1 t2
        | <BINOP Lt, t1, t2> -> binary "setlt" r t1 t2
        | <BINOP Leq, t1, t2> -> binary "setle" r t1 t2
        | <BINOP Lsl, t1, t2> -> binary "shl" r t1 t2
        | <BINOP Lsr, t1, t2> -> binary "shr" r t1 t2
        | <BINOP Asr, t1, t2> -> binary "sar" r t1 t2
        | <BINOP BitAnd, t1, t2> -> binary "and" r t1 t2
        | <BINOP BitOr, t1, t2> -> binary "or" r t1 t2
        | <w, @args> ->
            failwith (sprintf "eval $" [fInst w])

    and binary op r t1 t2 =
      let v1 = eval_reg t1 anyreg in
      let v2 = eval_rand t2 in
      gen_reg op [r; v1; v2]

    (* |eval_rand| -- evaluate expression to register or as constant *)
    and eval_rand =
      function
          <CONST k> -> Const k
        | <NIL> -> Const 0
        | <GLOBAL x> -> Global (x, 0)
        | t -> eval_reg t anyreg

    (* |eval_addr| -- evaluate expression to address value *)
    and eval_addr =
      function
          <LOCAL n> -> Offset (n, r_bp)
        | <GLOBAL x> | <LIBFUN x> -> Global (x, 0)
        | <OFFSET, <GLOBAL x>, <CONST n>> -> Global (x, n)
        | <OFFSET, <GLOBAL x>, <BINOP Lsl, t2, <CONST s>>> when s <= 3 ->
            let v2 = eval_reg t2 anyreg in
            Index (x, 0, reg_of v2, s)
        | <OFFSET, <GLOBAL x>, t2> ->
            let v2 = eval_reg t2 anyreg in
            Index (x, 0, reg_of v2, 0)
        | <OFFSET, t1, <CONST n>> ->
            let v1 = eval_reg t1 anyreg in
            Offset (n, reg_of v1)
        | <OFFSET, t1, <BINOP Lsl, t2, <CONST s>>> when s <= 3 ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_reg t2 anyreg in
            Index2 (reg_of v1, reg_of v2, s)
        | <OFFSET, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_reg t2 anyreg in
            Index2 (reg_of v1, reg_of v2, 0)
        | t ->
            let v1 = eval_reg t anyreg in
            Offset (0, reg_of v1)

    (* |branch_op| -- determine branch instruction for comparison operator *)
    let branch_op = 
      function Eq -> "beq" | Lt -> "blt" | Gt -> "bgt" 
        | Leq -> "ble" | Geq -> "bge" | Neq -> "bne" 
        | _ -> failwith "bad comparison"

    let statlink = ref R_none

    let call n t1 =
      Alloc.spill_temps move_reg volatile;
      let v1 = eval_addr t1 in
      release_reg !statlink;
      gen "call" [v1];
      if (n > 0) then
        gen "add" [Register r_sp; Register r_sp; Const (4*n)];
      statlink := R_none

    (* |tran_stmt| -- generate code to execute a statement *)
    let tran_stmt =
      function
          <CALL k, t1> -> call k t1

        | <DEFTEMP n, <CALLW k, t1>> ->
            call k t1;
            reserve_reg (Reg 0);
            Alloc.def_temp n (Reg 0)

        | <DEFTEMP n, t1> ->
            let v1 = eval_reg t1 anytemp in
            Alloc.def_temp n (reg_of v1)
        | <STOREW, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr t2 in
            gen "stw" [v1; v2]
        | <STOREC, t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_addr t2 in
            gen "stc" [v1; v2]
        | <RESULTW, t1> ->
            release (eval_reg t1 (Register (Reg 0)))
        | <LABEL lab> ->
            emit_lab lab
        | <JUMP lab> ->
            gen "jmp" [Label lab]
        | <JUMPC (w, lab), t1, t2> ->
            let v1 = eval_reg t1 anyreg in
            let v2 = eval_rand t2 in
            gen (branch_op w) [v1; v2; Label lab]
        | <JCASE (table, deflab), t1> ->
            let n = List.length table in
            let lab1 = gensym () in
            let v1 = eval_reg t1 anyreg in
            emit "bae" [v1; Const n; Label deflab];
            let v2 = gen_reg "ldw" [anyreg; Index (lab1, 0, reg_of v1, 2)] in
            gen "jmp" [Offset (0, reg_of v2)];
            put_jumptab lab1 table;

        | <ARG i, t1> ->
            let v1 = eval_rand t1 in
            gen "push" [v1]

        | <STATLINK, t1> ->
            let r = Reg 0 in
            Alloc.spill_temps move_reg [r];
            ignore (eval_reg t1 (Register r));
            statlink := r

        | <w, @ts> -> 
            failwith (sprintf "tran_stmt $" [fInst w])
  end (* Selector *)

  let options = []
end (* RISC86 *)

module Compiler = Main.F(RISC86)
