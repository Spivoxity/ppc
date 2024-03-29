(* ppcu/treegen.ml *)
(* Copyright (c) 2017--18 J. M. Spivey *)

open Dict
open Tree
open Optree
open Lexer
open Print
open Target

let debug = Coder.debug
let boundchk = ref false

module F(Targ : Target.T) = struct
  module Metrics = Targ.Metrics
  module Emitter = Targ.Emitter
  module Alloc = Targ.Alloc

  open Metrics

  (* |level| -- nesting level of current procedure *)
  let level = ref 0

  (* |retlab| -- label to return from current procedure *)
  let retlab = ref nolab

  (* |size_of| -- calculate size of type *)
  let size_of t = t.t_rep.r_size

  (* |get_value| -- get constant value or fail *)
  let get_value e =
    match e.e_value with
        Some v -> v
      | None -> failwith "get_value"

  (* |line_number| -- compute line number of variable for bound check *)
  let rec line_number v =
    match v.e_guts with
        Variable x -> x.x_line
      | Sub (a, i) -> line_number a
      | Select (r, x) -> x.x_line
      | Deref p -> line_number p
      | _ -> failwith "line_number"

  (* |addr_size| -- size of address *)
  let addr_size = Metrics.addr_rep.r_size

  (* choose -- choose instruction by size *)
  let choose e i8 i32 i64 =
    match size_of e.e_type with
        1 -> i8 | 4 -> i32 | 8 -> i64
      | _ -> failwith "choose"

  let load_addr =
    match addr_size with
      4 -> LOADW | 8 -> LOADQ | _ -> failwith "load_addr"

  let callop n result =
    match result.r_size with
        0 -> CALL n | (1 | 4) -> CALLW n | 8 -> CALLQ n
      | _ -> failwith "callop"

  let const n = <CONST (Int32.of_int n)>

  (* |schain| -- code to follow N links of static chain *)
  let rec schain n =
    if n = 0 then
      <LOCAL (nosym, 0)>
    else
      <load_addr,
        <OFFSET, schain (n-1), const Metrics.stat_link>>

  (* |address| -- code to push address of an object *)
  let address d =
    match d.d_addr with
        Symbol x -> 
          if d.d_level = 0 then
            <GLOBAL x>
          else
            <OFFSET, schain (!level - d.d_level), <SYMBOL (x, 0)>>
      | Absolute addr ->
          <CONST addr>
      | Register i ->
          <REGVAR i>
      | Nowhere -> 
          failwith (sprintf "address $" [fId d.d_tag])

  (* |gen_closure| -- two trees for a (code, envt) pair *)
  let gen_closure d =
    match d.d_kind with
        ProcDef ->
          (<GLOBAL (symbol_of d)>,
            if d.d_level = 0 then <NIL> else schain (!level - d.d_level))
      | PParamDef ->
          (<load_addr, address d>,
            <load_addr, <OFFSET, address d, const addr_size>>)
      | _ -> failwith "missing closure"

  let rec numargs i =
    function
        [] -> []
      | (x::xs) -> <ARG i, x> :: numargs (i+1) xs

  (* |libcall| -- code for library call *)
  let libcall lab args result =
    let n = List.length args in
    let op = callop n result in
    <op, @(<LIBFUN lab> :: numargs 0 args)>

  (* |gen_copy| -- generate code to copy a fixed-size chunk *)
  let gen_copy dst src n =
    libcall "memcpy" [dst; src; const n] void_rep

  (* |const_val| -- get value of constant or raise Not_found *)
  let const_val e =
    match e.e_value with Some v -> v | None -> raise Not_found

  (* |gen_addr| -- code for the address of a variable *)
  let rec gen_addr v = 
    match v.e_guts with
        Variable x ->
          let d = get_def x in
          begin
            match d.d_kind with
                VarDef ->
                  address d
              | VParamDef ->
                  <load_addr, address d>
              | CParamDef ->
                  if scalar d.d_type || is_pointer d.d_type then 
                    address d
                  else
                    <load_addr, address d>
              | StringDef ->
                  <GLOBAL (symbol_of d)>
              | _ -> 
                  failwith "load_addr"
          end
      | Sub (a, i) ->
          let bound_check t =
            if not !boundchk then t else <BOUND, t, const (bound a.e_type)> in
          <OFFSET, 
            gen_addr a,
            <BINOP Times,
              bound_check (gen_expr i),
              const (size_of v.e_type)>>
      | Select (r, x) ->
          let y = symbol_of (get_def x) in
          <OFFSET, gen_addr r, <SYMBOL (y, 0)>>
      | Deref p ->
          let null_check t =
            if not !boundchk then t else <NCHECK, t> in
          null_check (gen_expr p)
      | String (lab, n) -> <GLOBAL lab>
      | _ -> failwith "gen_addr"

  (* |gen_expr| -- tree for the value of an expression *)
  and gen_expr e =
    match e.e_value with
        Some v -> 
          <CONST v>
      | None -> 
          begin
            match e.e_guts with
                Variable _ | Sub _ | Select _ | Deref _ ->
                  let ld = choose e LOADC LOADW LOADQ in
                  <ld, gen_addr e>
              | Nil ->
                  <NIL>
              | Monop (w, e1) ->
                  <MONOP w, gen_expr e1>
              | Binop (Div, e1, e2) ->
                  begin try
                    let k = Util.exact_log2 (const_val e2) in
                    <BINOP Asr, gen_expr e1, const k>
                  with Not_found ->
                    libcall "int_div" [gen_expr e1; gen_expr e2] int_rep
                  end
              | Binop (Mod, e1, e2) ->
                  begin try
                    let k = Util.exact_log2 (const_val e2) in
                    <BINOP BitAnd, gen_expr e1,
                      <CONST (Int32.pred (Int32.shift_left Int32.one k))>>
                  with Not_found ->
                    libcall "int_mod" [gen_expr e1; gen_expr e2] int_rep
                  end
              | Binop (Eq, e1, e2) ->
                  let eq = choose e1 Eq Eq EqA in
                  <BINOP eq, gen_expr e1, gen_expr e2>
              | Binop (Neq, e1, e2) ->
                  let neq = choose e1 Neq Neq NeqA in
                  <BINOP neq, gen_expr e1, gen_expr e2>
              | Binop (And, e1, e2) ->
                  let lab1 = label () and lab2 = label () and lab3 = label () in
                  let t = Alloc.new_temp 1 in
                  <AFTER,
                    <SEQ, gen_cond e1 lab1 lab2,
                      <LABEL lab1>, <DEFTEMP t, gen_expr e2>, <JUMP lab3>,
                      <LABEL lab2>, <DEFTEMP t, const 0>,
                      <LABEL lab3>>,
                    <TEMPW t>>
              | Binop (Or, e1, e2) ->
                  let lab1 = label () and lab2 = label () and lab3 = label () in
                  let t = Alloc.new_temp 1 in
                  <AFTER,
                    <SEQ, gen_cond e1 lab1 lab2,
                      <LABEL lab1>, <DEFTEMP t, const 1>, <JUMP lab3>,
                      <LABEL lab2>, <DEFTEMP t, gen_expr e2>,
                      <LABEL lab3>>,
                    <TEMPW t>>
              | Binop (w, e1, e2) ->
                  <BINOP w, gen_expr e1, gen_expr e2>
              | FuncCall (p, args) -> 
                  gen_call p args
              | _ -> failwith "gen_expr"
          end

  (* |gen_call| -- generate code to call a procedure *)
  and gen_call x args =
    let d = get_def x in
    match d.d_kind with
        LibDef q ->
          gen_libcall q args
      | _ ->
          let p = get_proc d.d_type in
          let (fn, sl) = gen_closure d in
          let args = List.concat (List.map2 gen_arg p.p_fparams args) in
          let op = callop p.p_pcount p.p_result.t_rep in
          <op, @(fn :: <STATLINK, sl> :: numargs 0 args)>

  (* |gen_arg| -- generate code for a procedure argument *)
  and gen_arg f a = 
    match f.d_kind with
        CParamDef ->
          if scalar f.d_type || is_pointer f.d_type then 
            [gen_expr a]
          else 
            [gen_addr a]
      | VParamDef ->
          [gen_addr a]
      | PParamDef ->
          begin
            match a.e_guts with 
                Variable x -> 
                  let (fn, sl) = gen_closure (get_def x) in [fn; sl]
              | _ -> 
                  failwith "bad funarg"
          end
      | _ -> failwith "bad arg"

  (* |gen_libcall| -- generate code to call a built-in procedure *)
  and gen_libcall q args =
    match (q.q_id, args) with
        (ChrFun, [e]) -> gen_expr e
      | (OrdFun, [e]) -> gen_expr e
      | (PrintString, [e]) ->
          libcall "print_string"
            [gen_addr e; const (bound e.e_type)] void_rep
      | (ReadChar, [e]) ->
          libcall "read_char" [gen_addr e] char_rep
      | (NewProc, [e]) ->
          let size = size_of (base_type e.e_type) in
          let store = choose e STOREW STOREW STOREQ in
          <store, libcall "new" [const size] addr_rep, gen_addr e>
      | (ArgcFun, []) ->
          libcall "argc" [] int_rep
      | (ArgvProc, [e1; e2]) ->
          libcall "argv" [gen_expr e1; gen_addr e2] void_rep
      | (OpenIn, [e]) ->
          libcall "open_in" [gen_addr e] void_rep
      | (Operator op, [e1]) ->
          <MONOP op, gen_expr e1>
      | (Operator op, [e1; e2]) ->
          <BINOP op, gen_expr e1, gen_expr e2>
      | (_, _) ->
          let proc = sprintf "$" [fLibId q.q_id] in
          libcall proc (List.map gen_expr args) void_rep

  (* |gen_cond| -- generate code to branch on a condition *)
  and gen_cond test tlab flab =
    match test.e_value with
        Some v ->
          if v <> Int32.zero then <JUMP tlab> else <JUMP flab>
      | None ->
          begin match test.e_guts with
              Monop (Not, e) ->
                gen_cond e flab tlab
            | Binop (Or, e1, e2) ->
                let l1 = label () in
                <SEQ,
                  gen_cond e1 tlab l1,
                  <LABEL l1>,
                  gen_cond e2 tlab flab>
            | Binop (And, e1, e2) ->
                let l1 = label () in
                <SEQ,
                  gen_cond e1 l1 flab,
                  <LABEL l1>,
                  gen_cond e2 tlab flab>
            | Binop (Eq, e1, e2) ->
                let eq = choose e1 Eq Eq EqA in
                <SEQ,
                  <JUMPC (eq, tlab), gen_expr e1, gen_expr e2>,
                  <JUMP flab>>
            | Binop (Neq, e1, e2) ->
                let neq = choose e1 Neq Neq NeqA in
                <SEQ,
                  <JUMPC (neq, tlab), gen_expr e1, gen_expr e2>,
                  <JUMP flab>>
            | Binop ((Lt | Leq | Gt | Geq) as w, e1, e2) ->
                <SEQ,
                  <JUMPC (w, tlab), gen_expr e1, gen_expr e2>,
                  <JUMP flab>>
            | _ ->
                <SEQ,
                  <JUMPC (Neq, tlab), gen_expr test, const 0>,
                  <JUMP flab>>
          end

  (* |gen_jtable| -- lay out jump table for case statement *)
  let gen_jtable sel table0 deflab =
    if table0 = [] then
      <JUMP deflab>
    else begin
      let table = List.sort (fun (v1, l1) (v2, l2) -> compare v1 v2) table0 in
      let lobound = fst (List.hd table) in
      let rec tab u qs =
        match qs with
            [] -> []
          | (v, l) :: rs -> 
              if u = v then l :: tab (Int32.succ v) rs
              else deflab :: tab (Int32.succ u) qs in
      <JCASE (tab lobound table, deflab),
        <BINOP Minus, sel, <CONST lobound>>>
    end

  (* |gen_stmt| -- generate code for a statement *)
  let rec gen_stmt s = 
    let code =
      match s.s_guts with
          Skip -> <NOP>

        | Seq ss -> <SEQ, @(List.map gen_stmt ss)>

        | Assign (v, e) ->
            if scalar v.e_type || is_pointer v.e_type then begin
              let st = choose e STOREC STOREW STOREQ in
              <st, gen_expr e, gen_addr v>
            end else begin
              gen_copy (gen_addr v) (gen_addr e) (size_of v.e_type)
            end

        | ProcCall (p, args) ->
            gen_call p args

        | Return res ->
            begin
              match res with
                  Some e ->
                    let res = choose e RESULTW RESULTW RESULTQ in
                    <SEQ, <res, gen_expr e>, <JUMP !retlab>>
                | None -> <JUMP !retlab>
            end

        | IfStmt (test, thenpt, elsept) ->
            let l1 = label () and l2 = label () and l3 = label() in
            <SEQ,
              gen_cond test l1 l2,
              <LABEL l1>,
              gen_stmt thenpt,
              <JUMP l3>,
              <LABEL l2>,
              gen_stmt elsept,
              <LABEL l3>>

        | WhileStmt (test, body) ->
            (* The test is at the top, improving the chances of finding
               common subexpressions between the test and loop body. *)
            let l1 = label () and l2 = label () and l3 = label() in
            <SEQ,
              <LABEL l1>,
              gen_cond test l2 l3,
              <LABEL l2>,
              gen_stmt body,
              <JUMP l1>,
              <LABEL l3>>

        | RepeatStmt (body, test) ->
            let l1 = label () and l2 = label () in
            <SEQ,
              <LABEL l1>,
              gen_stmt body, 
              gen_cond test l2 l1,
              <LABEL l2>>

        | ForStmt (var, lo, hi, body, upb) ->
            (* Use previously allocated temp variable to store upper bound.
               We could avoid this if the upper bound is constant. *)
            let tmp = match !upb with Some d -> d | _ -> failwith "for" in
            let l1 = label () and l2 = label () in
            <SEQ,
              <STOREW, gen_expr lo, gen_addr var>,
              <STOREW, gen_expr hi, address tmp>,
              <LABEL l1>,
              <JUMPC (Gt, l2), gen_expr var, <LOADW, address tmp>>,
              gen_stmt body,
              <STOREW, <BINOP Plus, gen_expr var, const 1>, gen_addr var>,
              <JUMP l1>,
              <LABEL l2>>

        | CaseStmt (sel, arms, deflt) ->
            (* Use one jump table, and hope it is reasonably compact *)
            let deflab = label () and donelab = label () in
            let labs = List.map (function x -> label ()) arms in
            let get_val (v, body) = get_value v in
            let table = List.combine (List.map get_val arms) labs in
            let gen_case lab (v, body) =
              <SEQ,
                <LABEL lab>,
                gen_stmt body,
                <JUMP donelab>> in
            <SEQ,
              gen_jtable (gen_expr sel) table deflab,
              <SEQ, @(List.map2 gen_case labs arms)>,
              <LABEL deflab>,
              gen_stmt deflt,
              <LABEL donelab>> in

     (* Label the code with a line number *)
     <SEQ, <LINE s.s_line>, code>

  module Code = Coder.F(Targ)

  let fix_param adj d =
    match d.d_addr with
        Symbol a ->
          a.a_val <- a.a_val - adj
      | _ -> failwith "fix_param"

  let rec max_params =
    function
      <w, @args> ->
        let x = match w with CALL n | CALLW n | CALLQ n -> n | _ -> 0 in
        max x (Util.maximum (List.map max_params args))

  (* |do_proc| -- generate code for a procedure and pass to the back end *)
  let do_proc lab lev nargs params (Block (_, body, fsize, nregv)) =
    level := lev+1; retlab := label ();
    Alloc.init !nregv;
    let code = Optree.canon <SEQ, gen_stmt body, <LABEL !retlab>> in
    Alloc.set_outgoing (Util.maximum (List.map max_params code));
    Emitter.start_proc lab !level nargs !fsize;

    begin try 
      Code.translate code;
    with exc ->
      (* Code generation failed, but let's see how far we got *)
      let bt = Printexc.get_backtrace () in
      printf "Partial code:\n" [];
      Code.output ();
      printf "Backtrace:\n$\n" [fStr bt];
      raise exc
    end;

    if not Targ.Metrics.fixed_frame then begin
      let adj = Targ.Metrics.param_base - Targ.Emitter.param_offset () in
      List.iter (fix_param adj) params
    end;

    Emitter.prelude ();
    Code.output ();
    Emitter.postlude()

  let get_decls (Block (decls, _, _, _)) = decls

  (* |gen_proc| -- translate a procedure, ignore other declarations *)
  let rec gen_proc = 
    function
        ProcDecl (Heading (x, _, _), block) ->
          let d = get_def x in
          let p = get_proc d.d_type in
          let line = Source.get_line x.x_line in
          printf "$$\n" [fStr Emitter.comment; fStr line];
          do_proc (symbol_of d) d.d_level p.p_pcount p.p_fparams block;
          gen_procs (get_decls block)
      | _ -> ()

  (* |gen_procs| -- generate code for the procedures in a block *)
  and gen_procs ds = List.iter gen_proc ds

  (* |gen_global| -- generate declaration for global variable *)
  let gen_global d =
    if d.d_kind = VarDef then begin
      match d.d_addr with
          Symbol lab ->
            Emitter.put_global lab (size_of d.d_type)
        | Absolute _ -> ()
        | _ -> failwith "gen_global"
    end

  (* |translate| -- generate code for the whole program *)
  let translate (Prog (block, glodefs)) =
    Emitter.preamble ();
    gen_procs (get_decls block);
    do_proc (symbol "pmain") 0 0 [] block;
    List.iter gen_global !glodefs;
    List.iter (fun (lab, s) ->
      Emitter.put_string lab s) (string_table ());
    Emitter.postamble ()
end
