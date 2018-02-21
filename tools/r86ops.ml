open Print

(* Registers of the x86 machine *)
type register =
    AX | CX | DX | BX | SI | DI | BP | SP
  | F0 | F1 | F2 | F3 | F4 | F5 | NoReg

let mktable ps =
  let t = Hashtbl.create 100 in
  let add (x, y) = Hashtbl.add t x y in
  List.iter add ps; t

let regtab =
  mktable [("%0", AX); ("%1", CX); ("%2", DX); ("%3", BX);
    ("%4", SI); ("%5", DI); ("%sp", SP); ("%bp", BP);
    ("%f0", F0); ("%f1", F1); ("%f2", F2); ("%f3", F3); 
    ("%f4", F4); ("%f5", F5)]

let find_reg x =
  try Hashtbl.find regtab x with
    Not_found -> failwith (sprintf "unknown register $" [fStr x])

(* |eqtab| -- table of register names *)
let eqtab =
  mktable [(AX, ("%eax", "%al")); (CX, ("%ecx", "%cl")); 
    (DX, ("%edx", "%dl")); (BX, ("%ebx", "%bl"));
    (SI, ("%esi", "")); (DI, ("%edi", "")); 
    (SP, ("%esp", "")); (BP, ("%ebp", ""))]

let equiv r =
  try Hashtbl.find eqtab r with 
    Not_found -> failwith (sprintf "no equivalent for reg" [])
    
(* is8bit -- test if register has low 8 bits accessible *)
let is8bit r = snd (equiv r) <> ""

let fReg r = fStr (fst (equiv r))
let fReg8 r = fStr (snd (equiv r))

type const = string

type rand = 
    Reg of register 
  | Const of const
  | Indir of register
  | Index of const * register * int
  | Index2 of register * register * int

let uses reg =
  function
      Reg r -> reg = r
    | Const _ -> false
    | Indir r -> reg = r
    | Index (_, r, s) -> reg = r
    | Index2 (r1, r2, s) -> reg = r1 || reg = r2

let fConst c = fMeta "$$" [fChr '$'; fStr c]

let fAddr =
  function
     Const s -> fStr s
   | Indir r -> fMeta "($)" [fReg r]
   | Index (b, r, s) -> 
       if s = 1 then fMeta "$($)" [fStr b; fReg r] else
         fMeta "$(,$,$)" [fStr b; fReg r; fNum s]
   | Index2 (r1, r2, s) -> 
       if s = 1 then fMeta "($,$)" [fReg r1; fReg r2] else
         fMeta "($,$,$)" [fReg r1; fReg r2; fNum s]
   | _ -> failwith "fAddr"

let fRand =
  function
      Reg r -> fReg r
    | Const c -> fConst c
    | _ -> failwith "fRand"

let put fmt args = printf "\t$\n" [fMeta fmt args]

let move ra rb =
  if ra <> rb then
    put "movl $, $" [fReg rb; fReg ra]

let push ra = put "pushl $" [fReg ra]

let pop ra = put "popl $" [fReg ra]

let binop_r op ra rb rc =
  if ra <> rb && ra = rc then begin
    let rx = if rc = AX then CX else AX in
    push rx; move rx rb; 
    put "$ $, $" [fStr op; fReg rc; fReg rx];
    move ra rx; pop rx
  end else begin
    move ra rb;
    put "$ $, $" [fStr op; fReg rc; fReg ra]
  end

let subtract ra rb =
  function
      Reg rc -> 
	if ra = rc then begin
	  put "subl $, $" [fReg rb; fReg ra];
	  put "negl $" [fReg ra]
	end else begin
	  move ra rb;
	  put "subl $, $" [fReg rc; fReg ra]
	end
    | Const c -> 
	move ra rb;
	put "subl $, $" [fConst c; fReg ra]
    | _ -> 
	failwith "subtract"

(* shift_r0 -- easy 3-register shift instruction *)
let shift_r0 op ra rb rc =
  (* Assume ra <> CX and if ra <> rb then ra <> rc *)
  move ra rb;
  if rc = CX then
    put "$ %cl, $" [fStr op; fReg ra]
  else begin
    push CX; move CX rc; 
    put "$ %cl, $" [fStr op; fReg ra]; 
    pop CX
  end

let shift op ra rb =
  function
      Reg rc -> 
	if ra <> CX && (ra = rb || ra <> rc) then
	  shift_r0 op ra rb rc
	else begin
	  (* Choose rx <> CX and rx <> rc *)
	  let rx = if rc = AX then DX else AX in
	  push rx; shift_r0 op rx rb rc; move ra rx; pop rx
	end
    | Const c ->
	move ra rb; 
	put "$ $, $" [fStr op; fConst c; fReg ra]
    | _ ->
	failwith "shift"

(* storec -- store character *)
let storec ra b =
  if is8bit ra then
    (* If ra = AX, then use "al" in the instruction *)
    put "movb $, $" [fReg8 ra; fAddr b]
  else begin
    let ra' = if not (uses AX b) then AX
      else if not (uses DX b) then DX else BX in
    push ra'; move ra' ra; 
    put "movb $, $" [fReg8 ra'; fAddr b]; 
    pop ra'
  end

(* compare -- comparison with boolean result *)
let compare op ra rb c =
  if is8bit ra then begin
    put "cmpl $, $" [fRand c; fReg rb];
    put "$ $" [fStr op; fReg8 ra];
    put "movzbl $, $" [fReg8 ra; fReg ra]
  end else begin
    put "push %eax" [];
    put "cmpl $, $" [fRand c; fReg rb];
    put "$ %al" [fStr op];
    put "movzbl %al, $" [fReg ra];
    put "pop %eax" []
  end

let commute op ra rb =
  function
      Reg rc -> 
	if ra = rc then
	  put "$ $, $" [fStr op; fReg rb; fReg ra]
	else begin
	  move ra rb;
	  put "$ $, $" [fStr op; fReg rc; fReg ra]
	end
    | Const c -> 
	move ra rb;
	put "$ $, $" [fStr op; fConst c; fReg ra]
    | _ -> 
	failwith "commute"

let jump op =
  function
      Const f -> put "$ $" [fStr op; fStr f]
    | Indir r -> put "$ *$" [fStr op; fReg r]
    | _ -> failwith "jump"

let branch op ra b lab =
  put "cmpl $, $" [fRand b; fReg ra];
  put "$ $" [fStr op; fAddr lab]

type instr =
    Copy of string
  | Fixed of string
  | OpV of (rand -> unit)
  | OpRR of (register -> register -> unit)
  | OpRV of (register -> rand -> unit)
  | OpRRV of (register -> register -> rand -> unit) 
  | OpRVV of (register -> rand -> rand -> unit)

let copy x = (x, Copy x)

(* |optab| -- table of risc86 operators *)
let optab =
  mktable [copy ".text"; copy ".global"; copy ".data"; copy ".comm";
    copy ".align"; copy ".string"; copy ".byte"; copy ".long"; copy ".section";
    ("prolog", 
      Fixed "pushl %ebx; pushl %esi; pushl %edi; pushl %ebp; movl %esp, %ebp");
    ("epilog", 
      Fixed "mov %ebp, %esp; popl %ebp; popl %edi; popl %esi; popl %ebx");
    ("ret", Fixed "ret");
    ("mov", OpRV (fun ra b -> put "movl $, $" [fRand b; fReg ra]));
    ("push", OpV (fun a -> put "pushl $" [fRand a]));
    ("add", OpRRV (commute "addl"));
    ("and", OpRRV (commute "andl"));
    ("or", OpRRV (commute "orl"));
    ("xor", OpRRV (commute "xorl"));
    ("sub", OpRRV subtract);
    ("mul", OpRRV (commute "imull"));
    ("neg", OpRR (fun ra rb -> move ra rb; put "negl $" [fReg ra]));
    ("not", OpRR (fun ra rb -> move ra rb; put "notl $" [fReg ra]));
    ("seteq", OpRRV (compare "sete"));
    ("setne", OpRRV (compare "setne"));
    ("setgt", OpRRV (compare "setg"));
    ("setlt", OpRRV (compare "setl"));
    ("setge", OpRRV (compare "setge"));
    ("setle", OpRRV (compare "setle"));
    ("shl", OpRRV (shift "shl")); 
    ("shr", OpRRV (shift "shr")); 
    ("sar", OpRRV (shift "sar"));
    ("ldw", OpRV (fun ra b -> put "movl $, $" [fAddr b; fReg ra]));
    ("stw", OpRV (fun ra b -> put "movl $, $" [fReg ra; fAddr b]));
    ("ldc", OpRV (fun ra b -> put "movzbl $, $" [fAddr b; fReg ra]));
    ("stc", OpRV storec);
    ("jmp", OpV (jump "jmp"));
    ("call", OpV (jump "call"));
    ("bge", OpRVV (branch "jge")); 
    ("bne", OpRVV (branch "jne")); 
    ("beq", OpRVV (branch "je"));
    ("blt", OpRVV (branch "jl")); 
    ("ble", OpRVV (branch "jle")); 
    ("bgt", OpRVV (branch "jg"));
    ("bae", OpRVV (branch "jae"))]

let emit op rands =
  match (op, rands) with
      (Copy x, _) -> put "$ $" [fStr x; fList(fAddr) rands]
    | (Fixed s, []) -> put "$" [fStr s];
    | (OpV f, [a]) -> f a
    | (OpRR f, [Reg ra; Reg rb]) -> f ra rb
    | (OpRV f, [Reg ra; b]) -> f ra b
    | (OpRRV g, [Reg ra; Reg rb; c]) -> g ra rb c
    | (OpRVV g, [Reg ra; b; c]) -> g ra b c
    | (_, _) -> failwith "bad rands"

let emit_lab lab =
  printf "$:\n" [fStr lab]

let emit_instr x rands = 
  try 
    let op = Hashtbl.find optab x in
    emit op rands
  with Not_found ->
    failwith (sprintf "unknown op $" [fStr x])
