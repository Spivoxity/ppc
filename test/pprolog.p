(* A prolog interpreter running a program that computes tilings *)

(* This program is the output of a macro processor, so contains many
untidy expressions *)


(* tunable parameters *)
const
  MAXSYMBOLS = 512;  (* max no. of symbols *)
  HASHFACTOR = 90;  (* percent loading factor for hash table *)
  MAXCHARS = 2048;  (* max chars in symbols *)
  MAXSTRING = 128;  (* max string length *)
  MAXARITY = 63;  (* max arity of function, vars in clause *)
  MEMSIZE = 25000;  (* size of |mem| array *)

(* special character values *)
const ENDSTR = chr(0);  (* end of string *)
  TAB = chr(9);  (* tab character *)
  ENDLINE = chr(10);  (* newline character *)
  ENDFILE = chr(127);  (* end of file *)

var run: boolean;  (* whether execution should continue *)
  dflag: boolean;  (* switch for debugging code *)

type
  permstring = integer;
  tempstring = array MAXSTRING of char;

var
  charptr: integer;
  charbuf: array MAXCHARS of char;

(* |StringLength| -- length of a tempstring *)
proc StringLength(var s: tempstring): integer;
  var i: integer;
begin
  i := 0;
  while s[i] <> ENDSTR do i := i+1 end;
  return i
end;

(* |SaveString| -- make a tempstring permanent *)
proc SaveString(var s: tempstring): permstring;
  var p, i: integer;
begin
  if charptr + StringLength(s) + 1 > MAXCHARS then
    newline(); print_string("Panic: "); print_string("out of string space"); newline(); exit(2)
  end;
  p := charptr; i := 0;
  repeat
    charbuf[charptr] := s[i]; charptr := charptr+1; i := i+1
  until charbuf[charptr-1] = ENDSTR;
  return p
end;

(* |StringEqual| -- compare a tempstring to a permstring *)
proc StringEqual(var s1: tempstring; s2: permstring): boolean;
  var i: integer;
begin
  i := 0;
  while (s1[i] <> ENDSTR) and (s1[i] = charbuf[s2+i]) do i := i+1 end;
  return (s1[i] = charbuf[s2+i])
end;

(* |WriteString| -- print a permstring *)
proc WriteString(s: permstring);
  var i: integer;
begin
  i := s;
  while charbuf[i] <> ENDSTR do
    print_char(charbuf[i]); i := i+1
  end
end;

type
  ptr = integer;  (* index into |mem| array *)

const NULL = 0;  (* null pointer *)

type term = ptr;

const FUNC = 1;  (* compound term *)
  INT = 2;  (* integer *)
  CHRCTR = 3;  (* character *)
  CELL = 4;  (* variable cell *)
  REF = 5;  (* variable reference *)
  UNDO = 6;  (* trail item *)

const TERM_SIZE = 2;  (* \dots\ plus no. of args *)

var
  lsp, gsp, hp, hmark: ptr;
  mem: array MEMSIZE+1 of integer;

(* |LocAlloc| -- allocate space on local stack *)
proc LocAlloc(size: integer): ptr;
  var p: ptr;
begin
  if lsp + size >= gsp then newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2) end;
  p := lsp + 1; lsp := lsp + size; return p
end;

(* |GloAlloc| -- allocate space on global stack *)
proc GloAlloc(kind, size: integer): ptr;
  var p: ptr;
begin
  if gsp - size <= lsp then
    newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2)
  end;
  gsp := gsp - size; p := gsp;
  mem[p] := lsl(kind, 8) + size;
  return p
end;

(* |HeapAlloc| -- allocate space on heap *)
proc HeapAlloc(size: integer): ptr;
  var p: ptr;
begin
  if hp + size > MEMSIZE then newline(); print_string("Panic: "); print_string("out of heap space"); newline(); exit(2) end;
  p := hp + 1; hp := hp + size; return p
end;

var infile: array 3000 of char; pin, pout: integer;

proc prog(line: array 61 of char);
  var i: integer;
begin
  for i := 0 to 59 do
    infile[pin] := line[i]; pin := pin+1
  end;
  infile[pin] := ENDLINE; pin := pin+1
end;

proc rdchar(var ch: char);
begin
  if pout >= pin then
    ch := ENDFILE
  else
    ch := infile[pout]; pout := pout+1
  end
end;

var
  pbchar: char;  (* pushed-back char, else |ENDFILE| *)
  lineno: integer;  (* line number in current file *)

(* |GetChar| -- get a character *)
proc GetChar(): char;
  var ch: char;
begin
  if pbchar <> ENDFILE then
    ch := pbchar; pbchar := ENDFILE
  else
    rdchar(ch);
    if ch = ENDLINE then lineno := lineno+1 end
  end;
  return ch
end;

(* |PushBack| -- push back a character on the input *)
proc PushBack(ch: char);
begin
  pbchar := ch
end;

type clause = ptr;

const CLAUSE_SIZE = 4;  (* ... plus size of body + 1 *)

type frame = ptr;

const FRAME_SIZE = 7;  (* \dots plus space for local variables *)

var
  current: ptr;  (* current goal *)
  call: term;  (* |Deref|'ed first literal of goal *)
  goalframe: frame;  (* current stack frame *)
  choice: frame;  (* last choice point *)
  base: frame;  (* frame for original goal *)
  prok: clause;  (* clauses left to try on current goal *)

(* |Deref| -- follow |VAR| and |CELL| pointers *)
proc Deref(t: term; e: frame): term;
begin
  if t = NULL then newline(); print_string("Panic: "); print_string("Deref"); newline(); exit(2) end;
  if (lsr(mem[t], 8) = REF) and (e <> NULL) then
    t := (e+7+(mem[t+1]-1)*TERM_SIZE)
  end;
  while (lsr(mem[t], 8) = CELL) and (mem[t+1] <> NULL) do
    t := mem[t+1]
  end;
  return t
end;

type symbol = integer;  (* index in |symtab| *)

var
  nsymbols: integer;  (* number of symbols *)
  symtab: array MAXSYMBOLS+1 of record
      name: integer;  (* print name: index in |charbuf| *)
      arity: integer;  (* number of arguments or -1 *)
      action: integer;  (* code if built-in, 0 otherwise *)
      prok: clause  (* clause chain *)
    end;
  cons, eqsym, cutsym, nilsym, notsym: symbol;
  node: symbol;

(* |Lookup| -- convert string to internal symbol *)
proc Lookup(var name: tempstring): symbol;
  var h, i: integer; p: symbol;
begin
  (* Compute the hash function in |h| *)
  h := 0; i := 0;
  while name[i] <> ENDSTR do
    h := (5 * h + ord(name[i])) mod MAXSYMBOLS; i := i+1 
  end;

  (* Search the hash table *)
  p := h+1;
  while symtab[p].name <> -1 do
    if StringEqual(name, symtab[p].name) then return p end;
    p := p-1;
    if p = 0 then p := MAXSYMBOLS end
  end;

  (* Not found: enter a new symbol *)
  (* Be careful to avoid overflow on 16 bit machines: *)
  if nsymbols >= (MAXSYMBOLS div 10) * (HASHFACTOR div 10) then
    newline(); print_string("Panic: "); print_string("out of symbol space"); newline(); exit(2)
  end;
  symtab[p].name := SaveString(name);
  symtab[p].arity := -1;
  symtab[p].action := 0; symtab[p].prok := NULL;
  return p
end;

type keyword = array 9 of char;

(* |Enter| -- define a built-in symbol *)
proc Enter(name: keyword; arity: integer; action: integer): symbol;
  var s: symbol; i: integer; temp: tempstring;
begin
  i := 0;
  while name[i] <> ' ' do
    temp[i] := name[i]; i := i+1 
  end;
  temp[i] := ENDSTR; s := Lookup(temp);
  symtab[s].arity := arity; symtab[s].action := action;
  return s
end;

(* Codes for built-in relations *)
const
  CUT = 1;  (* $!/0$ *)
  CALL = 2;  (* |call/1| *)
  PLUS = 3;  (* |plus/3| *)
  TIMES = 4;  (* |times/3| *)
  ISINT = 5;  (* |integer/1| *)
  ISCHAR = 6;  (* |char/1| *)
  NAFF = 7;  (* |not/1| *)
  EQUALITY = 8;  (* |=/2| *)
  FAIL = 9;  (* |false/0| *)
  PRINT = 10;  (* |print/1| *)
  NL = 11;  (* |nl/0| *)

(* |InitSymbols| -- initialize and define standard symbols *)
proc InitSymbols();
  var i: integer; dummy: symbol;
begin
  nsymbols := 0;
  for i := 1 to MAXSYMBOLS do symtab[i].name := -1 end;
  cons   := Enter(":       ", 2, 0);
  cutsym := Enter("!       ", 0, CUT);
  eqsym  := Enter("=       ", 2, EQUALITY);
  nilsym := Enter("nil     ", 0, 0);
  notsym := Enter("not     ", 1, NAFF);
  node   := Enter("node    ", 2, 0);
  dummy  := Enter("call    ", 1, CALL);
  dummy  := Enter("plus    ", 3, PLUS);
  dummy  := Enter("times   ", 3, TIMES);
  dummy  := Enter("integer ", 1, ISINT);
  dummy  := Enter("char    ", 1, ISCHAR);
  dummy  := Enter("false   ", 0, FAIL);
  dummy  := Enter("print   ", 1, PRINT);
  dummy  := Enter("nl      ", 0, NL)
end;

(* |AddClause| -- insert a clause at the end of its chain *)
proc AddClause(c: clause);
  var s: symbol; p: clause;
begin
  s := mem[mem[c+3]+1];
  if symtab[s].action <> 0 then
    newline(); print_string("Error: "); print_string("cannot add clauses to built-in relation "); run := false;
    WriteString(symtab[s].name)
  elsif symtab[s].prok = NULL then
    symtab[s].prok := c
  else
    p := symtab[s].prok;
    while mem[p+2] <> NULL do p := mem[p+2] end;
    mem[p+2] := c
  end
end;

type argbuf = array MAXARITY+1 of term;

(* |MakeCompound| -- construct a compound term on the heap *)
proc MakeCompound(fun: symbol; var arg: argbuf): term;
  var p: term; i, n: integer;
begin
  n := symtab[fun].arity;
  p := HeapAlloc(TERM_SIZE+n);
  mem[p] := lsl(FUNC, 8) + TERM_SIZE+n;
  mem[p+1] := fun;
  for i := 1 to n do mem[p+i+1] := arg[i] end;
  return p
end;

(* |MakeNode| -- construct a compound term with up to 2 arguments *)
proc MakeNode(fun: symbol; a1, a2: term): term;
  var arg: argbuf;
begin
  arg[1] := a1; arg[2] := a2;
  return MakeCompound(fun, arg)
end;

var refnode: array MAXARITY+1 of term;

(* |MakeRef| -- return a reference cell prepared earlier *)
proc MakeRef(offset: integer): term;
begin
  return refnode[offset]
end;

(* |MakeInt| -- construct an integer node on the heap *)
proc MakeInt(i: integer): term;
  var p: term;
begin
  p := HeapAlloc(TERM_SIZE);
  mem[p] := lsl(INT, 8) + TERM_SIZE;
  mem[p+1] := i; return p
end;

(* |MakeChar| -- construct a character node on the heap *)
proc MakeChar(c: char): term;
  var p: term;
begin
  p := HeapAlloc(TERM_SIZE);
  mem[p] := lsl(CHRCTR, 8) + TERM_SIZE;
  mem[p+1] := ord(c); return p
end;

(* |MakeString| -- construct a string as a Prolog list of chars *)
proc MakeString(var s: tempstring): term;
  var p: term; i: integer;
begin
  i := StringLength(s);
  p := MakeNode(nilsym, NULL, NULL);
  while i > 0 do
    i := i-1; p := MakeNode(cons, MakeChar(s[i]), p)
  end;
  return p
end;

(* |MakeClause| -- construct a clause on the heap *)
proc MakeClause(nvars: integer; head: term;
		    var body: argbuf; nbody: integer): clause;
  var p: clause; i: integer;
begin
  p := HeapAlloc(CLAUSE_SIZE + nbody + 1);
  mem[p] := nvars; mem[p+2] := NULL; mem[p+3] := head;
  for i := 1 to nbody do mem[(p+4)+i-1] := body[i] end;
  mem[(p+4)+nbody+1-1] := NULL;
  if head = NULL then 
    mem[p+1] := 0
  else 
    mem[p+1] := Key(head, NULL)
  end;
  return p
end;

(* operator priorities *)
const
  MAXPRIO = 2;  (* isolated term *)
  ARGPRIO = 2;  (* function arguments *)
  EQPRIO = 2;  (* equals sign *)
  CONSPRIO = 1;  (* colon *)

(* |IsString| -- check if a list represents a string *)
proc IsString(t: term; e: frame): boolean;
  const limit = 128;
  var i: integer;
begin
  i := 0; t := Deref(t, e);
  while i < limit do
    if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
      return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
    elsif lsr(mem[Deref(mem[t+1+1], e)], 8) <> CHRCTR then
      return false
    else
      i := i+1; t := Deref(mem[t+2+1], e) 
    end
  end;
  return false
end;

(* |IsList| -- check if a term is a proper list *)
proc IsList(t: term; e: frame): boolean;
  const limit = 128;
  var i: integer;
begin
  i := 0; t := Deref(t, e);
  while i < limit do
    if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
      return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
    else
      i := i+1; t := Deref(mem[t+2+1], e)
    end
  end;
  return false
end;

(* |ShowString| -- print a list as a string *)
proc ShowString(t: term; e: frame);
begin
  t := Deref(t, e);
  print_char('"');
  while mem[t+1] <> nilsym do
    print_char(chr(mem[Deref(mem[t+1+1], e)+1]));
    t := Deref(mem[t+2+1], e)
  end;
  print_char('"')
end;

(* |PrintCompound| -- print a compound term *)
proc PrintCompound(t: term; e: frame; prio: integer);
  var f: symbol; i: integer;
begin
  f := mem[t+1];
  if f = cons then
    (* |t| is a list: try printing as a string, or use infix : *)
    if IsString(t, e) then
      ShowString(t, e)
    else
      if prio < CONSPRIO then print_char('(') end;
      PrintTerm(mem[t+1+1], e, CONSPRIO-1);
      print_char(':');
      PrintTerm(mem[t+2+1], e, CONSPRIO);
      if prio < CONSPRIO then print_char(')') end
    end
  elsif f = eqsym then
    (* |t| is an equation: use infix = *)
    if prio < EQPRIO then print_char('(') end;
    PrintTerm(mem[t+1+1], e, EQPRIO-1);
    print_string(" = ");
    PrintTerm(mem[t+2+1], e, EQPRIO-1);
    if prio < EQPRIO then print_char(')') end
  elsif f = notsym then
    (* |t| is a literal 'not P' *)
    print_string("not ");
    PrintTerm(mem[t+1+1], e, MAXPRIO)
  elsif (f = node) and IsList(mem[t+2+1], e) then
    PrintNode(t, e)
  else
    (* use ordinary notation *)
    WriteString(symtab[f].name);
    if symtab[f].arity > 0 then
      print_char('(');
      PrintTerm(mem[t+1+1], e, ARGPRIO);
      for i := 2 to symtab[f].arity do
        print_string(", ");
        PrintTerm(mem[t+i+1], e, ARGPRIO)
      end;
      print_char(')')
    end
  end
end;

(* |PrintNode| -- print and optree node *)
proc PrintNode(t: term; e: frame);
  var u: term;
begin
  print_char('<');
  PrintTerm(mem[t+1+1], e, MAXPRIO);
  u := Deref(mem[t+2+1], e);
  while mem[u+1] <> nilsym do
    print_string(", ");
    PrintTerm(mem[u+1+1], e, MAXPRIO);
    u := Deref(mem[u+2+1], e)
  end;
  print_char('>');
end;

(* |PrintTerm| -- print a term *)
proc PrintTerm(t: term; e: frame; prio: integer);
begin
  t := Deref(t, e);
  if t = NULL then
    print_string("*null-term*")
  else
    case lsr(mem[t], 8) of
      FUNC:
        PrintCompound(t, e, prio)
    | INT:
        print_num(mem[t+1])
    | CHRCTR:
        print_char(''''); print_char(chr(mem[t+1])); print_char('''')
    | CELL:
        if (t >= gsp) then
          print_char('G'); print_num((MEMSIZE - t) div TERM_SIZE)
        else
          print_char('L'); print_num((t - hp) div TERM_SIZE)
        end
    | REF:
        print_char('@'); print_num(mem[t+1])
    else
      print_string("*unknown-term(tag="); 
      print_num(lsr(mem[t], 8)); print_string(")*")
    end
  end
end;

(* |PrintClause| -- print a clause *)
proc PrintClause(c: clause);
  var i: integer;
begin
  if c = NULL then
    print_string("*null-clause*"); newline();
  else
    if mem[c+3] <> NULL then
      PrintTerm(mem[c+3], NULL, MAXPRIO);
      print_char(' ')
    end;
    print_string(":- ");
    if mem[(c+4)+1-1] <> NULL then
      PrintTerm(mem[(c+4)+1-1], NULL, MAXPRIO);
      i := 2;
      while mem[(c+4)+i-1] <> NULL do
	print_string(", ");
	PrintTerm(mem[(c+4)+i-1], NULL, MAXPRIO);
	i := i+1
      end
    end;
    print_char('.'); newline()
  end
end;

var
  token: integer;  (* last token from input *)
  tokval: symbol;  (* if |token = IDENT|, the identifier*)
  tokival: integer;  (* if |token = NUMBER|, the number *)
  toksval: tempstring;  (* if |token = STRCON|, the string *)
  errflag: boolean;  (* whether recovering from an error *)
  errcount: integer;  (* number of errors found so far *)

(* Possible values for |token|: *)
const
  IDENT = 1;  (* identifier: see |tokval| *)
  VARIABLE = 2;  (* variable: see |tokval| *)
  NUMBER = 3;  (* number: see |tokival| *)
  CHCON = 4;  (* char constant: see |tokival| *)
  STRCON = 5;  (* string constant: see |toksval| *)
  ARROW = 6;  (* |':-'| *)
  LPAR = 7;  (* |'('| *)
  RPAR = 8;  (* |')'| *)
  COMMA = 9;  (* |','| *)
  DOT = 10;  (* |'.'| *)
  COLON = 11;  (* |':'| *)
  EQUAL = 12;  (* |'='| *)
  NEGATE = 13;  (* |'not'| *)
  EOFTOK = 14;  (* end of file *)
  LANGLE = 15;  (* |'<'| *)
  RANGLE = 16;  (* |'>'| *)
  HASH = 17;  (* |'#'| *)

(* |ShowError| -- report error location *)
proc ShowError();
begin
  errflag := true; errcount := errcount+1;
  print_string("Line "); print_num(lineno); print_char(' ');
  print_string("Syntax error - ")
end;

(* |Recover| -- discard rest of input clause *)
proc Recover();
  var ch: char;
begin
  if errcount >= 20 then
    print_string("Too many errors: I am giving up"); newline(); exit(2) 
  end;
  if token <> DOT then
    repeat
      ch := GetChar()
    until (ch = '.') or (ch = ENDFILE);
    token := DOT
  end
end;

(* |Scan| -- read one symbol from |infile| into |token|. *)
proc Scan();
  var ch, ch2: char; i: integer;
begin
  ch := GetChar(); token := 0;
  while token = 0 do
    (* Loop after white-space or comment *)
    if ch = ENDFILE then
      token := EOFTOK
    elsif (ch = ' ') or (ch = TAB) or (ch = ENDLINE) then
      ch := GetChar()
    elsif ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) then
      if (((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) then 
	 token := VARIABLE
      else 
	 token := IDENT
      end;
      i := 0;
      while ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) or ((ch >= '0') and (ch <= '9')) do
        if i > MAXSTRING then
          newline(); print_string("Panic: "); print_string("identifier too long"); newline(); exit(2)
	end;
        toksval[i] := ch; ch := GetChar(); i := i+1
      end;
      PushBack(ch);
      toksval[i] := ENDSTR; tokval := Lookup(toksval);
      if tokval = notsym then token := NEGATE end
    elsif ((ch >= '0') and (ch <= '9')) then
      token := NUMBER; tokival := 0;
      while ((ch >= '0') and (ch <= '9')) do
        tokival := 10 * tokival + (ord(ch) - ord('0'));
        ch := GetChar()
      end;
      PushBack(ch)
    else
      case ch of
        '(': token := LPAR
      | ')': token := RPAR
      | ',': token := COMMA
      | '.': token := DOT
      | '=': token := EQUAL
      | '<': token := LANGLE
      | '>': token := RANGLE
      | '#': token := HASH
      | '!': token := IDENT; tokval := cutsym
      | '/':
	  ch := GetChar();
	  if ch <> '*' then
	    if not errflag then ShowError(); print_string("bad token /"); newline(); Recover() end
	  else
	    ch2 := ' '; ch := GetChar();
	    while (ch <> ENDFILE) and not ((ch2 = '*') and (ch = '/')) do
	      ch2 := ch; ch := GetChar() 
	    end;
	    if ch = ENDFILE then
	      if not errflag then ShowError(); print_string("end of file in comment"); newline(); Recover() end
	    else
	      ch := GetChar()
	    end
	  end
      | ':':
	  ch := GetChar();
	  if ch = '-' then
	    token := ARROW
	  else
	    PushBack(ch); token := COLON 
	  end
      | '''':
	  token := CHCON; tokival := ord(GetChar()); ch := GetChar();
	  if ch <> '''' then if not errflag then ShowError(); print_string("missing quote"); newline(); Recover() end end
      | '"':
	  token := STRCON; i := 0; ch := GetChar();
	  while (ch <> '"') and (ch <> ENDLINE) do
	    toksval[i] := ch; ch := GetChar(); i := i+1 
	  end;
	  toksval[i] := ENDSTR;
	  if ch = ENDLINE then
	    if not errflag then ShowError(); print_string("unterminated string"); newline(); Recover() end;
	    PushBack(ch)
	  end
      else
	if not errflag then ShowError(); print_string("illegal character"); newline(); Recover() end; print_char(ch); newline()
      end
    end
  end
end;

(* |PrintToken| -- print a token as a string *)
proc PrintToken(t: integer);
begin
  case t of
    IDENT:
      print_string("identifier "); WriteString(symtab[tokval].name)
  | VARIABLE:
      print_string("variable "); WriteString(symtab[tokval].name)
  | NUMBER: print_string("number");
  | CHCON:  print_string("char constant");
  | ARROW:  print_string(":-");
  | LPAR:   print_string("(");
  | RPAR:   print_string(")");
  | COMMA:  print_string(",");
  | DOT:    print_string(".");
  | COLON:  print_string(":");
  | EQUAL:  print_string("=");
  | STRCON: print_string("string constant")
  | LANGLE: print_string("<")
  | RANGLE: print_string(">")
  | HASH:   print_string("#")
  else
    print_string("unknown token")
  end
end;

var
  nvars: integer;  (* no. of variables so far *)
  vartable: array MAXARITY+1 of symbol;  (* names of the variables *)

(* |VarRep| -- look up a variable name *)
proc VarRep(name: symbol): term;
  var i: integer;
begin
  if nvars = MAXARITY then newline(); print_string("Panic: "); print_string("too many variables"); newline(); exit(2) end;
  i := 1; vartable[nvars+1] := name;  (* sentinel *)
  while name <> vartable[i] do i := i+1 end;
  if i = nvars+1 then nvars := nvars+1 end;
  return MakeRef(i)
end;

(* |ShowAnswer| -- display answer and get response *)
proc ShowAnswer(bindings: frame);
  var i: integer; ch, ch2: char;
begin
  if nvars = 0 then
    print_string("yes"); newline()
  else
    for i := 1 to nvars do
      WriteString(symtab[vartable[i]].name); print_string(" = ");
      PrintTerm((bindings+7+(i-1)*TERM_SIZE), NULL, EQPRIO-1);
      newline()
    end
  end
end;

(* |Eat| -- check for an expected token and discard it *)
proc Eat(expected: integer);
begin
  if token = expected then
    if token <> DOT then Scan() end
  elsif not errflag then
    ShowError();
    print_string("expected "); PrintToken(expected);
    print_string(", found "); PrintToken(token); newline();
    Recover()
  end
end;

(* |ParseCompound| -- parse a compound term *)
proc ParseCompound(): term;
  var fun: symbol; arg: argbuf; n: integer;
begin
  fun := tokval; n := 0; Eat(IDENT);
  if token = LPAR then
    Eat(LPAR); n := 1; arg[1] := ParseTerm();
    while token = COMMA do
      Eat(COMMA); n := n+1; arg[n] := ParseTerm()
    end;
    Eat(RPAR)
  end;
  if symtab[fun].arity = -1 then
    symtab[fun].arity := n
  elsif symtab[fun].arity <> n then
    if not errflag then ShowError(); print_string("wrong number of args"); newline(); Recover() end
  end;
  return MakeCompound(fun, arg)
end;

(* |ParsePrimary| -- parse a primary *)
proc ParsePrimary(): term;
  var t: term;
begin
  if token = IDENT then t := ParseCompound()
  elsif token = VARIABLE then
    t := VarRep(tokval); Eat(VARIABLE)
  elsif token = NUMBER then
    t := MakeInt(tokival); Eat(NUMBER)
  elsif token = CHCON then
    t := MakeChar(chr(tokival)); Eat(CHCON)
  elsif token = STRCON then
    t := MakeString(toksval); Eat(STRCON)
  elsif token = LPAR then
    Eat(LPAR); t := ParseTerm(); Eat(RPAR)
  elsif token = LANGLE then
    t := ParseNode()
  else
    if not errflag then ShowError(); print_string("expected a term"); newline(); Recover() end; t := NULL
  end;
  return t
end;

(* |ParseNode| -- parse an optree node *)
proc ParseNode(): term;
  var tag, kids: term;
begin
  Eat(LANGLE);
  tag := ParseTerm();
  kids := ParseKids();
  Eat(RANGLE);
  return MakeNode(node, tag, kids)
end;

(* |ParseKids| -- parse children of an optree node *)
proc ParseKids(): term;
  var head, tail: term;
begin
  if token <> COMMA then
    return MakeNode(nilsym, NULL, NULL)
  else
    Eat(COMMA);
    head := ParseTerm();
    tail := ParseKids();
    return MakeNode(cons, head, tail)
  end
end;    

(* |ParseFactor| -- parse a factor *)
proc ParseFactor(): term;
  var t: term;
begin
  t := ParsePrimary();
  if token <> COLON then
    return t
  else
    Eat(COLON);
    return MakeNode(cons, t, ParseFactor())
  end
end;

(* |ParseTerm| -- parse a term *)
proc ParseTerm(): term;
  var t: term;
 begin
  t := ParseFactor();
  if token <> EQUAL then
    return t
  else
    Eat(EQUAL);
    return MakeNode(eqsym, t, ParseFactor())
  end
end;

(* |CheckAtom| -- check that a literal is a compound term *)
proc CheckAtom(a: term);
begin
  if lsr(mem[a], 8) <> FUNC then
    if not errflag then ShowError(); print_string("literal must be a compound term"); newline(); Recover() end
  end
end;

(* |ParseClause| -- parse a clause *)
proc ParseClause(): clause;
  var head, t: term; 
    body: argbuf; 
    n: integer;
    minus, more: boolean;
begin
  if token = HASH then
    Eat(HASH); head := NULL
  else
    head := ParseTerm();
    CheckAtom(head)
  end;
  Eat(ARROW);
  n := 0;
  if token <> DOT then
    more := true;
    while more do
      n := n+1; minus := false;
      if token = NEGATE then
	Eat(NEGATE); minus := true 
      end;
      t := ParseTerm(); CheckAtom(t);
      if minus then 
	body[n] := MakeNode(notsym, t, NULL)
      else 
        body[n] := t
      end;
      if token = COMMA then Eat(COMMA) else more := false end
    end
  end;
  Eat(DOT);

  if errflag then 
    return NULL
  else 
    return MakeClause(nvars, head, body, n)
  end
end;

(* |ReadClause| -- read a clause from |infile| *)
proc ReadClause(): clause;
  var c: clause;
begin
  repeat
    hp := hmark; nvars := 0; errflag := false;
    Scan();
    if token = EOFTOK then 
      c := NULL
    else 
      c := ParseClause()
    end
  until (not errflag) or (token = EOFTOK);
  return c
end;

type trail = ptr;

const TRAIL_SIZE = 3;

var trhead: trail;  (* start of the trail *)

(* |Save| -- add a variable to the trail if it is critical *)
proc Save(v: term);
  var p: trail;
begin
  if ((v < choice) or (v >= mem[choice+4])) then
    p := GloAlloc(UNDO, TRAIL_SIZE);
    mem[p+1] := v; mem[p+2] := trhead; trhead := p
  end
end;

(* |Restore| -- undo bindings back to previous state *)
proc Restore();
  var v: term;
begin
  while (trhead <> mem[choice+5]) do
    v := mem[trhead+1];
    if v <> NULL then mem[v+1] := NULL end;
    trhead := mem[trhead+2]
  end
end;

(* |Commit| -- blank out trail entries not needed after cut *)
proc Commit();
  var p: trail;
begin
  p := trhead;
  while (p <> NULL) and (p < mem[choice+4]) do
    if (mem[p+1] <> NULL) and not ((mem[p+1] < choice) or (mem[p+1] >= mem[choice+4])) then
      mem[p+1] := NULL
    end;
    p := mem[p+2]
  end
end;

(* |GloCopy| -- copy a term onto the global stack *)
proc GloCopy(t: term; e: frame): term;
  var tt: term; i, n: integer;
begin
  t := Deref(t, e);
  if (t >= gsp) then
    return t
  else
    case lsr(mem[t], 8) of
      FUNC:
	n := symtab[mem[t+1]].arity;
	if (t <= hp) and (n = 0) then 
	  return t
	else
	  tt := GloAlloc(FUNC, TERM_SIZE+n);
	  mem[tt+1] := mem[t+1];
	  for i := 1 to n do
	    mem[tt+i+1] := GloCopy(mem[t+i+1], e)
	  end;
	  return tt
	end
    | CELL:
        tt := GloAlloc(CELL, TERM_SIZE);
        mem[tt+1] := NULL;
	Save(t); mem[t+1] := tt;
        return tt
    else
      return t
    end
  end
end;

(* |Share| -- bind two variables together *)
proc Share(v1, v2: term);
begin
  if (v1 * (2 * ord((v1 >= gsp)) - 1)) <= (v2 * (2 * ord((v2 >= gsp)) - 1)) then
    Save(v1); mem[v1+1] := v2
  else
    Save(v2); mem[v2+1] := v1 
  end
end;

(* |Unify| -- find and apply unifier for two terms *)
proc Unify(t1: term; e1: frame; t2: term; e2: frame): boolean;
  var i: integer; match: boolean;
begin
  t1 := Deref(t1, e1); t2 := Deref(t2, e2);
  if t1 = t2 then  (* Includes unifying a var with itself *)
    return true
  elsif (lsr(mem[t1], 8) = CELL) and (lsr(mem[t2], 8) = CELL) then
    Share(t1, t2); return true
  elsif lsr(mem[t1], 8) = CELL then
    Save(t1); mem[t1+1] := GloCopy(t2, e2); return true
  elsif lsr(mem[t2], 8) = CELL then
    Save(t2); mem[t2+1] := GloCopy(t1, e1); return true
  elsif lsr(mem[t1], 8) <> lsr(mem[t2], 8) then
    return false
  else
    case lsr(mem[t1], 8) of
      FUNC:
        if (mem[t1+1] <> mem[t2+1]) then
          return false
        else
          i := 1; match := true;
          while match and (i <= symtab[mem[t1+1]].arity) do
            match := Unify(mem[t1+i+1], e1, mem[t2+i+1], e2);
            i := i+1
          end;
          return match
        end
    | INT:
        return (mem[t1+1] = mem[t2+1])
    | CHRCTR:
        return (mem[t1+1] = mem[t2+1])
    else
      newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t1):1, " in ", "Unify"*)); newline(); exit(2)
    end
  end
end;

(* |Key| -- unification key of a term *)
proc Key(t: term; e: frame): integer;
  var t0: term;
begin
  (* The argument |t| must be a direct pointer to a compound term.
    The value returned is |key(t)|: if |t1| and |t2| are unifiable,
    then |key(t1) = 0| or |key(t2) = 0| or |key(t1) = key(t2)|. *)

  if t = NULL then newline(); print_string("Panic: "); print_string("Key"); newline(); exit(2) end;
  if lsr(mem[t], 8) <> FUNC then newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t):1, " in ", "Key1"*)); newline(); exit(2) end;

  if symtab[mem[t+1]].arity = 0 then
    return 0
  else
    t0 := Deref(mem[t+1+1], e);
    case lsr(mem[t0], 8) of
        FUNC:      return mem[t0+1]
      | INT:       return mem[t0+1] + 1
      | CHRCTR:    return mem[t0+1] + 1
    else
      return 0
    end
  end
end;

(* |Search| -- find the first clause that might match *)
proc Search(t: term; e: frame; p: clause): clause;
  var k: integer;
begin
  k := Key(t, e);
  if k <> 0 then
    while (p <> NULL) and (mem[p+1] <> 0) and (mem[p+1] <> k) do
      p := mem[p+2]
    end
  end;
  return p
end;

var ok: boolean;  (* whether execution succeeded *)

(* |PushFrame| -- create a new local stack frame *)
proc PushFrame(nvars: integer; retry: clause);
  var f: frame; i: integer;
begin
  f := LocAlloc((FRAME_SIZE + (nvars)*TERM_SIZE));
  mem[f] := current; mem[f+1] := goalframe;
  mem[f+2] := retry; mem[f+3] := choice;
  mem[f+4] := gsp; mem[f+5] := trhead;
  mem[f+6] := nvars;
  for i := 1 to nvars do
    mem[(f+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
    mem[(f+7+(i-1)*TERM_SIZE)+1] := NULL
  end;
  goalframe := f;
  if retry <> NULL then choice := goalframe end
end;

(* |TroStep| -- perform a resolution step with tail-recursion *)
proc TroStep();
  var temp: frame; oldsize, newsize, i: integer;
begin
  if dflag then print_string("(TRO)"); newline() end;

  oldsize := (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE); (* size of old frame *)
  newsize := (FRAME_SIZE + (mem[prok])*TERM_SIZE); (* size of new frame *)
  temp := LocAlloc(newsize);
  temp := goalframe + newsize; (* copy old frame here *)

  (* Copy the old frame: in reverse order in case of overlap *)
  for i := 1 to oldsize do 
    mem[temp+oldsize-i] := mem[goalframe+oldsize-i]
  end;

  (* Adjust internal pointers in the copy *)
  for i := 1 to mem[goalframe+6] do
    if (lsr(mem[(temp+7+(i-1)*TERM_SIZE)], 8) = CELL)
        and (mem[(temp+7+(i-1)*TERM_SIZE)+1] <> NULL)
        and (goalframe <= mem[(temp+7+(i-1)*TERM_SIZE)+1])
        and (mem[(temp+7+(i-1)*TERM_SIZE)+1] < goalframe + oldsize) then
      mem[(temp+7+(i-1)*TERM_SIZE)+1] := mem[(temp+7+(i-1)*TERM_SIZE)+1] + newsize
    end
  end;

  (* Overwrite the old frame with the new one *)
  mem[goalframe+6] := mem[prok];
  for i := 1 to mem[goalframe+6] do
    mem[(goalframe+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
    mem[(goalframe+7+(i-1)*TERM_SIZE)+1] := NULL
  end;

  (* Perform the resolution step *)
  ok := Unify(call, temp, mem[prok+3], goalframe);
  current := (prok+4);
  lsp := temp-1
end;

(* |Step| -- perform a resolution step *)
proc Step();
  var retry: clause;
begin
  if symtab[mem[call+1]].action <> 0 then
    ok := DoBuiltin(symtab[mem[call+1]].action)
  elsif prok = NULL then
    ok := false
  else
    retry := Search(call, goalframe, mem[prok+2]);
    if (mem[(current)+1] = NULL) and (choice < goalframe)
    and (retry = NULL) and (goalframe <> base) then
      TroStep()
    else
      PushFrame(mem[prok], retry);
      ok := Unify(call, mem[goalframe+1], mem[prok+3], goalframe);
      current := (prok+4);
    end
  end
end;

(* |Unwind| -- return from completed clauses *)
proc Unwind();
begin
  while (mem[current] = NULL) and (goalframe <> base) do
    if dflag then 
    print_string("Exit"); print_string(": "); 
    PrintTerm(mem[mem[goalframe]], mem[goalframe+1], MAXPRIO); newline()
  end;
    current := (mem[goalframe])+1;
    if goalframe > choice then lsp := goalframe-1 end;
    goalframe := mem[goalframe+1]
  end
end;

(* |Backtrack| -- roll back to the last choice-point *)
proc Backtrack();
begin
  Restore();
  current := mem[choice]; goalframe := mem[choice+1];
  call := Deref(mem[current], goalframe);
  prok := mem[choice+2]; gsp := mem[choice+4];
  lsp := choice-1; choice := mem[choice+3];
  if dflag then 
    print_string("Redo"); print_string(": "); 
    PrintTerm(call, goalframe, MAXPRIO); newline()
  end;
end;

(* |Resume| -- continue execution *)
proc Resume(flag: boolean);
begin
  ok := flag;
  while run do
    if ok then
      if mem[current] = NULL then return end;
      call := Deref(mem[current], goalframe);
      if dflag then 
    print_string("Call"); print_string(": "); 
    PrintTerm(call, goalframe, MAXPRIO); newline()
  end;
      if (symtab[mem[call+1]].prok = NULL)
	  and (symtab[mem[call+1]].action = 0) then
	newline(); print_string("Error: "); print_string("call to undefined relation "); run := false;
	WriteString(symtab[mem[call+1]].name);
	return
      end;
      prok := Search(call, goalframe, symtab[mem[call+1]].prok)
    else
      if choice <= base then return end;
      Backtrack()
    end;
    Step();
    if ok then Unwind() end;
  end;
end;

(* |Execute| -- solve a goal by SLD-resolution *)
proc Execute(g: clause);
  var nsoln: integer;
begin
  lsp := hp; gsp := MEMSIZE+1;
  current := NULL; goalframe := NULL; choice := NULL; trhead := NULL;
  PushFrame(mem[g], NULL);
  choice := goalframe; base := goalframe; current := (g+4);
  run := true;
  Resume(true);
  if not run then return end;
  while ok do
    nsoln := nsoln+1;
    ShowAnswer(base);
    newline();
    Resume(false);
    if not run then return end;
  end;

  if nsoln = 0 then
    print_string("no"); newline(); newline();
  end
end;

var
  av: argbuf;  (* |GetArgs| puts arguments here *)
  callbody: ptr;  (* dummy clause body used by |call/1| *)

(* |GetArgs| -- set up |av| array *)
proc GetArgs();
  var i: integer;
begin
  for i := 1 to symtab[mem[call+1]].arity do
    av[i] := Deref(mem[call+i+1], goalframe)
  end
end;

proc NewInt(n: integer): term;
  var t: term;
begin
  t := GloAlloc(INT, TERM_SIZE);
  mem[t+1] := n;
  return t
end;

(* |DoCut| -- built-in relation !/0 *)
proc DoCut(): boolean;
begin
  choice := mem[goalframe+3];
  lsp := goalframe + (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE) - 1;
  Commit();
  current := (current)+1;
  return true
end;

(* |DoCall| -- built-in relation |call/1| *)
proc DoCall(): boolean;
begin
  GetArgs();
  if not (lsr(mem[av[1]], 8) = FUNC) then
    newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
    return false
  else
    PushFrame(1, NULL);
    mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
      GloCopy(av[1], mem[goalframe+1]);
    current := callbody;
    return true
  end
end;

(* |DoNot| -- built-in relation |not/1| *)
proc DoNot(): boolean;
  var savebase: frame;
begin
  GetArgs();
  if not (lsr(mem[av[1]], 8) = FUNC) then
    newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
    return false
  else
    PushFrame(1, NULL);
    savebase := base; base := goalframe; choice := goalframe;
    mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
      GloCopy(av[1], mem[goalframe+1]);
    current := callbody;
    Resume(true);
    choice := mem[base+3]; goalframe := mem[base+1];
    if not ok then
      current := (mem[base])+1;
      return true
    else
      Commit();
      return false
    end;
    lsp := base-1; base := savebase
  end
end;

(* |DoPlus| -- built-in relation |plus/3| *)
proc DoPlus(): boolean;
  var result: boolean;
begin
  GetArgs();
  result := false;
  if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
    result := Unify(av[3], goalframe, NewInt(mem[av[1]+1] + mem[av[2]+1]), NULL)
  elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
    if mem[av[1]+1] <= mem[av[3]+1] then
      result := Unify(av[2], goalframe, 
		      NewInt(mem[av[3]+1] - mem[av[1]+1]), NULL)
    end
  elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
    if mem[av[2]+1] <= mem[av[3]+1] then
      result := Unify(av[1], goalframe, NewInt(mem[av[3]+1] - mem[av[2]+1]), NULL)
    end
  else
    newline(); print_string("Error: "); print_string("plus/3 needs at least two integers"); run := false
  end;
  current := (current)+1;
  return result
end;

(* |DoTimes| -- built-in relation |times/3| *)
proc DoTimes(): boolean;
  var result: boolean;
begin
  GetArgs();
  result := false;
  if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
    result := Unify(av[3], goalframe, 
		    NewInt(mem[av[1]+1] * mem[av[2]+1]), NULL)
  elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
    if mem[av[1]+1] <> 0 then
      if mem[av[3]+1] mod mem[av[1]+1] = 0 then
        result := Unify(av[2], goalframe, 
		        NewInt(mem[av[3]+1] div mem[av[1]+1]), NULL)
      end
    end
  elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
    if mem[av[2]+1] <> 0 then
      if mem[av[3]+1] mod mem[av[2]+1] = 0 then
        result := Unify(av[1], goalframe, 
			NewInt(mem[av[3]+1] div mem[av[2]+1]), NULL)
      end
    end
  else
    newline(); print_string("Error: "); print_string("times/3 needs at least two integers"); run := false
  end;
  current := (current)+1;
  return result
end;

(* |DoEqual| -- built-in relation |=/2| *)
proc DoEqual(): boolean;
begin
  GetArgs();
  current := (current)+1;
  return Unify(av[1], goalframe, av[2], goalframe)
end;

(* |DoInteger| -- built-in relation |integer/1| *)
proc DoInteger(): boolean;
begin
  GetArgs();
  current := (current)+1;
  return (lsr(mem[av[1]], 8) = INT)
end;

(* |DoChar| -- built-in relation |char/1| *)
proc DoChar(): boolean;
begin
  GetArgs();
  current := (current)+1;
  return (lsr(mem[av[1]], 8) = CHRCTR)
end;

(* |DoPrint| -- built-in relation |print/1| *)
proc DoPrint(): boolean;
begin
  GetArgs();
  PrintTerm(av[1], goalframe, MAXPRIO);
  current := (current)+1;
  return true
end;

(* |DoNl| -- built-in relation |nl/0| *)
proc DoNl(): boolean;
begin
  newline();
  current := (current)+1;
  return true
end;  

(* |DoBuiltin| -- switch for built-in relations *)
proc DoBuiltin(action: integer): boolean;
begin
  case action of
    CUT:      return DoCut()
  | CALL:     return DoCall()
  | PLUS:     return DoPlus()
  | TIMES:    return DoTimes()
  | ISINT:    return DoInteger()
  | ISCHAR:   return DoChar()
  | NAFF:     return DoNot()
  | EQUALITY: return DoEqual()
  | FAIL:     return false
  | PRINT:    return DoPrint()
  | NL:	      return DoNl()
  else
    newline(); print_string("Panic: "); print_string("bad tag" (*action:1, " in ", "DoBuiltin"*)); newline(); exit(2)
  end
end;

(* |Initialize| -- initialize everything *)
proc Initialize();
  var i: integer; p: term;
begin
  dflag := false; errcount := 0;
  pbchar := ENDFILE; charptr := 0;
  hp := 0; InitSymbols();

  (* Set up the |refnode| array *)
  for i := 1 to MAXARITY do
    p := HeapAlloc(TERM_SIZE);
    mem[p] := lsl(REF, 8) + TERM_SIZE;
    mem[p+1] := i; refnode[i] := p
  end;

  (* The dummy clause $\it call(\sci p) \IF p$ is used by |call/1|. *)
  callbody := HeapAlloc(2);
  mem[callbody] := MakeRef(1);
  mem[(callbody)+1] := NULL
end;

(* |ReadFile| -- read and process clauses from an open file *)
proc ReadFile();
  var c: clause;
    ch: char;
begin
  lineno := 1;
  repeat
    hmark := hp;
    c := ReadClause();
    if c <> NULL then
      if dflag then PrintClause(c) end;	
      if mem[c+3] <> NULL then
        AddClause(c)
      else
        Execute(c);
	hp := hmark
      end
    end
  until c = NULL
end;

begin  (* main program *)
  prog("subject(                                                    ");
  prog("  <store,                                                   ");
  prog("    <load,                                                  ");
  prog("      <plusa,                                               ");
  prog("        <global(a)>,                                        ");
  prog("        <lsl, <load, <local(16)>>, <const(2)>>>>,           ");
  prog("    <local(20)>>                                            ");
  prog(") :- .                                                      ");
                                                                
  prog("rule(""*str"", stmt, <store, reg, addr>) :- .                 ");
  prog("rule(""*ldr"", reg,  <load, addr>) :- .                       ");
  prog("rule(""*addfp"", reg, <local(N)>) :- .                        ");
  prog("rule(""local"", addr, <local(N)>) :- .                        ");
  prog("rule(""*add"", reg, <plusa, reg, rand>) :- .                  ");
  prog("rule(""index"", addr, <plusa, reg, reg>) :- .                 ");
  prog("rule(""scale"", addr,                                         ");
  prog("       <plusa, reg, <lsl, reg, <const(N)>>>) :- .           ");
  prog("rule(""*global"", reg, <global(X)>) :- .                      ");
  prog("rule(""*lsl"", reg, <lsl, reg, rand>) :- .                    ");
  prog("rule(""lshiftc"", rand, <lsl, reg, <const(N)>>) :- .          ");
  prog("rule(""lshiftr"", rand, <lsl, reg, reg>) :- .                 ");
  prog("rule(""*mov"", reg, <const(N)>) :- .                          ");
  prog("rule(""const"", rand, <const(N)>) :- .                        ");
  prog("rule(""reg"", rand, reg) :- .                                 ");
  prog("rule(""indir"", addr, reg) :- .                               ");
                                                                
  prog("use_rule(NT, Tree, node(Name, Kids)) :-                     ");
  prog("  rule(Name, NT, RHS), match(RHS, Tree, Kids, nil).         ");
                                                                
  prog("match(NT, Tree, Parse:Kids0, Kids0) :-                      ");
  prog("  use_rule(NT, Tree, Parse).                                ");

  prog("match(node(W, PS), node(W, TS), Kids, Kids0) :-             ");
  prog("  matchall(PS, TS, Kids, Kids0).                            ");
                                                                
  prog("matchall(nil, nil, Kids0, Kids0) :- .                       ");
  prog("matchall(P:PS, T:TS, Kids, Kids0) :-                        ");
  prog("  match(P, T, Kids, Kids1), matchall(PS, TS, Kids1, Kids0). ");
                                                                
  prog("cost(node(X, TS), C) :-                                     ");
  prog("  opcost(X, A), allcosts(TS, B), plus(A, B, C).             ");
                                                                
  prog("allcosts(nil, 0) :- .                                       ");
  prog("allcosts(T:TS, C) :-                                        ");
  prog("  cost(T, A), allcosts(TS, B), plus(A, B, C).               ");
                                                                
  prog("opcost('*':_, 1) :- !.                                      ");
  prog("opcost(_, 0) :- .                                           ");
                                                                
  prog("answer(P, C) :-                                             ");
  prog("  subject(T), use_rule(stmt, T, P), cost(P, C).             ");
                                                                
  prog("min(N, P) :- min1(N, 0, P).                                 ");
  prog("min1(N, N, P) :- call(P), !.                                ");
  prog("min1(N, N0, P) :- plus(N0, 1, N1), min1(N, N1, P).          ");
                                                                
  prog("# :- answer(P, C).                                          ");

  Initialize();
  ReadFile()
end.

(*<<
P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"local">>, <"const">>>>, <"local">>
C = 5

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"local">>, <"const">>>>, <"indir", <"*addfp">>>
C = 6

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"local">>, <"reg", <"*mov">>>>>, <"local">>
C = 6

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"local">>, <"reg", <"*mov">>>>>, <"indir", <"*addfp">>>
C = 7

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"const">>>>, <"local">>
C = 6

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"const">>>>, <"indir", <"*addfp">>>
C = 7

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"reg", <"*mov">>>>>, <"local">>
C = 7

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"reg", <"*mov">>>>>, <"indir", <"*addfp">>>
C = 8

P = <"*str", <"*ldr", <"scale", <"*global">, <"*ldr", <"local">>>>, <"local">>
C = 4

P = <"*str", <"*ldr", <"scale", <"*global">, <"*ldr", <"local">>>>, <"indir", <"*addfp">>>
C = 5

P = <"*str", <"*ldr", <"scale", <"*global">, <"*ldr", <"indir", <"*addfp">>>>>, <"local">>
C = 5

P = <"*str", <"*ldr", <"scale", <"*global">, <"*ldr", <"indir", <"*addfp">>>>>, <"indir", <"*addfp">>>
C = 6

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftc", <"*ldr", <"local">>>>>>, <"local">>
C = 5

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftc", <"*ldr", <"local">>>>>>, <"indir", <"*addfp">>>
C = 6

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftc", <"*ldr", <"indir", <"*addfp">>>>>>>, <"local">>
C = 6

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftc", <"*ldr", <"indir", <"*addfp">>>>>>>, <"indir", <"*addfp">>>
C = 7

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftr", <"*ldr", <"local">>, <"*mov">>>>>, <"local">>
C = 6

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftr", <"*ldr", <"local">>, <"*mov">>>>>, <"indir", <"*addfp">>>
C = 7

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftr", <"*ldr", <"indir", <"*addfp">>>, <"*mov">>>>>, <"local">>
C = 7

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftr", <"*ldr", <"indir", <"*addfp">>>, <"*mov">>>>>, <"indir", <"*addfp">>>
C = 8

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"local">>, <"const">>>>>>, <"local">>
C = 6

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"local">>, <"const">>>>>>, <"indir", <"*addfp">>>
C = 7

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"local">>, <"reg", <"*mov">>>>>>>, <"local">>
C = 7

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"local">>, <"reg", <"*mov">>>>>>>, <"indir", <"*addfp">>>
C = 8

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"const">>>>>>, <"local">>
C = 7

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"const">>>>>>, <"indir", <"*addfp">>>
C = 8

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"reg", <"*mov">>>>>>>, <"local">>
C = 8

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"reg", <"*mov">>>>>>>, <"indir", <"*addfp">>>
C = 9

>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc StringLength(var s: tempstring): integer;
	.section .text
_StringLength:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   i := 0;
	mov r5, #0
.L2:
@   while s[i] <> ENDSTR do i := i+1 end;
	ldr r0, [fp, #24]
	ldrb r0, [r0, r5]
	cmp r0, #0
	beq .L4
	add r5, r5, #1
	b .L2
.L4:
@   return i
	mov r0, r5
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc SaveString(var s: tempstring): permstring;
_SaveString:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if charptr + StringLength(s) + 1 > MAXCHARS then
	ldr r0, [fp, #40]
	bl _StringLength
	ldr r1, =_charptr
	ldr r1, [r1]
	add r0, r1, r0
	add r0, r0, #1
	cmp r0, #2048
	ble .L8
@     newline(); print_string("Panic: "); print_string("out of string space"); newline(); exit(2)
	bl newline
	mov r1, #8
	ldr r0, =g1
	bl print_string
	mov r1, #20
	ldr r0, =g2
	bl print_string
	bl newline
	mov r0, #2
	bl exit
.L8:
@   p := charptr; i := 0;
	ldr r0, =_charptr
	ldr r5, [r0]
	mov r6, #0
.L9:
@     charbuf[charptr] := s[i]; charptr := charptr+1; i := i+1
	ldr r7, =_charbuf
	ldr r8, =_charptr
	ldr r0, [fp, #40]
	ldrb r0, [r0, r6]
	ldr r1, [r8]
	strb r0, [r7, r1]
	ldr r0, [r8]
	add r9, r0, #1
	str r9, [r8]
	add r6, r6, #1
	add r0, r7, r9
	ldrb r0, [r0, #-1]
	cmp r0, #0
	bne .L9
@   return p
	mov r0, r5
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc StringEqual(var s1: tempstring; s2: permstring): boolean;
_StringEqual:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   i := 0;
	mov r5, #0
.L12:
@   while (s1[i] <> ENDSTR) and (s1[i] = charbuf[s2+i]) do i := i+1 end;
	ldr r0, [fp, #24]
	ldrb r6, [r0, r5]
	cmp r6, #0
	beq .L14
	ldr r0, =_charbuf
	ldr r1, [fp, #28]
	add r0, r0, r1
	ldrb r0, [r0, r5]
	cmp r6, r0
	bne .L14
	add r5, r5, #1
	b .L12
.L14:
@   return (s1[i] = charbuf[s2+i])
	ldr r0, [fp, #24]
	ldrb r0, [r0, r5]
	ldr r1, =_charbuf
	ldr r2, [fp, #28]
	add r1, r1, r2
	ldrb r1, [r1, r5]
	cmp r0, r1
	mov r0, #0
	moveq r0, #1
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc WriteString(s: permstring);
_WriteString:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   i := s;
	ldr r5, [fp, #24]
.L17:
@   while charbuf[i] <> ENDSTR do
	ldr r0, =_charbuf
	ldrb r6, [r0, r5]
	cmp r6, #0
	beq .L16
@     print_char(charbuf[i]); i := i+1
	mov r0, r6
	bl print_char
	add r5, r5, #1
	b .L17
.L16:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc LocAlloc(size: integer): ptr;
_LocAlloc:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   if lsp + size >= gsp then newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2) end;
	ldr r0, =_lsp
	ldr r0, [r0]
	ldr r1, [fp, #32]
	add r0, r0, r1
	ldr r1, =_gsp
	ldr r1, [r1]
	cmp r0, r1
	blt .L23
	bl newline
	mov r1, #8
	ldr r0, =g3
	bl print_string
	mov r1, #19
	ldr r0, =g4
	bl print_string
	bl newline
	mov r0, #2
	bl exit
.L23:
@   p := lsp + 1; lsp := lsp + size; return p
	ldr r6, =_lsp
	ldr r7, [r6]
	add r5, r7, #1
	ldr r0, [fp, #32]
	add r0, r7, r0
	str r0, [r6]
	mov r0, r5
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc GloAlloc(kind, size: integer): ptr;
_GloAlloc:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   if gsp - size <= lsp then
	ldr r0, =_gsp
	ldr r0, [r0]
	ldr r1, [fp, #36]
	sub r0, r0, r1
	ldr r1, =_lsp
	ldr r1, [r1]
	cmp r0, r1
	bgt .L27
@     newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2)
	bl newline
	mov r1, #8
	ldr r0, =g5
	bl print_string
	mov r1, #19
	ldr r0, =g6
	bl print_string
	bl newline
	mov r0, #2
	bl exit
.L27:
@   gsp := gsp - size; p := gsp;
	ldr r6, =_gsp
	ldr r7, [fp, #36]
	ldr r0, [r6]
	sub r8, r0, r7
	str r8, [r6]
	mov r5, r8
@   mem[p] := lsl(kind, 8) + size;
	ldr r0, [fp, #32]
	lsl r0, r0, #8
	add r0, r0, r7
	ldr r1, =_mem
	str r0, [r1, r5, LSL #2]
@   return p
	mov r0, r5
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc HeapAlloc(size: integer): ptr;
_HeapAlloc:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   if hp + size > MEMSIZE then newline(); print_string("Panic: "); print_string("out of heap space"); newline(); exit(2) end;
	ldr r0, =_hp
	ldr r0, [r0]
	ldr r1, [fp, #32]
	add r0, r0, r1
	ldr r1, =25000
	cmp r0, r1
	ble .L31
	bl newline
	mov r1, #8
	ldr r0, =g7
	bl print_string
	mov r1, #18
	ldr r0, =g8
	bl print_string
	bl newline
	mov r0, #2
	bl exit
.L31:
@   p := hp + 1; hp := hp + size; return p
	ldr r6, =_hp
	ldr r7, [r6]
	add r5, r7, #1
	ldr r0, [fp, #32]
	add r0, r7, r0
	str r0, [r6]
	mov r0, r5
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc prog(line: array 61 of char);
_prog:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   for i := 0 to 59 do
	mov r5, #0
	mov r6, #59
.L33:
	cmp r5, r6
	bgt .L34
@     infile[pin] := line[i]; pin := pin+1
	ldr r7, =_pin
	ldr r0, [fp, #32]
	ldrb r0, [r0, r5]
	ldr r1, =_infile
	ldr r2, [r7]
	strb r0, [r1, r2]
	ldr r0, [r7]
	add r0, r0, #1
	str r0, [r7]
	add r5, r5, #1
	b .L33
.L34:
@   infile[pin] := ENDLINE; pin := pin+1
	ldr r7, =_pin
	mov r0, #10
	ldr r1, =_infile
	ldr r2, [r7]
	strb r0, [r1, r2]
	ldr r0, [r7]
	add r0, r0, #1
	str r0, [r7]
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc rdchar(var ch: char);
_rdchar:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   if pout >= pin then
	ldr r0, =_pout
	ldr r0, [r0]
	ldr r1, =_pin
	ldr r1, [r1]
	cmp r0, r1
	blt .L37
@     ch := ENDFILE
	mov r0, #127
	ldr r1, [fp, #24]
	strb r0, [r1]
	b .L35
.L37:
@     ch := infile[pout]; pout := pout+1
	ldr r5, =_pout
	ldr r0, =_infile
	ldr r1, [r5]
	ldrb r0, [r0, r1]
	ldr r1, [fp, #24]
	strb r0, [r1]
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
.L35:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc GetChar(): char;
_GetChar:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #8
@   if pbchar <> ENDFILE then
	ldr r5, =_pbchar
	ldrb r6, [r5]
	cmp r6, #127
	beq .L41
@     ch := pbchar; pbchar := ENDFILE
	strb r6, [fp, #-1]
	mov r0, #127
	strb r0, [r5]
	b .L42
.L41:
@     rdchar(ch);
	add r0, fp, #-1
	bl _rdchar
@     if ch = ENDLINE then lineno := lineno+1 end
	ldrb r0, [fp, #-1]
	cmp r0, #10
	bne .L42
	ldr r5, =_lineno
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
.L42:
@   return ch
	ldrb r0, [fp, #-1]
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc PushBack(ch: char);
_PushBack:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   pbchar := ch
	ldrb r0, [fp, #16]
	ldr r1, =_pbchar
	strb r0, [r1]
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

@ proc Deref(t: term; e: frame): term;
_Deref:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   if t = NULL then newline(); print_string("Panic: "); print_string("Deref"); newline(); exit(2) end;
	ldr r0, [fp, #32]
	cmp r0, #0
	bne .L50
	bl newline
	mov r1, #8
	ldr r0, =g9
	bl print_string
	mov r1, #6
	ldr r0, =g10
	bl print_string
	bl newline
	mov r0, #2
	bl exit
.L50:
@   if (lsr(mem[t], 8) = REF) and (e <> NULL) then
	ldr r5, =_mem
	ldr r6, [fp, #32]
	ldr r0, [r5, r6, LSL #2]
	lsr r0, r0, #8
	cmp r0, #5
	bne .L55
	ldr r7, [fp, #36]
	cmp r7, #0
	beq .L55
@     t := (e+7+(mem[t+1]-1)*TERM_SIZE)
	add r0, r7, #7
	add r1, r5, r6, LSL #2
	ldr r1, [r1, #4]
	lsl r1, r1, #1
	sub r1, r1, #2
	add r0, r0, r1
	str r0, [fp, #32]
.L55:
@   while (lsr(mem[t], 8) = CELL) and (mem[t+1] <> NULL) do
	ldr r5, =_mem
	ldr r6, [fp, #32]
	ldr r0, [r5, r6, LSL #2]
	lsr r0, r0, #8
	cmp r0, #4
	bne .L57
	add r0, r5, r6, LSL #2
	ldr r5, [r0, #4]
	cmp r5, #0
	beq .L57
@     t := mem[t+1]
	str r5, [fp, #32]
	b .L55
.L57:
@   return t
	ldr r0, [fp, #32]
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc Lookup(var name: tempstring): symbol;
_Lookup:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   h := 0; i := 0;
	mov r5, #0
	mov r6, #0
.L60:
@   while name[i] <> ENDSTR do
	ldr r0, [fp, #32]
	ldrb r8, [r0, r6]
	cmp r8, #0
	beq .L62
@     h := (5 * h + ord(name[i])) mod MAXSYMBOLS; i := i+1 
	mov r0, #5
	mul r0, r5, r0
	add r0, r0, r8
	ldr r1, =511
	and r5, r0, r1
	add r6, r6, #1
	b .L60
.L62:
@   p := h+1;
	add r7, r5, #1
.L63:
@   while symtab[p].name <> -1 do
	ldr r0, =_symtab
	add r0, r0, r7, LSL #4
	ldr r8, [r0]
	cmp r8, #-1
	beq .L65
@     if StringEqual(name, symtab[p].name) then return p end;
	mov r1, r8
	ldr r0, [fp, #32]
	bl _StringEqual
	cmp r0, #0
	beq .L68
	mov r0, r7
	b .L59
.L68:
@     p := p-1;
	sub r7, r7, #1
@     if p = 0 then p := MAXSYMBOLS end
	cmp r7, #0
	bne .L63
	mov r7, #512
	b .L63
.L65:
@   if nsymbols >= (MAXSYMBOLS div 10) * (HASHFACTOR div 10) then
	ldr r0, =_nsymbols
	ldr r0, [r0]
	ldr r1, =459
	cmp r0, r1
	blt .L74
@     newline(); print_string("Panic: "); print_string("out of symbol space"); newline(); exit(2)
	bl newline
	mov r1, #8
	ldr r0, =g11
	bl print_string
	mov r1, #20
	ldr r0, =g12
	bl print_string
	bl newline
	mov r0, #2
	bl exit
.L74:
@   symtab[p].name := SaveString(name);
	ldr r0, [fp, #32]
	bl _SaveString
	ldr r1, =_symtab
	add r8, r1, r7, LSL #4
	str r0, [r8]
@   symtab[p].arity := -1;
	mov r0, #-1
	str r0, [r8, #4]
@   symtab[p].action := 0; symtab[p].prok := NULL;
	mov r0, #0
	str r0, [r8, #8]
	mov r0, #0
	str r0, [r8, #12]
@   return p
	mov r0, r7
.L59:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc Enter(name: keyword; arity: integer; action: integer): symbol;
_Enter:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #128
@   i := 0;
	mov r6, #0
.L76:
@   while name[i] <> ' ' do
	ldr r0, [fp, #32]
	ldrb r7, [r0, r6]
	cmp r7, #32
	beq .L78
@     temp[i] := name[i]; i := i+1 
	add r0, fp, #-128
	strb r7, [r0, r6]
	add r6, r6, #1
	b .L76
.L78:
@   temp[i] := ENDSTR; s := Lookup(temp);
	add r7, fp, #-128
	mov r0, #0
	strb r0, [r7, r6]
	mov r0, r7
	bl _Lookup
	mov r5, r0
@   symtab[s].arity := arity; symtab[s].action := action;
	ldr r0, =_symtab
	add r7, r0, r5, LSL #4
	ldr r0, [fp, #36]
	str r0, [r7, #4]
	ldr r0, [fp, #40]
	str r0, [r7, #8]
@   return s
	mov r0, r5
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc InitSymbols();
_InitSymbols:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   nsymbols := 0;
	mov r0, #0
	ldr r1, =_nsymbols
	str r0, [r1]
@   for i := 1 to MAXSYMBOLS do symtab[i].name := -1 end;
	mov r5, #1
	mov r7, #512
.L80:
	cmp r5, r7
	bgt .L81
	mov r0, #-1
	ldr r1, =_symtab
	add r1, r1, r5, LSL #4
	str r0, [r1]
	add r5, r5, #1
	b .L80
.L81:
@   cons   := Enter(":       ", 2, 0);
	mov r2, #0
	mov r1, #2
	ldr r0, =g13
	bl _Enter
	ldr r1, =_cons
	str r0, [r1]
@   cutsym := Enter("!       ", 0, CUT);
	mov r2, #1
	mov r1, #0
	ldr r0, =g14
	bl _Enter
	ldr r1, =_cutsym
	str r0, [r1]
@   eqsym  := Enter("=       ", 2, EQUALITY);
	mov r2, #8
	mov r1, #2
	ldr r0, =g15
	bl _Enter
	ldr r1, =_eqsym
	str r0, [r1]
@   nilsym := Enter("nil     ", 0, 0);
	mov r2, #0
	mov r1, #0
	ldr r0, =g16
	bl _Enter
	ldr r1, =_nilsym
	str r0, [r1]
@   notsym := Enter("not     ", 1, NAFF);
	mov r2, #7
	mov r1, #1
	ldr r0, =g17
	bl _Enter
	ldr r1, =_notsym
	str r0, [r1]
@   node   := Enter("node    ", 2, 0);
	mov r2, #0
	mov r1, #2
	ldr r0, =g18
	bl _Enter
	ldr r1, =_node
	str r0, [r1]
@   dummy  := Enter("call    ", 1, CALL);
	mov r2, #2
	mov r1, #1
	ldr r0, =g19
	bl _Enter
	mov r6, r0
@   dummy  := Enter("plus    ", 3, PLUS);
	mov r2, #3
	mov r1, #3
	ldr r0, =g20
	bl _Enter
	mov r6, r0
@   dummy  := Enter("times   ", 3, TIMES);
	mov r2, #4
	mov r1, #3
	ldr r0, =g21
	bl _Enter
	mov r6, r0
@   dummy  := Enter("integer ", 1, ISINT);
	mov r2, #5
	mov r1, #1
	ldr r0, =g22
	bl _Enter
	mov r6, r0
@   dummy  := Enter("char    ", 1, ISCHAR);
	mov r2, #6
	mov r1, #1
	ldr r0, =g23
	bl _Enter
	mov r6, r0
@   dummy  := Enter("false   ", 0, FAIL);
	mov r2, #9
	mov r1, #0
	ldr r0, =g24
	bl _Enter
	mov r6, r0
@   dummy  := Enter("print   ", 1, PRINT);
	mov r2, #10
	mov r1, #1
	ldr r0, =g25
	bl _Enter
	mov r6, r0
@   dummy  := Enter("nl      ", 0, NL)
	mov r2, #11
	mov r1, #0
	ldr r0, =g26
	bl _Enter
	mov r6, r0
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc AddClause(c: clause);
_AddClause:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   s := mem[mem[c+3]+1];
	ldr r7, =_mem
	ldr r0, [fp, #32]
	add r0, r7, r0, LSL #2
	ldr r0, [r0, #12]
	add r0, r7, r0, LSL #2
	ldr r5, [r0, #4]
@   if symtab[s].action <> 0 then
	ldr r7, =_symtab
	add r0, r7, r5, LSL #4
	ldr r0, [r0, #8]
	cmp r0, #0
	beq .L84
@     newline(); print_string("Error: "); print_string("cannot add clauses to built-in relation "); run := false;
	bl newline
	mov r1, #8
	ldr r0, =g27
	bl print_string
	mov r1, #41
	ldr r0, =g28
	bl print_string
	mov r0, #0
	ldr r1, =_run
	strb r0, [r1]
@     WriteString(symtab[s].name)
	add r0, r7, r5, LSL #4
	ldr r0, [r0]
	bl _WriteString
	b .L82
.L84:
@   elsif symtab[s].prok = NULL then
	ldr r0, =_symtab
	add r7, r0, r5, LSL #4
	ldr r8, =12
	ldr r0, [r7, r8]
	cmp r0, #0
	bne .L87
@     symtab[s].prok := c
	ldr r0, [fp, #32]
	str r0, [r7, r8]
	b .L82
.L87:
@     p := symtab[s].prok;
	ldr r0, =_symtab
	add r0, r0, r5, LSL #4
	ldr r6, [r0, #12]
.L89:
@     while mem[p+2] <> NULL do p := mem[p+2] end;
	ldr r0, =_mem
	add r0, r0, r6, LSL #2
	ldr r7, [r0, #8]
	cmp r7, #0
	beq .L91
	mov r6, r7
	b .L89
.L91:
@     mem[p+2] := c
	ldr r0, [fp, #32]
	ldr r1, =_mem
	add r1, r1, r6, LSL #2
	str r0, [r1, #8]
.L82:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc MakeCompound(fun: symbol; var arg: argbuf): term;
_MakeCompound:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #8
@   n := symtab[fun].arity;
	ldr r0, =_symtab
	ldr r1, [fp, #32]
	add r0, r0, r1, LSL #4
	ldr r7, [r0, #4]
@   p := HeapAlloc(TERM_SIZE+n);
	add r0, r7, #2
	bl _HeapAlloc
	mov r5, r0
@   mem[p] := lsl(FUNC, 8) + TERM_SIZE+n;
	ldr r8, =_mem
	ldr r0, =258
	add r0, r7, r0
	str r0, [r8, r5, LSL #2]
@   mem[p+1] := fun;
	ldr r0, [fp, #32]
	add r1, r8, r5, LSL #2
	str r0, [r1, #4]
@   for i := 1 to n do mem[p+i+1] := arg[i] end;
	mov r6, #1
	str r7, [fp, #-4]
.L93:
	ldr r0, [fp, #-4]
	cmp r6, r0
	bgt .L94
	ldr r0, [fp, #36]
	ldr r0, [r0, r6, LSL #2]
	ldr r1, =_mem
	add r2, r5, r6
	add r1, r1, r2, LSL #2
	str r0, [r1, #4]
	add r6, r6, #1
	b .L93
.L94:
@   return p
	mov r0, r5
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc MakeNode(fun: symbol; a1, a2: term): term;
_MakeNode:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #256
@   arg[1] := a1; arg[2] := a2;
	ldr r0, [fp, #20]
	str r0, [fp, #-252]
	ldr r0, [fp, #24]
	str r0, [fp, #-248]
@   return MakeCompound(fun, arg)
	add r1, fp, #-256
	ldr r0, [fp, #16]
	bl _MakeCompound
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

@ proc MakeRef(offset: integer): term;
_MakeRef:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   return refnode[offset]
	ldr r0, =_refnode
	ldr r1, [fp, #16]
	ldr r0, [r0, r1, LSL #2]
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

@ proc MakeInt(i: integer): term;
_MakeInt:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   p := HeapAlloc(TERM_SIZE);
	mov r0, #2
	bl _HeapAlloc
	mov r5, r0
@   mem[p] := lsl(INT, 8) + TERM_SIZE;
	ldr r6, =_mem
	ldr r0, =514
	str r0, [r6, r5, LSL #2]
@   mem[p+1] := i; return p
	ldr r0, [fp, #24]
	add r1, r6, r5, LSL #2
	str r0, [r1, #4]
	mov r0, r5
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc MakeChar(c: char): term;
_MakeChar:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   p := HeapAlloc(TERM_SIZE);
	mov r0, #2
	bl _HeapAlloc
	mov r5, r0
@   mem[p] := lsl(CHRCTR, 8) + TERM_SIZE;
	ldr r6, =_mem
	ldr r0, =770
	str r0, [r6, r5, LSL #2]
@   mem[p+1] := ord(c); return p
	ldrb r0, [fp, #24]
	add r1, r6, r5, LSL #2
	str r0, [r1, #4]
	mov r0, r5
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc MakeString(var s: tempstring): term;
_MakeString:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   i := StringLength(s);
	ldr r0, [fp, #24]
	bl _StringLength
	mov r6, r0
@   p := MakeNode(nilsym, NULL, NULL);
	mov r2, #0
	mov r1, #0
	ldr r0, =_nilsym
	ldr r0, [r0]
	bl _MakeNode
	mov r5, r0
.L100:
@   while i > 0 do
	cmp r6, #0
	ble .L102
@     i := i-1; p := MakeNode(cons, MakeChar(s[i]), p)
	sub r6, r6, #1
	ldr r0, [fp, #24]
	ldrb r0, [r0, r6]
	bl _MakeChar
	mov r2, r5
	mov r1, r0
	ldr r0, =_cons
	ldr r0, [r0]
	bl _MakeNode
	mov r5, r0
	b .L100
.L102:
@   return p
	mov r0, r5
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc MakeClause(nvars: integer; head: term;
_MakeClause:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   p := HeapAlloc(CLAUSE_SIZE + nbody + 1);
	ldr r0, [fp, #44]
	add r0, r0, #5
	bl _HeapAlloc
	mov r5, r0
@   mem[p] := nvars; mem[p+2] := NULL; mem[p+3] := head;
	ldr r8, =_mem
	ldr r0, [fp, #32]
	str r0, [r8, r5, LSL #2]
	add r8, r8, r5, LSL #2
	mov r0, #0
	str r0, [r8, #8]
	ldr r0, [fp, #36]
	str r0, [r8, #12]
@   for i := 1 to nbody do mem[(p+4)+i-1] := body[i] end;
	mov r6, #1
	ldr r7, [fp, #44]
.L104:
	cmp r6, r7
	bgt .L105
	ldr r0, [fp, #40]
	ldr r0, [r0, r6, LSL #2]
	ldr r1, =_mem
	add r2, r5, #4
	add r2, r2, r6
	add r1, r1, r2, LSL #2
	str r0, [r1, #-4]
	add r6, r6, #1
	b .L104
.L105:
@   mem[(p+4)+nbody+1-1] := NULL;
	ldr r8, =_mem
	mov r0, #0
	add r1, r5, #4
	ldr r2, [fp, #44]
	add r1, r1, r2
	str r0, [r8, r1, LSL #2]
@   if head = NULL then 
	ldr r0, [fp, #36]
	cmp r0, #0
	bne .L107
@     mem[p+1] := 0
	mov r0, #0
	add r1, r8, r5, LSL #2
	str r0, [r1, #4]
	b .L108
.L107:
@     mem[p+1] := Key(head, NULL)
	mov r1, #0
	ldr r0, [fp, #36]
	bl _Key
	ldr r1, =_mem
	add r1, r1, r5, LSL #2
	str r0, [r1, #4]
.L108:
@   return p
	mov r0, r5
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc IsString(t: term; e: frame): boolean;
_IsString:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   i := 0; t := Deref(t, e);
	mov r5, #0
	ldr r1, [fp, #36]
	ldr r0, [fp, #32]
	bl _Deref
	str r0, [fp, #32]
.L110:
@   while i < limit do
	cmp r5, #128
	bge .L112
@     if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
	ldr r6, =_mem
	ldr r7, [fp, #32]
	ldr r0, [r6, r7, LSL #2]
	lsr r0, r0, #8
	cmp r0, #1
	bne .L113
	add r0, r6, r7, LSL #2
	ldr r0, [r0, #4]
	ldr r1, =_cons
	ldr r1, [r1]
	cmp r0, r1
	beq .L114
.L113:
@       return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
	ldr r6, =_mem
	ldr r7, [fp, #32]
	ldr r0, [r6, r7, LSL #2]
	lsr r0, r0, #8
	cmp r0, #1
	bne .L120
	add r0, r6, r7, LSL #2
	ldr r0, [r0, #4]
	ldr r1, =_nilsym
	ldr r1, [r1]
	cmp r0, r1
	mov r6, #0
	moveq r6, #1
	b .L121
.L120:
	mov r6, #0
.L121:
	mov r0, r6
	b .L109
.L114:
@     elsif lsr(mem[Deref(mem[t+1+1], e)], 8) <> CHRCTR then
	ldr r6, =_mem
	ldr r1, [fp, #36]
	ldr r0, [fp, #32]
	add r0, r6, r0, LSL #2
	ldr r0, [r0, #8]
	bl _Deref
	ldr r0, [r6, r0, LSL #2]
	lsr r0, r0, #8
	cmp r0, #3
	beq .L117
@       return false
	mov r0, #0
	b .L109
.L117:
@       i := i+1; t := Deref(mem[t+2+1], e) 
	add r5, r5, #1
	ldr r1, [fp, #36]
	ldr r0, =_mem
	ldr r2, [fp, #32]
	add r0, r0, r2, LSL #2
	ldr r0, [r0, #12]
	bl _Deref
	str r0, [fp, #32]
	b .L110
.L112:
@   return false
	mov r0, #0
.L109:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc IsList(t: term; e: frame): boolean;
_IsList:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   i := 0; t := Deref(t, e);
	mov r5, #0
	ldr r1, [fp, #36]
	ldr r0, [fp, #32]
	bl _Deref
	str r0, [fp, #32]
.L124:
@   while i < limit do
	cmp r5, #128
	bge .L126
@     if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
	ldr r6, =_mem
	ldr r7, [fp, #32]
	ldr r0, [r6, r7, LSL #2]
	lsr r0, r0, #8
	cmp r0, #1
	bne .L127
	add r0, r6, r7, LSL #2
	ldr r0, [r0, #4]
	ldr r1, =_cons
	ldr r1, [r1]
	cmp r0, r1
	beq .L128
.L127:
@       return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
	ldr r6, =_mem
	ldr r7, [fp, #32]
	ldr r0, [r6, r7, LSL #2]
	lsr r0, r0, #8
	cmp r0, #1
	bne .L131
	add r0, r6, r7, LSL #2
	ldr r0, [r0, #4]
	ldr r1, =_nilsym
	ldr r1, [r1]
	cmp r0, r1
	mov r6, #0
	moveq r6, #1
	b .L132
.L131:
	mov r6, #0
.L132:
	mov r0, r6
	b .L123
.L128:
@       i := i+1; t := Deref(mem[t+2+1], e)
	add r5, r5, #1
	ldr r1, [fp, #36]
	ldr r0, =_mem
	ldr r2, [fp, #32]
	add r0, r0, r2, LSL #2
	ldr r0, [r0, #12]
	bl _Deref
	str r0, [fp, #32]
	b .L124
.L126:
@   return false
	mov r0, #0
.L123:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc ShowString(t: term; e: frame);
_ShowString:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   t := Deref(t, e);
	ldr r1, [fp, #28]
	ldr r0, [fp, #24]
	bl _Deref
	str r0, [fp, #24]
@   print_char('"');
	mov r0, #34
	bl print_char
.L135:
@   while mem[t+1] <> nilsym do
	ldr r5, =_mem
	ldr r0, [fp, #24]
	add r6, r5, r0, LSL #2
	ldr r0, [r6, #4]
	ldr r1, =_nilsym
	ldr r1, [r1]
	cmp r0, r1
	beq .L137
@     print_char(chr(mem[Deref(mem[t+1+1], e)+1]));
	ldr r1, [fp, #28]
	ldr r0, [r6, #8]
	bl _Deref
	mov r6, r0
	add r0, r5, r6, LSL #2
	ldr r0, [r0, #4]
	bl print_char
@     t := Deref(mem[t+2+1], e)
	ldr r1, [fp, #28]
	ldr r0, [fp, #24]
	add r0, r5, r0, LSL #2
	ldr r0, [r0, #12]
	bl _Deref
	str r0, [fp, #24]
	b .L135
.L137:
@   print_char('"')
	mov r0, #34
	bl print_char
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc PrintCompound(t: term; e: frame; prio: integer);
_PrintCompound:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   f := mem[t+1];
	ldr r8, [fp, #40]
	ldr r0, =_mem
	add r0, r0, r8, LSL #2
	ldr r5, [r0, #4]
@   if f = cons then
	ldr r0, =_cons
	ldr r0, [r0]
	cmp r5, r0
	bne .L140
@     if IsString(t, e) then
	ldr r1, [fp, #44]
	mov r0, r8
	bl _IsString
	cmp r0, #0
	beq .L164
@       ShowString(t, e)
	ldr r1, [fp, #44]
	ldr r0, [fp, #40]
	bl _ShowString
	b .L138
.L164:
@       if prio < CONSPRIO then print_char('(') end;
	ldr r0, [fp, #48]
	cmp r0, #1
	bge .L168
	mov r0, #40
	bl print_char
.L168:
@       PrintTerm(mem[t+1+1], e, CONSPRIO-1);
	ldr r8, =_mem
	mov r2, #0
	ldr r1, [fp, #44]
	ldr r0, [fp, #40]
	add r0, r8, r0, LSL #2
	ldr r0, [r0, #8]
	bl _PrintTerm
@       print_char(':');
	mov r0, #58
	bl print_char
@       PrintTerm(mem[t+2+1], e, CONSPRIO);
	mov r2, #1
	ldr r1, [fp, #44]
	ldr r0, [fp, #40]
	add r0, r8, r0, LSL #2
	ldr r0, [r0, #12]
	bl _PrintTerm
@       if prio < CONSPRIO then print_char(')') end
	ldr r0, [fp, #48]
	cmp r0, #1
	bge .L138
	mov r0, #41
	bl print_char
	b .L138
.L140:
@   elsif f = eqsym then
	ldr r0, =_eqsym
	ldr r0, [r0]
	cmp r5, r0
	bne .L143
@     if prio < EQPRIO then print_char('(') end;
	ldr r0, [fp, #48]
	cmp r0, #2
	bge .L159
	mov r0, #40
	bl print_char
.L159:
@     PrintTerm(mem[t+1+1], e, EQPRIO-1);
	ldr r8, =_mem
	mov r2, #1
	ldr r1, [fp, #44]
	ldr r0, [fp, #40]
	add r0, r8, r0, LSL #2
	ldr r0, [r0, #8]
	bl _PrintTerm
@     print_string(" = ");
	mov r1, #4
	ldr r0, =g29
	bl print_string
@     PrintTerm(mem[t+2+1], e, EQPRIO-1);
	mov r2, #1
	ldr r1, [fp, #44]
	ldr r0, [fp, #40]
	add r0, r8, r0, LSL #2
	ldr r0, [r0, #12]
	bl _PrintTerm
@     if prio < EQPRIO then print_char(')') end
	ldr r0, [fp, #48]
	cmp r0, #2
	bge .L138
	mov r0, #41
	bl print_char
	b .L138
.L143:
@   elsif f = notsym then
	ldr r0, =_notsym
	ldr r0, [r0]
	cmp r5, r0
	bne .L146
@     print_string("not ");
	mov r1, #5
	ldr r0, =g30
	bl print_string
@     PrintTerm(mem[t+1+1], e, MAXPRIO)
	mov r2, #2
	ldr r1, [fp, #44]
	ldr r0, =_mem
	ldr r3, [fp, #40]
	add r0, r0, r3, LSL #2
	ldr r0, [r0, #8]
	bl _PrintTerm
	b .L138
.L146:
@   elsif (f = node) and IsList(mem[t+2+1], e) then
	ldr r0, =_node
	ldr r0, [r0]
	cmp r5, r0
	bne .L149
	ldr r1, [fp, #44]
	ldr r0, =_mem
	ldr r2, [fp, #40]
	add r0, r0, r2, LSL #2
	ldr r0, [r0, #12]
	bl _IsList
	cmp r0, #0
	beq .L149
@     PrintNode(t, e)
	ldr r1, [fp, #44]
	ldr r0, [fp, #40]
	bl _PrintNode
	b .L138
.L149:
@     WriteString(symtab[f].name);
	ldr r8, =_symtab
	add r0, r8, r5, LSL #4
	ldr r0, [r0]
	bl _WriteString
@     if symtab[f].arity > 0 then
	ldr r9, =4
	add r0, r8, r5, LSL #4
	ldr r0, [r0, r9]
	cmp r0, #0
	ble .L138
@       print_char('(');
	mov r0, #40
	bl print_char
@       PrintTerm(mem[t+1+1], e, ARGPRIO);
	mov r2, #2
	ldr r1, [fp, #44]
	ldr r0, =_mem
	ldr r3, [fp, #40]
	add r0, r0, r3, LSL #2
	ldr r0, [r0, #8]
	bl _PrintTerm
@       for i := 2 to symtab[f].arity do
	mov r6, #2
	add r0, r8, r5, LSL #4
	ldr r7, [r0, r9]
.L154:
	cmp r6, r7
	bgt .L155
@         print_string(", ");
	mov r1, #3
	ldr r0, =g31
	bl print_string
@         PrintTerm(mem[t+i+1], e, ARGPRIO)
	mov r2, #2
	ldr r1, [fp, #44]
	ldr r0, =_mem
	ldr r3, [fp, #40]
	add r3, r3, r6
	add r0, r0, r3, LSL #2
	ldr r0, [r0, #4]
	bl _PrintTerm
	add r6, r6, #1
	b .L154
.L155:
@       print_char(')')
	mov r0, #41
	bl print_char
.L138:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc PrintNode(t: term; e: frame);
_PrintNode:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   print_char('<');
	mov r0, #60
	bl print_char
@   PrintTerm(mem[t+1+1], e, MAXPRIO);
	ldr r6, =_mem
	mov r2, #2
	ldr r1, [fp, #28]
	ldr r0, [fp, #24]
	add r0, r6, r0, LSL #2
	ldr r0, [r0, #8]
	bl _PrintTerm
@   u := Deref(mem[t+2+1], e);
	ldr r1, [fp, #28]
	ldr r0, [fp, #24]
	add r0, r6, r0, LSL #2
	ldr r0, [r0, #12]
	bl _Deref
	mov r5, r0
.L173:
@   while mem[u+1] <> nilsym do
	ldr r6, =_mem
	add r0, r6, r5, LSL #2
	ldr r0, [r0, #4]
	ldr r1, =_nilsym
	ldr r1, [r1]
	cmp r0, r1
	beq .L175
@     print_string(", ");
	mov r1, #3
	ldr r0, =g32
	bl print_string
@     PrintTerm(mem[u+1+1], e, MAXPRIO);
	mov r2, #2
	ldr r1, [fp, #28]
	add r0, r6, r5, LSL #2
	ldr r0, [r0, #8]
	bl _PrintTerm
@     u := Deref(mem[u+2+1], e)
	ldr r1, [fp, #28]
	add r0, r6, r5, LSL #2
	ldr r0, [r0, #12]
	bl _Deref
	mov r5, r0
	b .L173
.L175:
@   print_char('>');
	mov r0, #62
	bl print_char
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc PrintTerm(t: term; e: frame; prio: integer);
_PrintTerm:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   t := Deref(t, e);
	ldr r1, [fp, #20]
	ldr r0, [fp, #16]
	bl _Deref
	str r0, [fp, #16]
@   if t = NULL then
	cmp r0, #0
	bne .L178
@     print_string("*null-term*")
	mov r1, #12
	ldr r0, =g33
	bl print_string
	b .L176
.L178:
@     case lsr(mem[t], 8) of
	ldr r0, =_mem
	ldr r1, [fp, #16]
	ldr r0, [r0, r1, LSL #2]
	lsr r0, r0, #8
	sub r0, r0, #1
	cmp r0, #5
	ldrlo pc, [pc, r0, LSL #2]
	b .L180
	.word .L182
	.word .L183
	.word .L184
	.word .L185
	.word .L186
.L182:
@         PrintCompound(t, e, prio)
	ldr r2, [fp, #24]
	ldr r1, [fp, #20]
	ldr r0, [fp, #16]
	bl _PrintCompound
	b .L176
.L183:
@         print_num(mem[t+1])
	ldr r0, =_mem
	ldr r1, [fp, #16]
	add r0, r0, r1, LSL #2
	ldr r0, [r0, #4]
	bl print_num
	b .L176
.L184:
@         print_char(''''); print_char(chr(mem[t+1])); print_char('''')
	mov r0, #39
	bl print_char
	ldr r0, =_mem
	ldr r1, [fp, #16]
	add r0, r0, r1, LSL #2
	ldr r0, [r0, #4]
	bl print_char
	mov r0, #39
	bl print_char
	b .L176
.L185:
@         if (t >= gsp) then
	ldr r0, [fp, #16]
	ldr r1, =_gsp
	ldr r1, [r1]
	cmp r0, r1
	blt .L188
@           print_char('G'); print_num((MEMSIZE - t) div TERM_SIZE)
	mov r0, #71
	bl print_char
	ldr r0, =25000
	ldr r1, [fp, #16]
	sub r0, r0, r1
	asr r0, r0, #1
	bl print_num
	b .L176
.L188:
@           print_char('L'); print_num((t - hp) div TERM_SIZE)
	mov r0, #76
	bl print_char
	ldr r0, [fp, #16]
	ldr r1, =_hp
	ldr r1, [r1]
	sub r0, r0, r1
	asr r0, r0, #1
	bl print_num
	b .L176
.L186:
@         print_char('@'); print_num(mem[t+1])
	mov r0, #64
	bl print_char
	ldr r0, =_mem
	ldr r1, [fp, #16]
	add r0, r0, r1, LSL #2
	ldr r0, [r0, #4]
	bl print_num
	b .L176
.L180:
@       print_string("*unknown-term(tag="); 
	mov r1, #19
	ldr r0, =g34
	bl print_string
@       print_num(lsr(mem[t], 8)); print_string(")*")
	ldr r0, =_mem
	ldr r1, [fp, #16]
	ldr r0, [r0, r1, LSL #2]
	lsr r0, r0, #8
	bl print_num
	mov r1, #3
	ldr r0, =g35
	bl print_string
.L176:
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

@ proc PrintClause(c: clause);
_PrintClause:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   if c = NULL then
	ldr r0, [fp, #24]
	cmp r0, #0
	bne .L192
@     print_string("*null-clause*"); newline();
	mov r1, #14
	ldr r0, =g36
	bl print_string
	bl newline
	b .L190
.L192:
@     if mem[c+3] <> NULL then
	ldr r0, =_mem
	ldr r1, [fp, #24]
	add r0, r0, r1, LSL #2
	ldr r6, [r0, #12]
	cmp r6, #0
	beq .L196
@       PrintTerm(mem[c+3], NULL, MAXPRIO);
	mov r2, #2
	mov r1, #0
	mov r0, r6
	bl _PrintTerm
@       print_char(' ')
	mov r0, #32
	bl print_char
.L196:
@     print_string(":- ");
	mov r1, #4
	ldr r0, =g37
	bl print_string
@     if mem[(c+4)+1-1] <> NULL then
	ldr r0, =_mem
	ldr r1, [fp, #24]
	add r0, r0, r1, LSL #2
	ldr r6, [r0, #16]
	cmp r6, #0
	beq .L199
@       PrintTerm(mem[(c+4)+1-1], NULL, MAXPRIO);
	mov r2, #2
	mov r1, #0
	mov r0, r6
	bl _PrintTerm
@       i := 2;
	mov r5, #2
.L200:
@       while mem[(c+4)+i-1] <> NULL do
	ldr r6, =_mem
	ldr r0, [fp, #24]
	add r0, r0, #4
	add r0, r0, r5
	add r0, r6, r0, LSL #2
	ldr r0, [r0, #-4]
	cmp r0, #0
	beq .L199
@ 	print_string(", ");
	mov r1, #3
	ldr r0, =g38
	bl print_string
@ 	PrintTerm(mem[(c+4)+i-1], NULL, MAXPRIO);
	mov r2, #2
	mov r1, #0
	ldr r0, [fp, #24]
	add r0, r0, #4
	add r0, r0, r5
	add r0, r6, r0, LSL #2
	ldr r0, [r0, #-4]
	bl _PrintTerm
@ 	i := i+1
	add r5, r5, #1
	b .L200
.L199:
@     print_char('.'); newline()
	mov r0, #46
	bl print_char
	bl newline
.L190:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc ShowError();
_ShowError:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   errflag := true; errcount := errcount+1;
	mov r0, #1
	ldr r1, =_errflag
	strb r0, [r1]
	ldr r5, =_errcount
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
@   print_string("Line "); print_num(lineno); print_char(' ');
	mov r1, #6
	ldr r0, =g39
	bl print_string
	ldr r0, =_lineno
	ldr r0, [r0]
	bl print_num
	mov r0, #32
	bl print_char
@   print_string("Syntax error - ")
	mov r1, #16
	ldr r0, =g40
	bl print_string
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc Recover();
_Recover:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   if errcount >= 20 then
	ldr r0, =_errcount
	ldr r0, [r0]
	cmp r0, #20
	blt .L207
@     print_string("Too many errors: I am giving up"); newline(); exit(2) 
	mov r1, #32
	ldr r0, =g41
	bl print_string
	bl newline
	mov r0, #2
	bl exit
.L207:
@   if token <> DOT then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #10
	beq .L204
.L211:
@       ch := GetChar()
	bl _GetChar
	mov r5, r0
	cmp r5, #46
	beq .L212
	cmp r5, #127
	bne .L211
.L212:
@     token := DOT
	mov r0, #10
	ldr r1, =_token
	str r0, [r1]
.L204:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc Scan();
_Scan:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   ch := GetChar(); token := 0;
	bl _GetChar
	mov r5, r0
	mov r0, #0
	ldr r1, =_token
	str r0, [r1]
.L215:
@   while token = 0 do
	ldr r8, =_token
	ldr r0, [r8]
	cmp r0, #0
	bne .L214
@     if ch = ENDFILE then
	cmp r5, #127
	bne .L219
@       token := EOFTOK
	mov r0, #14
	str r0, [r8]
	b .L215
.L219:
@     elsif (ch = ' ') or (ch = TAB) or (ch = ENDLINE) then
	cmp r5, #32
	beq .L221
	cmp r5, #9
	beq .L221
	cmp r5, #10
	bne .L222
.L221:
@       ch := GetChar()
	bl _GetChar
	mov r5, r0
	b .L215
.L222:
@     elsif ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) then
	cmp r5, #65
	blt .L311
	cmp r5, #90
	ble .L224
.L311:
	cmp r5, #95
	beq .L224
	cmp r5, #97
	blt .L225
	cmp r5, #122
	bgt .L225
.L224:
@       if (((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) then 
	cmp r5, #65
	blt .L292
	cmp r5, #90
	ble .L289
.L292:
	cmp r5, #95
	bne .L290
.L289:
@ 	 token := VARIABLE
	mov r0, #2
	ldr r1, =_token
	str r0, [r1]
	b .L291
.L290:
@ 	 token := IDENT
	mov r0, #1
	ldr r1, =_token
	str r0, [r1]
.L291:
@       i := 0;
	mov r7, #0
.L294:
@       while ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) or ((ch >= '0') and (ch <= '9')) do
	cmp r5, #65
	blt .L304
	cmp r5, #90
	ble .L295
.L304:
	cmp r5, #95
	beq .L295
	cmp r5, #97
	blt .L300
	cmp r5, #122
	ble .L295
.L300:
	cmp r5, #48
	blt .L296
	cmp r5, #57
	bgt .L296
.L295:
@         if i > MAXSTRING then
	cmp r7, #128
	ble .L299
@           newline(); print_string("Panic: "); print_string("identifier too long"); newline(); exit(2)
	bl newline
	mov r1, #8
	ldr r0, =g42
	bl print_string
	mov r1, #20
	ldr r0, =g43
	bl print_string
	bl newline
	mov r0, #2
	bl exit
.L299:
@         toksval[i] := ch; ch := GetChar(); i := i+1
	ldr r0, =_toksval
	strb r5, [r0, r7]
	bl _GetChar
	mov r5, r0
	add r7, r7, #1
	b .L294
.L296:
@       PushBack(ch);
	mov r0, r5
	bl _PushBack
@       toksval[i] := ENDSTR; tokval := Lookup(toksval);
	ldr r8, =_toksval
	mov r0, #0
	strb r0, [r8, r7]
	mov r0, r8
	bl _Lookup
	ldr r1, =_tokval
	str r0, [r1]
@       if tokval = notsym then token := NEGATE end
	ldr r1, =_notsym
	ldr r1, [r1]
	cmp r0, r1
	bne .L215
	mov r0, #13
	ldr r1, =_token
	str r0, [r1]
	b .L215
.L225:
@     elsif ((ch >= '0') and (ch <= '9')) then
	cmp r5, #48
	blt .L228
	cmp r5, #57
	bgt .L228
@       token := NUMBER; tokival := 0;
	mov r0, #3
	ldr r1, =_token
	str r0, [r1]
	mov r0, #0
	ldr r1, =_tokival
	str r0, [r1]
.L284:
@       while ((ch >= '0') and (ch <= '9')) do
	cmp r5, #48
	blt .L286
	cmp r5, #57
	bgt .L286
@         tokival := 10 * tokival + (ord(ch) - ord('0'));
	ldr r8, =_tokival
	ldr r0, [r8]
	mov r1, #10
	mul r0, r0, r1
	sub r1, r5, #48
	add r0, r0, r1
	str r0, [r8]
@         ch := GetChar()
	bl _GetChar
	mov r5, r0
	b .L284
.L286:
@       PushBack(ch)
	mov r0, r5
	bl _PushBack
	b .L215
.L228:
@       case ch of
	sub r0, r5, #33
	cmp r0, #30
	ldrlo pc, [pc, r0, LSL #2]
	b .L230
	.word .L240
	.word .L244
	.word .L239
	.word .L230
	.word .L230
	.word .L230
	.word .L243
	.word .L232
	.word .L233
	.word .L230
	.word .L230
	.word .L234
	.word .L230
	.word .L235
	.word .L241
	.word .L230
	.word .L230
	.word .L230
	.word .L230
	.word .L230
	.word .L230
	.word .L230
	.word .L230
	.word .L230
	.word .L230
	.word .L242
	.word .L230
	.word .L237
	.word .L236
	.word .L238
.L232:
@         '(': token := LPAR
	mov r0, #7
	ldr r1, =_token
	str r0, [r1]
	b .L215
.L233:
@       | ')': token := RPAR
	mov r0, #8
	ldr r1, =_token
	str r0, [r1]
	b .L215
.L234:
@       | ',': token := COMMA
	mov r0, #9
	ldr r1, =_token
	str r0, [r1]
	b .L215
.L235:
@       | '.': token := DOT
	mov r0, #10
	ldr r1, =_token
	str r0, [r1]
	b .L215
.L236:
@       | '=': token := EQUAL
	mov r0, #12
	ldr r1, =_token
	str r0, [r1]
	b .L215
.L237:
@       | '<': token := LANGLE
	mov r0, #15
	ldr r1, =_token
	str r0, [r1]
	b .L215
.L238:
@       | '>': token := RANGLE
	mov r0, #16
	ldr r1, =_token
	str r0, [r1]
	b .L215
.L239:
@       | '#': token := HASH
	mov r0, #17
	ldr r1, =_token
	str r0, [r1]
	b .L215
.L240:
@       | '!': token := IDENT; tokval := cutsym
	mov r0, #1
	ldr r1, =_token
	str r0, [r1]
	ldr r0, =_cutsym
	ldr r0, [r0]
	ldr r1, =_tokval
	str r0, [r1]
	b .L215
.L241:
@ 	  ch := GetChar();
	bl _GetChar
	mov r5, r0
@ 	  if ch <> '*' then
	cmp r5, #42
	beq .L249
@ 	    if not errflag then ShowError(); print_string("bad token /"); newline(); Recover() end
	ldr r0, =_errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L215
	bl _ShowError
	mov r1, #12
	ldr r0, =g44
	bl print_string
	bl newline
	bl _Recover
	b .L215
.L249:
@ 	    ch2 := ' '; ch := GetChar();
	mov r6, #32
	bl _GetChar
	mov r5, r0
.L251:
@ 	    while (ch <> ENDFILE) and not ((ch2 = '*') and (ch = '/')) do
	cmp r5, #127
	beq .L253
	cmp r6, #42
	bne .L252
	cmp r5, #47
	beq .L253
.L252:
@ 	      ch2 := ch; ch := GetChar() 
	mov r6, r5
	bl _GetChar
	mov r5, r0
	b .L251
.L253:
@ 	    if ch = ENDFILE then
	cmp r5, #127
	bne .L257
@ 	      if not errflag then ShowError(); print_string("end of file in comment"); newline(); Recover() end
	ldr r0, =_errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L215
	bl _ShowError
	mov r1, #23
	ldr r0, =g45
	bl print_string
	bl newline
	bl _Recover
	b .L215
.L257:
@ 	      ch := GetChar()
	bl _GetChar
	mov r5, r0
	b .L215
.L242:
@ 	  ch := GetChar();
	bl _GetChar
	mov r5, r0
@ 	  if ch = '-' then
	cmp r5, #45
	bne .L266
@ 	    token := ARROW
	mov r0, #6
	ldr r1, =_token
	str r0, [r1]
	b .L215
.L266:
@ 	    PushBack(ch); token := COLON 
	mov r0, r5
	bl _PushBack
	mov r0, #11
	ldr r1, =_token
	str r0, [r1]
	b .L215
.L243:
@ 	  token := CHCON; tokival := ord(GetChar()); ch := GetChar();
	mov r0, #4
	ldr r1, =_token
	str r0, [r1]
	bl _GetChar
	ldr r1, =_tokival
	str r0, [r1]
	bl _GetChar
	mov r5, r0
@ 	  if ch <> '''' then if not errflag then ShowError(); print_string("missing quote"); newline(); Recover() end end
	cmp r5, #39
	beq .L215
	ldr r0, =_errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L215
	bl _ShowError
	mov r1, #14
	ldr r0, =g46
	bl print_string
	bl newline
	bl _Recover
	b .L215
.L244:
@ 	  token := STRCON; i := 0; ch := GetChar();
	mov r0, #5
	ldr r1, =_token
	str r0, [r1]
	mov r7, #0
	bl _GetChar
	mov r5, r0
.L274:
@ 	  while (ch <> '"') and (ch <> ENDLINE) do
	cmp r5, #34
	beq .L276
	cmp r5, #10
	beq .L276
@ 	    toksval[i] := ch; ch := GetChar(); i := i+1 
	ldr r0, =_toksval
	strb r5, [r0, r7]
	bl _GetChar
	mov r5, r0
	add r7, r7, #1
	b .L274
.L276:
@ 	  toksval[i] := ENDSTR;
	mov r0, #0
	ldr r1, =_toksval
	strb r0, [r1, r7]
@ 	  if ch = ENDLINE then
	cmp r5, #10
	bne .L215
@ 	    if not errflag then ShowError(); print_string("unterminated string"); newline(); Recover() end;
	ldr r0, =_errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L283
	bl _ShowError
	mov r1, #20
	ldr r0, =g47
	bl print_string
	bl newline
	bl _Recover
.L283:
@ 	    PushBack(ch)
	mov r0, r5
	bl _PushBack
	b .L215
.L230:
@ 	if not errflag then ShowError(); print_string("illegal character"); newline(); Recover() end; print_char(ch); newline()
	ldr r0, =_errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L247
	bl _ShowError
	mov r1, #18
	ldr r0, =g48
	bl print_string
	bl newline
	bl _Recover
.L247:
	mov r0, r5
	bl print_char
	bl newline
	b .L215
.L214:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc PrintToken(t: integer);
_PrintToken:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   case t of
	ldr r0, [fp, #16]
	sub r0, r0, #1
	cmp r0, #17
	ldrlo pc, [pc, r0, LSL #2]
	b .L316
	.word .L318
	.word .L319
	.word .L320
	.word .L321
	.word .L329
	.word .L322
	.word .L323
	.word .L324
	.word .L325
	.word .L326
	.word .L327
	.word .L328
	.word .L316
	.word .L316
	.word .L330
	.word .L331
	.word .L332
.L318:
@       print_string("identifier "); WriteString(symtab[tokval].name)
	mov r1, #12
	ldr r0, =g49
	bl print_string
	ldr r0, =_symtab
	ldr r1, =_tokval
	ldr r1, [r1]
	add r0, r0, r1, LSL #4
	ldr r0, [r0]
	bl _WriteString
	b .L315
.L319:
@       print_string("variable "); WriteString(symtab[tokval].name)
	mov r1, #10
	ldr r0, =g50
	bl print_string
	ldr r0, =_symtab
	ldr r1, =_tokval
	ldr r1, [r1]
	add r0, r0, r1, LSL #4
	ldr r0, [r0]
	bl _WriteString
	b .L315
.L320:
@   | NUMBER: print_string("number");
	mov r1, #7
	ldr r0, =g51
	bl print_string
	b .L315
.L321:
@   | CHCON:  print_string("char constant");
	mov r1, #14
	ldr r0, =g52
	bl print_string
	b .L315
.L322:
@   | ARROW:  print_string(":-");
	mov r1, #3
	ldr r0, =g53
	bl print_string
	b .L315
.L323:
@   | LPAR:   print_string("(");
	mov r1, #2
	ldr r0, =g54
	bl print_string
	b .L315
.L324:
@   | RPAR:   print_string(")");
	mov r1, #2
	ldr r0, =g55
	bl print_string
	b .L315
.L325:
@   | COMMA:  print_string(",");
	mov r1, #2
	ldr r0, =g56
	bl print_string
	b .L315
.L326:
@   | DOT:    print_string(".");
	mov r1, #2
	ldr r0, =g57
	bl print_string
	b .L315
.L327:
@   | COLON:  print_string(":");
	mov r1, #2
	ldr r0, =g58
	bl print_string
	b .L315
.L328:
@   | EQUAL:  print_string("=");
	mov r1, #2
	ldr r0, =g59
	bl print_string
	b .L315
.L329:
@   | STRCON: print_string("string constant")
	mov r1, #16
	ldr r0, =g60
	bl print_string
	b .L315
.L330:
@   | LANGLE: print_string("<")
	mov r1, #2
	ldr r0, =g61
	bl print_string
	b .L315
.L331:
@   | RANGLE: print_string(">")
	mov r1, #2
	ldr r0, =g62
	bl print_string
	b .L315
.L332:
@   | HASH:   print_string("#")
	mov r1, #2
	ldr r0, =g63
	bl print_string
	b .L315
.L316:
@     print_string("unknown token")
	mov r1, #14
	ldr r0, =g64
	bl print_string
.L315:
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

@ proc VarRep(name: symbol): term;
_VarRep:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   if nvars = MAXARITY then newline(); print_string("Panic: "); print_string("too many variables"); newline(); exit(2) end;
	ldr r0, =_nvars
	ldr r0, [r0]
	cmp r0, #63
	bne .L336
	bl newline
	mov r1, #8
	ldr r0, =g65
	bl print_string
	mov r1, #19
	ldr r0, =g66
	bl print_string
	bl newline
	mov r0, #2
	bl exit
.L336:
@   i := 1; vartable[nvars+1] := name;  (* sentinel *)
	mov r5, #1
	ldr r0, [fp, #32]
	ldr r1, =_vartable
	ldr r2, =_nvars
	ldr r2, [r2]
	add r1, r1, r2, LSL #2
	str r0, [r1, #4]
.L337:
@   while name <> vartable[i] do i := i+1 end;
	ldr r0, [fp, #32]
	ldr r1, =_vartable
	ldr r1, [r1, r5, LSL #2]
	cmp r0, r1
	beq .L339
	add r5, r5, #1
	b .L337
.L339:
@   if i = nvars+1 then nvars := nvars+1 end;
	ldr r6, =_nvars
	ldr r0, [r6]
	add r7, r0, #1
	cmp r5, r7
	bne .L342
	str r7, [r6]
.L342:
@   return MakeRef(i)
	mov r0, r5
	bl _MakeRef
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc ShowAnswer(bindings: frame);
_ShowAnswer:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #8
@   if nvars = 0 then
	ldr r0, =_nvars
	ldr r0, [r0]
	cmp r0, #0
	bne .L345
@     print_string("yes"); newline()
	mov r1, #4
	ldr r0, =g67
	bl print_string
	bl newline
	b .L343
.L345:
@     for i := 1 to nvars do
	mov r5, #1
	ldr r0, =_nvars
	ldr r0, [r0]
	str r0, [fp, #-4]
.L347:
	ldr r0, [fp, #-4]
	cmp r5, r0
	bgt .L343
@       WriteString(symtab[vartable[i]].name); print_string(" = ");
	ldr r0, =_symtab
	ldr r1, =_vartable
	ldr r1, [r1, r5, LSL #2]
	add r0, r0, r1, LSL #4
	ldr r0, [r0]
	bl _WriteString
	mov r1, #4
	ldr r0, =g68
	bl print_string
@       PrintTerm((bindings+7+(i-1)*TERM_SIZE), NULL, EQPRIO-1);
	mov r2, #1
	mov r1, #0
	ldr r0, [fp, #24]
	add r0, r0, #7
	lsl r3, r5, #1
	sub r3, r3, #2
	add r0, r0, r3
	bl _PrintTerm
@       newline()
	bl newline
	add r5, r5, #1
	b .L347
.L343:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc Eat(expected: integer);
_Eat:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   if token = expected then
	ldr r0, =_token
	ldr r5, [r0]
	ldr r0, [fp, #24]
	cmp r5, r0
	bne .L351
@     if token <> DOT then Scan() end
	cmp r5, #10
	beq .L349
	bl _Scan
	b .L349
.L351:
@   elsif not errflag then
	ldr r0, =_errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L349
@     ShowError();
	bl _ShowError
@     print_string("expected "); PrintToken(expected);
	mov r1, #10
	ldr r0, =g69
	bl print_string
	ldr r0, [fp, #24]
	bl _PrintToken
@     print_string(", found "); PrintToken(token); newline();
	mov r1, #9
	ldr r0, =g70
	bl print_string
	ldr r0, =_token
	ldr r0, [r0]
	bl _PrintToken
	bl newline
@     Recover()
	bl _Recover
.L349:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc ParseCompound(): term;
_ParseCompound:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #256
@   fun := tokval; n := 0; Eat(IDENT);
	ldr r0, =_tokval
	ldr r5, [r0]
	mov r6, #0
	mov r0, #1
	bl _Eat
@   if token = LPAR then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #7
	bne .L362
@     Eat(LPAR); n := 1; arg[1] := ParseTerm();
	mov r0, #7
	bl _Eat
	mov r6, #1
	bl _ParseTerm
	str r0, [fp, #-252]
.L363:
@     while token = COMMA do
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #9
	bne .L365
@       Eat(COMMA); n := n+1; arg[n] := ParseTerm()
	mov r0, #9
	bl _Eat
	add r6, r6, #1
	bl _ParseTerm
	add r1, fp, #-256
	str r0, [r1, r6, LSL #2]
	b .L363
.L365:
@     Eat(RPAR)
	mov r0, #8
	bl _Eat
.L362:
@   if symtab[fun].arity = -1 then
	ldr r0, =_symtab
	add r7, r0, r5, LSL #4
	ldr r8, =4
	ldr r0, [r7, r8]
	cmp r0, #-1
	bne .L367
@     symtab[fun].arity := n
	str r6, [r7, r8]
	b .L368
.L367:
@   elsif symtab[fun].arity <> n then
	ldr r0, =_symtab
	add r0, r0, r5, LSL #4
	ldr r0, [r0, #4]
	cmp r0, r6
	beq .L368
@     if not errflag then ShowError(); print_string("wrong number of args"); newline(); Recover() end
	ldr r0, =_errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L368
	bl _ShowError
	mov r1, #21
	ldr r0, =g71
	bl print_string
	bl newline
	bl _Recover
.L368:
@   return MakeCompound(fun, arg)
	add r1, fp, #-256
	mov r0, r5
	bl _MakeCompound
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc ParsePrimary(): term;
_ParsePrimary:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   if token = IDENT then t := ParseCompound()
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #1
	bne .L377
	bl _ParseCompound
	mov r5, r0
	b .L378
.L377:
@   elsif token = VARIABLE then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #2
	bne .L380
@     t := VarRep(tokval); Eat(VARIABLE)
	ldr r0, =_tokval
	ldr r0, [r0]
	bl _VarRep
	mov r5, r0
	mov r0, #2
	bl _Eat
	b .L378
.L380:
@   elsif token = NUMBER then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #3
	bne .L383
@     t := MakeInt(tokival); Eat(NUMBER)
	ldr r0, =_tokival
	ldr r0, [r0]
	bl _MakeInt
	mov r5, r0
	mov r0, #3
	bl _Eat
	b .L378
.L383:
@   elsif token = CHCON then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #4
	bne .L386
@     t := MakeChar(chr(tokival)); Eat(CHCON)
	ldr r0, =_tokival
	ldr r0, [r0]
	bl _MakeChar
	mov r5, r0
	mov r0, #4
	bl _Eat
	b .L378
.L386:
@   elsif token = STRCON then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #5
	bne .L389
@     t := MakeString(toksval); Eat(STRCON)
	ldr r0, =_toksval
	bl _MakeString
	mov r5, r0
	mov r0, #5
	bl _Eat
	b .L378
.L389:
@   elsif token = LPAR then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #7
	bne .L392
@     Eat(LPAR); t := ParseTerm(); Eat(RPAR)
	mov r0, #7
	bl _Eat
	bl _ParseTerm
	mov r5, r0
	mov r0, #8
	bl _Eat
	b .L378
.L392:
@   elsif token = LANGLE then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #15
	bne .L395
@     t := ParseNode()
	bl _ParseNode
	mov r5, r0
	b .L378
.L395:
@     if not errflag then ShowError(); print_string("expected a term"); newline(); Recover() end; t := NULL
	ldr r0, =_errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L399
	bl _ShowError
	mov r1, #16
	ldr r0, =g72
	bl print_string
	bl newline
	bl _Recover
.L399:
	mov r5, #0
.L378:
@   return t
	mov r0, r5
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc ParseNode(): term;
_ParseNode:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   Eat(LANGLE);
	mov r0, #15
	bl _Eat
@   tag := ParseTerm();
	bl _ParseTerm
	mov r5, r0
@   kids := ParseKids();
	bl _ParseKids
	mov r6, r0
@   Eat(RANGLE);
	mov r0, #16
	bl _Eat
@   return MakeNode(node, tag, kids)
	mov r2, r6
	mov r1, r5
	ldr r0, =_node
	ldr r0, [r0]
	bl _MakeNode
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc ParseKids(): term;
_ParseKids:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   if token <> COMMA then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #9
	beq .L403
@     return MakeNode(nilsym, NULL, NULL)
	mov r2, #0
	mov r1, #0
	ldr r0, =_nilsym
	ldr r0, [r0]
	bl _MakeNode
	b .L401
.L403:
@     Eat(COMMA);
	mov r0, #9
	bl _Eat
@     head := ParseTerm();
	bl _ParseTerm
	mov r5, r0
@     tail := ParseKids();
	bl _ParseKids
	mov r6, r0
@     return MakeNode(cons, head, tail)
	mov r2, r6
	mov r1, r5
	ldr r0, =_cons
	ldr r0, [r0]
	bl _MakeNode
.L401:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc ParseFactor(): term;
_ParseFactor:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   t := ParsePrimary();
	bl _ParsePrimary
	mov r5, r0
@   if token <> COLON then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #11
	beq .L407
@     return t
	mov r0, r5
	b .L405
.L407:
@     Eat(COLON);
	mov r0, #11
	bl _Eat
@     return MakeNode(cons, t, ParseFactor())
	bl _ParseFactor
	mov r2, r0
	mov r1, r5
	ldr r0, =_cons
	ldr r0, [r0]
	bl _MakeNode
.L405:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc ParseTerm(): term;
_ParseTerm:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   t := ParseFactor();
	bl _ParseFactor
	mov r5, r0
@   if token <> EQUAL then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #12
	beq .L411
@     return t
	mov r0, r5
	b .L409
.L411:
@     Eat(EQUAL);
	mov r0, #12
	bl _Eat
@     return MakeNode(eqsym, t, ParseFactor())
	bl _ParseFactor
	mov r2, r0
	mov r1, r5
	ldr r0, =_eqsym
	ldr r0, [r0]
	bl _MakeNode
.L409:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc CheckAtom(a: term);
_CheckAtom:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   if lsr(mem[a], 8) <> FUNC then
	ldr r0, =_mem
	ldr r1, [fp, #16]
	ldr r0, [r0, r1, LSL #2]
	lsr r0, r0, #8
	cmp r0, #1
	beq .L413
@     if not errflag then ShowError(); print_string("literal must be a compound term"); newline(); Recover() end
	ldr r0, =_errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L413
	bl _ShowError
	mov r1, #32
	ldr r0, =g73
	bl print_string
	bl newline
	bl _Recover
.L413:
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

@ proc ParseClause(): clause;
_ParseClause:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #264
@   if token = HASH then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #17
	bne .L422
@     Eat(HASH); head := NULL
	mov r0, #17
	bl _Eat
	mov r5, #0
	b .L423
.L422:
@     head := ParseTerm();
	bl _ParseTerm
	mov r5, r0
@     CheckAtom(head)
	mov r0, r5
	bl _CheckAtom
.L423:
@   Eat(ARROW);
	mov r0, #6
	bl _Eat
@   n := 0;
	mov r7, #0
@   if token <> DOT then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #10
	beq .L426
@     more := true;
	mov r0, #1
	strb r0, [fp, #-258]
.L427:
@     while more do
	ldrb r0, [fp, #-258]
	cmp r0, #0
	beq .L426
@       n := n+1; minus := false;
	add r7, r7, #1
	mov r0, #0
	strb r0, [fp, #-257]
@       if token = NEGATE then
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #13
	bne .L432
@ 	Eat(NEGATE); minus := true 
	mov r0, #13
	bl _Eat
	mov r0, #1
	strb r0, [fp, #-257]
.L432:
@       t := ParseTerm(); CheckAtom(t);
	bl _ParseTerm
	mov r6, r0
	mov r0, r6
	bl _CheckAtom
@       if minus then 
	ldrb r0, [fp, #-257]
	cmp r0, #0
	beq .L434
@ 	body[n] := MakeNode(notsym, t, NULL)
	mov r2, #0
	mov r1, r6
	ldr r0, =_notsym
	ldr r0, [r0]
	bl _MakeNode
	add r1, fp, #-256
	str r0, [r1, r7, LSL #2]
	b .L435
.L434:
@         body[n] := t
	add r0, fp, #-256
	str r6, [r0, r7, LSL #2]
.L435:
@       if token = COMMA then Eat(COMMA) else more := false end
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #9
	bne .L437
	mov r0, #9
	bl _Eat
	b .L427
.L437:
	mov r0, #0
	strb r0, [fp, #-258]
	b .L427
.L426:
@   Eat(DOT);
	mov r0, #10
	bl _Eat
@   if errflag then 
	ldr r0, =_errflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L440
@     return NULL
	mov r0, #0
	b .L420
.L440:
@     return MakeClause(nvars, head, body, n)
	mov r3, r7
	add r2, fp, #-256
	mov r1, r5
	ldr r0, =_nvars
	ldr r0, [r0]
	bl _MakeClause
.L420:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc ReadClause(): clause;
_ReadClause:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
.L443:
@     hp := hmark; nvars := 0; errflag := false;
	ldr r0, =_hmark
	ldr r0, [r0]
	ldr r1, =_hp
	str r0, [r1]
	mov r0, #0
	ldr r1, =_nvars
	str r0, [r1]
	mov r0, #0
	ldr r1, =_errflag
	strb r0, [r1]
@     Scan();
	bl _Scan
@     if token = EOFTOK then 
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #14
	bne .L447
@       c := NULL
	mov r5, #0
	b .L448
.L447:
@       c := ParseClause()
	bl _ParseClause
	mov r5, r0
.L448:
	ldr r0, =_errflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L444
	ldr r0, =_token
	ldr r0, [r0]
	cmp r0, #14
	bne .L443
.L444:
@   return c
	mov r0, r5
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc Save(v: term);
_Save:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   if ((v < choice) or (v >= mem[choice+4])) then
	ldr r6, [fp, #32]
	ldr r0, =_choice
	ldr r7, [r0]
	cmp r6, r7
	blt .L450
	ldr r0, =_mem
	add r0, r0, r7, LSL #2
	ldr r0, [r0, #16]
	cmp r6, r0
	blt .L449
.L450:
@     p := GloAlloc(UNDO, TRAIL_SIZE);
	mov r1, #3
	mov r0, #6
	bl _GloAlloc
	mov r5, r0
@     mem[p+1] := v; mem[p+2] := trhead; trhead := p
	ldr r0, =_mem
	add r6, r0, r5, LSL #2
	ldr r0, [fp, #32]
	str r0, [r6, #4]
	ldr r7, =_trhead
	ldr r0, [r7]
	str r0, [r6, #8]
	str r5, [r7]
.L449:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc Restore();
_Restore:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
.L455:
@   while (trhead <> mem[choice+5]) do
	ldr r0, =_trhead
	ldr r6, [r0]
	ldr r7, =_mem
	ldr r0, =_choice
	ldr r0, [r0]
	add r0, r7, r0, LSL #2
	ldr r0, [r0, #20]
	cmp r6, r0
	beq .L454
@     v := mem[trhead+1];
	add r0, r7, r6, LSL #2
	ldr r5, [r0, #4]
@     if v <> NULL then mem[v+1] := NULL end;
	cmp r5, #0
	beq .L460
	mov r0, #0
	add r1, r7, r5, LSL #2
	str r0, [r1, #4]
.L460:
@     trhead := mem[trhead+2]
	ldr r6, =_trhead
	ldr r0, =_mem
	ldr r1, [r6]
	add r0, r0, r1, LSL #2
	ldr r0, [r0, #8]
	str r0, [r6]
	b .L455
.L454:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc Commit();
_Commit:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   p := trhead;
	ldr r0, =_trhead
	ldr r5, [r0]
.L462:
@   while (p <> NULL) and (p < mem[choice+4]) do
	cmp r5, #0
	beq .L461
	ldr r6, =_mem
	ldr r0, =_choice
	ldr r7, [r0]
	add r0, r6, r7, LSL #2
	ldr r8, [r0, #16]
	cmp r5, r8
	bge .L461
@     if (mem[p+1] <> NULL) and not ((mem[p+1] < choice) or (mem[p+1] >= mem[choice+4])) then
	add r6, r6, r5, LSL #2
	ldr r9, [r6, #4]
	cmp r9, #0
	beq .L467
	cmp r9, r7
	blt .L467
	cmp r9, r8
	bge .L467
@       mem[p+1] := NULL
	mov r0, #0
	str r0, [r6, #4]
.L467:
@     p := mem[p+2]
	ldr r0, =_mem
	add r0, r0, r5, LSL #2
	ldr r5, [r0, #8]
	b .L462
.L461:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc GloCopy(t: term; e: frame): term;
_GloCopy:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #8
@   t := Deref(t, e);
	ldr r1, [fp, #36]
	ldr r0, [fp, #32]
	bl _Deref
	str r0, [fp, #32]
@   if (t >= gsp) then
	ldr r1, =_gsp
	ldr r1, [r1]
	cmp r0, r1
	blt .L473
@     return t
	b .L471
.L473:
@     case lsr(mem[t], 8) of
	ldr r0, =_mem
	ldr r1, [fp, #32]
	ldr r0, [r0, r1, LSL #2]
	lsr r0, r0, #8
	sub r0, r0, #1
	cmp r0, #4
	ldrlo pc, [pc, r0, LSL #2]
	b .L475
	.word .L477
	.word .L475
	.word .L475
	.word .L478
.L477:
@ 	n := symtab[mem[t+1]].arity;
	ldr r8, [fp, #32]
	ldr r0, =_symtab
	ldr r1, =_mem
	add r1, r1, r8, LSL #2
	ldr r1, [r1, #4]
	add r0, r0, r1, LSL #4
	ldr r7, [r0, #4]
@ 	if (t <= hp) and (n = 0) then 
	ldr r0, =_hp
	ldr r0, [r0]
	cmp r8, r0
	bgt .L480
	cmp r7, #0
	bne .L480
@ 	  return t
	mov r0, r8
	b .L471
.L480:
@ 	  tt := GloAlloc(FUNC, TERM_SIZE+n);
	add r1, r7, #2
	mov r0, #1
	bl _GloAlloc
	mov r5, r0
@ 	  mem[tt+1] := mem[t+1];
	ldr r8, =_mem
	ldr r0, [fp, #32]
	add r0, r8, r0, LSL #2
	ldr r0, [r0, #4]
	add r1, r8, r5, LSL #2
	str r0, [r1, #4]
@ 	  for i := 1 to n do
	mov r6, #1
	str r7, [fp, #-4]
.L482:
	ldr r0, [fp, #-4]
	cmp r6, r0
	bgt .L483
@ 	    mem[tt+i+1] := GloCopy(mem[t+i+1], e)
	ldr r8, =_mem
	ldr r1, [fp, #36]
	ldr r0, [fp, #32]
	add r0, r0, r6
	add r0, r8, r0, LSL #2
	ldr r0, [r0, #4]
	bl _GloCopy
	add r1, r5, r6
	add r1, r8, r1, LSL #2
	str r0, [r1, #4]
	add r6, r6, #1
	b .L482
.L483:
@ 	  return tt
	mov r0, r5
	b .L471
.L478:
@         tt := GloAlloc(CELL, TERM_SIZE);
	mov r1, #2
	mov r0, #4
	bl _GloAlloc
	mov r5, r0
@         mem[tt+1] := NULL;
	ldr r8, =_mem
	mov r0, #0
	add r1, r8, r5, LSL #2
	str r0, [r1, #4]
@ 	Save(t); mem[t+1] := tt;
	ldr r0, [fp, #32]
	bl _Save
	ldr r0, [fp, #32]
	add r0, r8, r0, LSL #2
	str r5, [r0, #4]
@         return tt
	mov r0, r5
	b .L471
.L475:
@       return t
	ldr r0, [fp, #32]
.L471:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc Share(v1, v2: term);
_Share:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   if (v1 * (2 * ord((v1 >= gsp)) - 1)) <= (v2 * (2 * ord((v2 >= gsp)) - 1)) then
	ldr r5, [fp, #32]
	ldr r0, =_gsp
	ldr r6, [r0]
	ldr r7, [fp, #36]
	cmp r5, r6
	mov r0, #0
	movge r0, #1
	lsl r0, r0, #1
	sub r0, r0, #1
	mul r0, r5, r0
	cmp r7, r6
	mov r1, #0
	movge r1, #1
	lsl r1, r1, #1
	sub r1, r1, #1
	mul r1, r7, r1
	cmp r0, r1
	bgt .L487
@     Save(v1); mem[v1+1] := v2
	mov r0, r5
	bl _Save
	ldr r0, [fp, #36]
	ldr r1, =_mem
	ldr r2, [fp, #32]
	add r1, r1, r2, LSL #2
	str r0, [r1, #4]
	b .L485
.L487:
@     Save(v2); mem[v2+1] := v1 
	ldr r0, [fp, #36]
	bl _Save
	ldr r0, [fp, #32]
	ldr r1, =_mem
	ldr r2, [fp, #36]
	add r1, r1, r2, LSL #2
	str r0, [r1, #4]
.L485:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc Unify(t1: term; e1: frame; t2: term; e2: frame): boolean;
_Unify:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   t1 := Deref(t1, e1); t2 := Deref(t2, e2);
	ldr r1, [fp, #44]
	ldr r0, [fp, #40]
	bl _Deref
	str r0, [fp, #40]
	ldr r1, [fp, #52]
	ldr r0, [fp, #48]
	bl _Deref
	str r0, [fp, #48]
@   if t1 = t2 then  (* Includes unifying a var with itself *)
	ldr r1, [fp, #40]
	cmp r1, r0
	bne .L491
@     return true
	mov r0, #1
	b .L489
.L491:
@   elsif (lsr(mem[t1], 8) = CELL) and (lsr(mem[t2], 8) = CELL) then
	ldr r7, =_mem
	ldr r8, [fp, #40]
	ldr r0, [r7, r8, LSL #2]
	lsr r0, r0, #8
	cmp r0, #4
	bne .L494
	ldr r9, [fp, #48]
	ldr r0, [r7, r9, LSL #2]
	lsr r0, r0, #8
	cmp r0, #4
	bne .L494
@     Share(t1, t2); return true
	mov r1, r9
	mov r0, r8
	bl _Share
	mov r0, #1
	b .L489
.L494:
@   elsif lsr(mem[t1], 8) = CELL then
	ldr r7, =_mem
	ldr r8, [fp, #40]
	ldr r0, [r7, r8, LSL #2]
	lsr r0, r0, #8
	cmp r0, #4
	bne .L497
@     Save(t1); mem[t1+1] := GloCopy(t2, e2); return true
	mov r0, r8
	bl _Save
	ldr r1, [fp, #52]
	ldr r0, [fp, #48]
	bl _GloCopy
	ldr r1, [fp, #40]
	add r1, r7, r1, LSL #2
	str r0, [r1, #4]
	mov r0, #1
	b .L489
.L497:
@   elsif lsr(mem[t2], 8) = CELL then
	ldr r7, =_mem
	ldr r8, [fp, #48]
	ldr r0, [r7, r8, LSL #2]
	lsr r0, r0, #8
	cmp r0, #4
	bne .L500
@     Save(t2); mem[t2+1] := GloCopy(t1, e1); return true
	mov r0, r8
	bl _Save
	ldr r1, [fp, #44]
	ldr r0, [fp, #40]
	bl _GloCopy
	ldr r1, [fp, #48]
	add r1, r7, r1, LSL #2
	str r0, [r1, #4]
	mov r0, #1
	b .L489
.L500:
@   elsif lsr(mem[t1], 8) <> lsr(mem[t2], 8) then
	ldr r7, =_mem
	ldr r0, [fp, #40]
	ldr r0, [r7, r0, LSL #2]
	lsr r0, r0, #8
	ldr r1, [fp, #48]
	ldr r1, [r7, r1, LSL #2]
	lsr r1, r1, #8
	cmp r0, r1
	beq .L503
@     return false
	mov r0, #0
	b .L489
.L503:
@     case lsr(mem[t1], 8) of
	ldr r0, =_mem
	ldr r1, [fp, #40]
	ldr r0, [r0, r1, LSL #2]
	lsr r0, r0, #8
	sub r0, r0, #1
	cmp r0, #3
	ldrlo pc, [pc, r0, LSL #2]
	b .L505
	.word .L507
	.word .L508
	.word .L509
.L507:
@         if (mem[t1+1] <> mem[t2+1]) then
	ldr r7, =_mem
	ldr r0, [fp, #40]
	add r0, r7, r0, LSL #2
	ldr r0, [r0, #4]
	ldr r1, [fp, #48]
	add r1, r7, r1, LSL #2
	ldr r1, [r1, #4]
	cmp r0, r1
	beq .L511
@           return false
	mov r0, #0
	b .L489
.L511:
@           i := 1; match := true;
	mov r5, #1
	mov r6, #1
.L513:
@           while match and (i <= symtab[mem[t1+1]].arity) do
	cmp r6, #0
	beq .L515
	ldr r7, =_mem
	ldr r8, [fp, #40]
	ldr r0, =_symtab
	add r1, r7, r8, LSL #2
	ldr r1, [r1, #4]
	add r0, r0, r1, LSL #4
	ldr r0, [r0, #4]
	cmp r5, r0
	bgt .L515
@             match := Unify(mem[t1+i+1], e1, mem[t2+i+1], e2);
	ldr r3, [fp, #52]
	ldr r0, [fp, #48]
	add r0, r0, r5
	add r0, r7, r0, LSL #2
	ldr r2, [r0, #4]
	ldr r1, [fp, #44]
	add r0, r8, r5
	add r0, r7, r0, LSL #2
	ldr r0, [r0, #4]
	bl _Unify
	mov r6, r0
@             i := i+1
	add r5, r5, #1
	b .L513
.L515:
@           return match
	mov r0, r6
	b .L489
.L508:
@         return (mem[t1+1] = mem[t2+1])
	ldr r7, =_mem
	ldr r0, [fp, #40]
	add r0, r7, r0, LSL #2
	ldr r0, [r0, #4]
	ldr r1, [fp, #48]
	add r1, r7, r1, LSL #2
	ldr r1, [r1, #4]
	cmp r0, r1
	mov r0, #0
	moveq r0, #1
	b .L489
.L509:
@         return (mem[t1+1] = mem[t2+1])
	ldr r7, =_mem
	ldr r0, [fp, #40]
	add r0, r7, r0, LSL #2
	ldr r0, [r0, #4]
	ldr r1, [fp, #48]
	add r1, r7, r1, LSL #2
	ldr r1, [r1, #4]
	cmp r0, r1
	mov r0, #0
	moveq r0, #1
	b .L489
.L505:
@       newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t1):1, " in ", "Unify"*)); newline(); exit(2)
	bl newline
	mov r1, #8
	ldr r0, =g74
	bl print_string
	mov r1, #8
	ldr r0, =g75
	bl print_string
	bl newline
	mov r0, #2
	bl exit
.L489:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Key(t: term; e: frame): integer;
_Key:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   if t = NULL then newline(); print_string("Panic: "); print_string("Key"); newline(); exit(2) end;
	ldr r0, [fp, #24]
	cmp r0, #0
	bne .L521
	bl newline
	mov r1, #8
	ldr r0, =g76
	bl print_string
	mov r1, #4
	ldr r0, =g77
	bl print_string
	bl newline
	mov r0, #2
	bl exit
.L521:
@   if lsr(mem[t], 8) <> FUNC then newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t):1, " in ", "Key1"*)); newline(); exit(2) end;
	ldr r0, =_mem
	ldr r1, [fp, #24]
	ldr r0, [r0, r1, LSL #2]
	lsr r0, r0, #8
	cmp r0, #1
	beq .L524
	bl newline
	mov r1, #8
	ldr r0, =g78
	bl print_string
	mov r1, #8
	ldr r0, =g79
	bl print_string
	bl newline
	mov r0, #2
	bl exit
.L524:
@   if symtab[mem[t+1]].arity = 0 then
	ldr r0, =_symtab
	ldr r1, =_mem
	ldr r2, [fp, #24]
	add r1, r1, r2, LSL #2
	ldr r1, [r1, #4]
	add r0, r0, r1, LSL #4
	ldr r0, [r0, #4]
	cmp r0, #0
	bne .L526
@     return 0
	mov r0, #0
	b .L518
.L526:
@     t0 := Deref(mem[t+1+1], e);
	ldr r6, =_mem
	ldr r1, [fp, #28]
	ldr r0, [fp, #24]
	add r0, r6, r0, LSL #2
	ldr r0, [r0, #8]
	bl _Deref
	mov r5, r0
@     case lsr(mem[t0], 8) of
	ldr r0, [r6, r5, LSL #2]
	lsr r0, r0, #8
	sub r0, r0, #1
	cmp r0, #3
	ldrlo pc, [pc, r0, LSL #2]
	b .L528
	.word .L530
	.word .L531
	.word .L532
.L530:
@         FUNC:      return mem[t0+1]
	ldr r0, =_mem
	add r0, r0, r5, LSL #2
	ldr r0, [r0, #4]
	b .L518
.L531:
@       | INT:       return mem[t0+1] + 1
	ldr r0, =_mem
	add r0, r0, r5, LSL #2
	ldr r0, [r0, #4]
	add r0, r0, #1
	b .L518
.L532:
@       | CHRCTR:    return mem[t0+1] + 1
	ldr r0, =_mem
	add r0, r0, r5, LSL #2
	ldr r0, [r0, #4]
	add r0, r0, #1
	b .L518
.L528:
@       return 0
	mov r0, #0
.L518:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc Search(t: term; e: frame; p: clause): clause;
_Search:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   k := Key(t, e);
	ldr r1, [fp, #36]
	ldr r0, [fp, #32]
	bl _Key
	mov r5, r0
@   if k <> 0 then
	cmp r5, #0
	beq .L536
.L537:
@     while (p <> NULL) and (mem[p+1] <> 0) and (mem[p+1] <> k) do
	ldr r6, [fp, #40]
	cmp r6, #0
	beq .L536
	ldr r0, =_mem
	add r6, r0, r6, LSL #2
	ldr r7, [r6, #4]
	cmp r7, #0
	beq .L536
	cmp r7, r5
	beq .L536
@       p := mem[p+2]
	ldr r0, [r6, #8]
	str r0, [fp, #40]
	b .L537
.L536:
@   return p
	ldr r0, [fp, #40]
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc PushFrame(nvars: integer; retry: clause);
_PushFrame:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   f := LocAlloc((FRAME_SIZE + (nvars)*TERM_SIZE));
	ldr r0, [fp, #40]
	lsl r0, r0, #1
	add r0, r0, #7
	bl _LocAlloc
	mov r5, r0
@   mem[f] := current; mem[f+1] := goalframe;
	ldr r8, =_mem
	ldr r0, =_current
	ldr r0, [r0]
	str r0, [r8, r5, LSL #2]
	add r8, r8, r5, LSL #2
	ldr r0, =_goalframe
	ldr r0, [r0]
	str r0, [r8, #4]
@   mem[f+2] := retry; mem[f+3] := choice;
	ldr r0, [fp, #44]
	str r0, [r8, #8]
	ldr r0, =_choice
	ldr r0, [r0]
	str r0, [r8, #12]
@   mem[f+4] := gsp; mem[f+5] := trhead;
	ldr r0, =_gsp
	ldr r0, [r0]
	str r0, [r8, #16]
	ldr r0, =_trhead
	ldr r0, [r0]
	str r0, [r8, #20]
@   mem[f+6] := nvars;
	ldr r9, [fp, #40]
	str r9, [r8, #24]
@   for i := 1 to nvars do
	mov r6, #1
	mov r7, r9
.L543:
	cmp r6, r7
	bgt .L544
@     mem[(f+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
	ldr r8, =_mem
	add r0, r5, #7
	lsl r1, r6, #1
	sub r1, r1, #2
	add r9, r0, r1
	ldr r0, =1026
	str r0, [r8, r9, LSL #2]
@     mem[(f+7+(i-1)*TERM_SIZE)+1] := NULL
	mov r0, #0
	add r1, r8, r9, LSL #2
	str r0, [r1, #4]
	add r6, r6, #1
	b .L543
.L544:
@   goalframe := f;
	ldr r0, =_goalframe
	str r5, [r0]
@   if retry <> NULL then choice := goalframe end
	ldr r0, [fp, #44]
	cmp r0, #0
	beq .L542
	ldr r0, =_goalframe
	ldr r8, [r0]
	ldr r0, =_choice
	str r8, [r0]
.L542:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc TroStep();
_TroStep:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #16
@   if dflag then print_string("(TRO)"); newline() end;
	ldr r0, =_dflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L551
	mov r1, #6
	ldr r0, =g80
	bl print_string
	bl newline
.L551:
@   oldsize := (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE); (* size of old frame *)
	ldr r8, =_mem
	ldr r9, =_goalframe
	ldr r0, [r9]
	add r0, r8, r0, LSL #2
	ldr r0, [r0, #24]
	lsl r0, r0, #1
	add r6, r0, #7
@   newsize := (FRAME_SIZE + (mem[prok])*TERM_SIZE); (* size of new frame *)
	ldr r0, =_prok
	ldr r0, [r0]
	ldr r0, [r8, r0, LSL #2]
	lsl r0, r0, #1
	add r7, r0, #7
@   temp := LocAlloc(newsize);
	mov r0, r7
	bl _LocAlloc
	mov r5, r0
@   temp := goalframe + newsize; (* copy old frame here *)
	ldr r0, [r9]
	add r5, r0, r7
@   for i := 1 to oldsize do 
	mov r0, #1
	str r0, [fp, #-4]
	str r6, [fp, #-8]
.L552:
	ldr r8, [fp, #-4]
	ldr r0, [fp, #-8]
	cmp r8, r0
	bgt .L553
@     mem[temp+oldsize-i] := mem[goalframe+oldsize-i]
	ldr r9, =_mem
	ldr r0, =_goalframe
	ldr r0, [r0]
	add r0, r0, r6
	sub r0, r0, r8
	ldr r0, [r9, r0, LSL #2]
	add r1, r5, r6
	sub r1, r1, r8
	str r0, [r9, r1, LSL #2]
	add r0, r8, #1
	str r0, [fp, #-4]
	b .L552
.L553:
@   for i := 1 to mem[goalframe+6] do
	mov r0, #1
	str r0, [fp, #-4]
	ldr r0, =_mem
	ldr r1, =_goalframe
	ldr r1, [r1]
	add r0, r0, r1, LSL #2
	ldr r0, [r0, #24]
	str r0, [fp, #-12]
.L554:
	ldr r8, [fp, #-4]
	ldr r0, [fp, #-12]
	cmp r8, r0
	bgt .L555
@     if (lsr(mem[(temp+7+(i-1)*TERM_SIZE)], 8) = CELL)
	ldr r9, =_mem
	add r0, r5, #7
	lsl r1, r8, #1
	sub r1, r1, #2
	add r8, r0, r1
	ldr r0, [r9, r8, LSL #2]
	lsr r0, r0, #8
	cmp r0, #4
	bne .L558
	add r8, r9, r8, LSL #2
	ldr r9, [r8, #4]
	cmp r9, #0
	beq .L558
	ldr r0, =_goalframe
	ldr r10, [r0]
	cmp r10, r9
	bgt .L558
	add r0, r10, r6
	cmp r9, r0
	bge .L558
@       mem[(temp+7+(i-1)*TERM_SIZE)+1] := mem[(temp+7+(i-1)*TERM_SIZE)+1] + newsize
	add r0, r9, r7
	str r0, [r8, #4]
.L558:
	ldr r0, [fp, #-4]
	add r0, r0, #1
	str r0, [fp, #-4]
	b .L554
.L555:
@   mem[goalframe+6] := mem[prok];
	ldr r8, =_mem
	ldr r9, =_goalframe
	ldr r0, =_prok
	ldr r0, [r0]
	ldr r0, [r8, r0, LSL #2]
	ldr r1, [r9]
	add r1, r8, r1, LSL #2
	str r0, [r1, #24]
@   for i := 1 to mem[goalframe+6] do
	mov r0, #1
	str r0, [fp, #-4]
	ldr r0, [r9]
	add r0, r8, r0, LSL #2
	ldr r0, [r0, #24]
	str r0, [fp, #-16]
.L562:
	ldr r8, [fp, #-4]
	ldr r0, [fp, #-16]
	cmp r8, r0
	bgt .L563
@     mem[(goalframe+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
	ldr r9, =_mem
	ldr r10, =_goalframe
	lsl r0, r8, #1
	sub r0, r0, #2
	ldr r1, =1026
	ldr r2, [r10]
	add r2, r2, #7
	add r2, r2, r0
	str r1, [r9, r2, LSL #2]
@     mem[(goalframe+7+(i-1)*TERM_SIZE)+1] := NULL
	mov r1, #0
	ldr r2, [r10]
	add r2, r2, #7
	add r0, r2, r0
	add r0, r9, r0, LSL #2
	str r1, [r0, #4]
	add r0, r8, #1
	str r0, [fp, #-4]
	b .L562
.L563:
@   ok := Unify(call, temp, mem[prok+3], goalframe);
	ldr r8, =_prok
	ldr r0, =_goalframe
	ldr r3, [r0]
	ldr r0, =_mem
	ldr r1, [r8]
	add r0, r0, r1, LSL #2
	ldr r2, [r0, #12]
	mov r1, r5
	ldr r0, =_call
	ldr r0, [r0]
	bl _Unify
	ldr r1, =_ok
	strb r0, [r1]
@   current := (prok+4);
	ldr r0, [r8]
	add r0, r0, #4
	ldr r1, =_current
	str r0, [r1]
@   lsp := temp-1
	sub r0, r5, #1
	ldr r1, =_lsp
	str r0, [r1]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Step();
_Step:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   if symtab[mem[call+1]].action <> 0 then
	ldr r0, =_symtab
	ldr r1, =_mem
	ldr r2, =_call
	ldr r2, [r2]
	add r1, r1, r2, LSL #2
	ldr r1, [r1, #4]
	add r0, r0, r1, LSL #4
	ldr r6, [r0, #8]
	cmp r6, #0
	beq .L566
@     ok := DoBuiltin(symtab[mem[call+1]].action)
	mov r0, r6
	bl _DoBuiltin
	ldr r1, =_ok
	strb r0, [r1]
	b .L567
.L566:
@   elsif prok = NULL then
	ldr r0, =_prok
	ldr r0, [r0]
	cmp r0, #0
	bne .L569
@     ok := false
	mov r0, #0
	ldr r1, =_ok
	strb r0, [r1]
	b .L567
.L569:
@     retry := Search(call, goalframe, mem[prok+2]);
	ldr r6, =_goalframe
	ldr r7, =_mem
	ldr r0, =_prok
	ldr r0, [r0]
	add r0, r7, r0, LSL #2
	ldr r2, [r0, #8]
	ldr r1, [r6]
	ldr r0, =_call
	ldr r0, [r0]
	bl _Search
	mov r5, r0
@     if (mem[(current)+1] = NULL) and (choice < goalframe)
	ldr r0, =_current
	ldr r0, [r0]
	add r0, r7, r0, LSL #2
	ldr r0, [r0, #4]
	cmp r0, #0
	bne .L572
	ldr r6, [r6]
	ldr r0, =_choice
	ldr r0, [r0]
	cmp r0, r6
	bge .L572
	cmp r5, #0
	bne .L572
	ldr r0, =_base
	ldr r0, [r0]
	cmp r6, r0
	beq .L572
@       TroStep()
	bl _TroStep
	b .L567
.L572:
@       PushFrame(mem[prok], retry);
	ldr r6, =_mem
	ldr r7, =_prok
	mov r1, r5
	ldr r0, [r7]
	ldr r0, [r6, r0, LSL #2]
	bl _PushFrame
@       ok := Unify(call, mem[goalframe+1], mem[prok+3], goalframe);
	ldr r0, =_goalframe
	ldr r8, [r0]
	mov r3, r8
	ldr r0, [r7]
	add r0, r6, r0, LSL #2
	ldr r2, [r0, #12]
	add r0, r6, r8, LSL #2
	ldr r1, [r0, #4]
	ldr r0, =_call
	ldr r0, [r0]
	bl _Unify
	ldr r1, =_ok
	strb r0, [r1]
@       current := (prok+4);
	ldr r0, [r7]
	add r0, r0, #4
	ldr r1, =_current
	str r0, [r1]
.L567:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc Unwind();
_Unwind:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
.L578:
@   while (mem[current] = NULL) and (goalframe <> base) do
	ldr r5, =_mem
	ldr r0, =_current
	ldr r0, [r0]
	ldr r0, [r5, r0, LSL #2]
	cmp r0, #0
	bne .L577
	ldr r6, =_goalframe
	ldr r0, [r6]
	ldr r1, =_base
	ldr r1, [r1]
	cmp r0, r1
	beq .L577
@     if dflag then 
	ldr r0, =_dflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L583
@     print_string("Exit"); print_string(": "); 
	mov r1, #5
	ldr r0, =g81
	bl print_string
	mov r1, #3
	ldr r0, =g82
	bl print_string
@     PrintTerm(mem[mem[goalframe]], mem[goalframe+1], MAXPRIO); newline()
	ldr r6, [r6]
	mov r2, #2
	add r0, r5, r6, LSL #2
	ldr r1, [r0, #4]
	ldr r0, [r5, r6, LSL #2]
	ldr r0, [r5, r0, LSL #2]
	bl _PrintTerm
	bl newline
.L583:
@     current := (mem[goalframe])+1;
	ldr r0, =_goalframe
	ldr r5, [r0]
	ldr r0, =_mem
	ldr r0, [r0, r5, LSL #2]
	add r0, r0, #1
	ldr r1, =_current
	str r0, [r1]
@     if goalframe > choice then lsp := goalframe-1 end;
	ldr r0, =_choice
	ldr r0, [r0]
	cmp r5, r0
	ble .L586
	sub r0, r5, #1
	ldr r1, =_lsp
	str r0, [r1]
.L586:
@     goalframe := mem[goalframe+1]
	ldr r5, =_goalframe
	ldr r0, =_mem
	ldr r1, [r5]
	add r0, r0, r1, LSL #2
	ldr r0, [r0, #4]
	str r0, [r5]
	b .L578
.L577:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc Backtrack();
_Backtrack:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   Restore();
	bl _Restore
@   current := mem[choice]; goalframe := mem[choice+1];
	ldr r5, =_mem
	ldr r6, =_choice
	ldr r7, [r6]
	ldr r8, [r5, r7, LSL #2]
	ldr r0, =_current
	str r8, [r0]
	add r0, r5, r7, LSL #2
	ldr r7, [r0, #4]
	ldr r9, =_goalframe
	str r7, [r9]
@   call := Deref(mem[current], goalframe);
	mov r1, r7
	ldr r0, [r5, r8, LSL #2]
	bl _Deref
	ldr r7, =_call
	str r0, [r7]
@   prok := mem[choice+2]; gsp := mem[choice+4];
	ldr r8, [r6]
	add r5, r5, r8, LSL #2
	ldr r0, [r5, #8]
	ldr r1, =_prok
	str r0, [r1]
	ldr r0, [r5, #16]
	ldr r1, =_gsp
	str r0, [r1]
@   lsp := choice-1; choice := mem[choice+3];
	sub r0, r8, #1
	ldr r1, =_lsp
	str r0, [r1]
	ldr r0, [r5, #12]
	str r0, [r6]
@   if dflag then 
	ldr r0, =_dflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L591
@     print_string("Redo"); print_string(": "); 
	mov r1, #5
	ldr r0, =g83
	bl print_string
	mov r1, #3
	ldr r0, =g84
	bl print_string
@     PrintTerm(call, goalframe, MAXPRIO); newline()
	mov r2, #2
	ldr r1, [r9]
	ldr r0, [r7]
	bl _PrintTerm
	bl newline
.L591:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Resume(flag: boolean);
_Resume:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   ok := flag;
	ldrb r0, [fp, #32]
	ldr r1, =_ok
	strb r0, [r1]
.L593:
@   while run do
	ldr r0, =_run
	ldrb r0, [r0]
	cmp r0, #0
	beq .L592
@     if ok then
	ldr r0, =_ok
	ldrb r0, [r0]
	cmp r0, #0
	beq .L597
@       if mem[current] = NULL then return end;
	ldr r0, =_mem
	ldr r1, =_current
	ldr r1, [r1]
	ldr r5, [r0, r1, LSL #2]
	cmp r5, #0
	beq .L592
@       call := Deref(mem[current], goalframe);
	ldr r6, =_goalframe
	ldr r1, [r6]
	mov r0, r5
	bl _Deref
	ldr r5, =_call
	str r0, [r5]
@       if dflag then 
	ldr r0, =_dflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L607
@     print_string("Call"); print_string(": "); 
	mov r1, #5
	ldr r0, =g85
	bl print_string
	mov r1, #3
	ldr r0, =g86
	bl print_string
@     PrintTerm(call, goalframe, MAXPRIO); newline()
	mov r2, #2
	ldr r1, [r6]
	ldr r0, [r5]
	bl _PrintTerm
	bl newline
.L607:
@       if (symtab[mem[call+1]].prok = NULL)
	ldr r5, =_symtab
	ldr r6, =_mem
	ldr r7, =_call
	ldr r0, [r7]
	add r0, r6, r0, LSL #2
	ldr r0, [r0, #4]
	add r8, r5, r0, LSL #4
	ldr r0, [r8, #12]
	cmp r0, #0
	bne .L610
	ldr r0, [r8, #8]
	cmp r0, #0
	bne .L610
@ 	newline(); print_string("Error: "); print_string("call to undefined relation "); run := false;
	bl newline
	mov r1, #8
	ldr r0, =g87
	bl print_string
	mov r1, #28
	ldr r0, =g88
	bl print_string
	mov r0, #0
	ldr r1, =_run
	strb r0, [r1]
@ 	WriteString(symtab[mem[call+1]].name);
	ldr r0, [r7]
	add r0, r6, r0, LSL #2
	ldr r0, [r0, #4]
	add r0, r5, r0, LSL #4
	ldr r0, [r0]
	bl _WriteString
	b .L592
.L610:
@       prok := Search(call, goalframe, symtab[mem[call+1]].prok)
	ldr r0, =_call
	ldr r5, [r0]
	ldr r0, =_symtab
	ldr r1, =_mem
	add r1, r1, r5, LSL #2
	ldr r1, [r1, #4]
	add r0, r0, r1, LSL #4
	ldr r2, [r0, #12]
	ldr r0, =_goalframe
	ldr r1, [r0]
	mov r0, r5
	bl _Search
	ldr r1, =_prok
	str r0, [r1]
	b .L598
.L597:
@       if choice <= base then return end;
	ldr r0, =_choice
	ldr r0, [r0]
	ldr r1, =_base
	ldr r1, [r1]
	cmp r0, r1
	ble .L592
@       Backtrack()
	bl _Backtrack
.L598:
@     Step();
	bl _Step
@     if ok then Unwind() end;
	ldr r0, =_ok
	ldrb r0, [r0]
	cmp r0, #0
	beq .L593
	bl _Unwind
	b .L593
.L592:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc Execute(g: clause);
_Execute:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   lsp := hp; gsp := MEMSIZE+1;
	ldr r0, =_hp
	ldr r0, [r0]
	ldr r1, =_lsp
	str r0, [r1]
	ldr r0, =25001
	ldr r1, =_gsp
	str r0, [r1]
@   current := NULL; goalframe := NULL; choice := NULL; trhead := NULL;
	ldr r6, =_current
	mov r0, #0
	str r0, [r6]
	ldr r7, =_goalframe
	mov r0, #0
	str r0, [r7]
	ldr r8, =_choice
	mov r0, #0
	str r0, [r8]
	mov r0, #0
	ldr r1, =_trhead
	str r0, [r1]
@   PushFrame(mem[g], NULL);
	mov r1, #0
	ldr r0, =_mem
	ldr r2, [fp, #32]
	ldr r0, [r0, r2, LSL #2]
	bl _PushFrame
@   choice := goalframe; base := goalframe; current := (g+4);
	ldr r7, [r7]
	str r7, [r8]
	ldr r0, =_base
	str r7, [r0]
	ldr r0, [fp, #32]
	add r0, r0, #4
	str r0, [r6]
@   run := true;
	ldr r6, =_run
	mov r0, #1
	strb r0, [r6]
@   Resume(true);
	mov r0, #1
	bl _Resume
@   if not run then return end;
	ldrb r0, [r6]
	cmp r0, #0
	beq .L615
.L619:
@   while ok do
	ldr r0, =_ok
	ldrb r0, [r0]
	cmp r0, #0
	beq .L621
@     nsoln := nsoln+1;
	add r5, r5, #1
@     ShowAnswer(base);
	ldr r0, =_base
	ldr r0, [r0]
	bl _ShowAnswer
@     newline();
	bl newline
@     Resume(false);
	mov r0, #0
	bl _Resume
@     if not run then return end;
	ldr r0, =_run
	ldrb r0, [r0]
	cmp r0, #0
	beq .L615
	b .L619
.L621:
@   if nsoln = 0 then
	cmp r5, #0
	bne .L615
@     print_string("no"); newline(); newline();
	mov r1, #3
	ldr r0, =g89
	bl print_string
	bl newline
	bl newline
.L615:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc GetArgs();
_GetArgs:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   for i := 1 to symtab[mem[call+1]].arity do
	mov r5, #1
	ldr r0, =_symtab
	ldr r1, =_mem
	ldr r2, =_call
	ldr r2, [r2]
	add r1, r1, r2, LSL #2
	ldr r1, [r1, #4]
	add r0, r0, r1, LSL #4
	ldr r6, [r0, #4]
.L629:
	cmp r5, r6
	bgt .L628
@     av[i] := Deref(mem[call+i+1], goalframe)
	ldr r0, =_goalframe
	ldr r1, [r0]
	ldr r0, =_mem
	ldr r2, =_call
	ldr r2, [r2]
	add r2, r2, r5
	add r0, r0, r2, LSL #2
	ldr r0, [r0, #4]
	bl _Deref
	ldr r1, =_av
	str r0, [r1, r5, LSL #2]
	add r5, r5, #1
	b .L629
.L628:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc NewInt(n: integer): term;
_NewInt:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   t := GloAlloc(INT, TERM_SIZE);
	mov r1, #2
	mov r0, #2
	bl _GloAlloc
	mov r5, r0
@   mem[t+1] := n;
	ldr r0, [fp, #24]
	ldr r1, =_mem
	add r1, r1, r5, LSL #2
	str r0, [r1, #4]
@   return t
	mov r0, r5
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc DoCut(): boolean;
_DoCut:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   choice := mem[goalframe+3];
	ldr r0, =_goalframe
	ldr r5, [r0]
	ldr r0, =_mem
	add r6, r0, r5, LSL #2
	ldr r0, [r6, #12]
	ldr r1, =_choice
	str r0, [r1]
@   lsp := goalframe + (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE) - 1;
	ldr r0, [r6, #24]
	lsl r0, r0, #1
	add r0, r0, #7
	add r0, r5, r0
	sub r0, r0, #1
	ldr r1, =_lsp
	str r0, [r1]
@   Commit();
	bl _Commit
@   current := (current)+1;
	ldr r5, =_current
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
@   return true
	mov r0, #1
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc DoCall(): boolean;
_DoCall:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   GetArgs();
	bl _GetArgs
@   if not (lsr(mem[av[1]], 8) = FUNC) then
	ldr r0, =_mem
	ldr r1, =_av
	ldr r1, [r1, #4]
	ldr r0, [r0, r1, LSL #2]
	lsr r0, r0, #8
	cmp r0, #1
	beq .L635
@     newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
	bl newline
	mov r1, #8
	ldr r0, =g90
	bl print_string
	mov r1, #23
	ldr r0, =g91
	bl print_string
	mov r0, #0
	ldr r1, =_run
	strb r0, [r1]
@     return false
	mov r0, #0
	b .L633
.L635:
@     PushFrame(1, NULL);
	mov r1, #0
	mov r0, #1
	bl _PushFrame
@     mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
	ldr r5, =_mem
	ldr r6, =_goalframe
	ldr r0, [r6]
	add r0, r5, r0, LSL #2
	ldr r1, [r0, #4]
	ldr r0, =_av
	ldr r0, [r0, #4]
	bl _GloCopy
	ldr r1, [r6]
	add r1, r5, r1, LSL #2
	str r0, [r1, #32]
@     current := callbody;
	ldr r0, =_callbody
	ldr r0, [r0]
	ldr r1, =_current
	str r0, [r1]
@     return true
	mov r0, #1
.L633:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc DoNot(): boolean;
_DoNot:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   GetArgs();
	bl _GetArgs
@   if not (lsr(mem[av[1]], 8) = FUNC) then
	ldr r0, =_mem
	ldr r1, =_av
	ldr r1, [r1, #4]
	ldr r0, [r0, r1, LSL #2]
	lsr r0, r0, #8
	cmp r0, #1
	beq .L639
@     newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
	bl newline
	mov r1, #8
	ldr r0, =g92
	bl print_string
	mov r1, #23
	ldr r0, =g93
	bl print_string
	mov r0, #0
	ldr r1, =_run
	strb r0, [r1]
@     return false
	mov r0, #0
	b .L637
.L639:
@     PushFrame(1, NULL);
	mov r1, #0
	mov r0, #1
	bl _PushFrame
@     savebase := base; base := goalframe; choice := goalframe;
	ldr r6, =_base
	ldr r5, [r6]
	ldr r7, =_goalframe
	ldr r8, [r7]
	str r8, [r6]
	ldr r9, =_choice
	str r8, [r9]
@     mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
	ldr r10, =_mem
	add r0, r10, r8, LSL #2
	ldr r1, [r0, #4]
	ldr r0, =_av
	ldr r0, [r0, #4]
	bl _GloCopy
	ldr r1, [r7]
	add r1, r10, r1, LSL #2
	str r0, [r1, #32]
@     current := callbody;
	ldr r8, =_current
	ldr r0, =_callbody
	ldr r0, [r0]
	str r0, [r8]
@     Resume(true);
	mov r0, #1
	bl _Resume
@     choice := mem[base+3]; goalframe := mem[base+1];
	ldr r6, [r6]
	add r0, r10, r6, LSL #2
	ldr r1, [r0, #12]
	str r1, [r9]
	ldr r0, [r0, #4]
	str r0, [r7]
@     if not ok then
	ldr r0, =_ok
	ldrb r0, [r0]
	cmp r0, #0
	bne .L642
@       current := (mem[base])+1;
	ldr r0, [r10, r6, LSL #2]
	add r0, r0, #1
	str r0, [r8]
@       return true
	mov r0, #1
	b .L637
.L642:
@       Commit();
	bl _Commit
@       return false
	mov r0, #0
.L637:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoPlus(): boolean;
_DoPlus:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   GetArgs();
	bl _GetArgs
@   result := false;
	mov r5, #0
@   if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
	ldr r6, =_mem
	ldr r7, =_av
	ldr r8, [r7, #4]
	ldr r0, [r6, r8, LSL #2]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L646
	ldr r9, [r7, #8]
	ldr r0, [r6, r9, LSL #2]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L646
@     result := Unify(av[3], goalframe, NewInt(mem[av[1]+1] + mem[av[2]+1]), NULL)
	add r0, r6, r8, LSL #2
	ldr r0, [r0, #4]
	add r1, r6, r9, LSL #2
	ldr r1, [r1, #4]
	add r0, r0, r1
	bl _NewInt
	mov r3, #0
	mov r2, r0
	ldr r0, =_goalframe
	ldr r1, [r0]
	ldr r0, [r7, #12]
	bl _Unify
	mov r5, r0
	b .L647
.L646:
@   elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
	ldr r6, =_mem
	ldr r7, =_av
	ldr r8, [r7, #4]
	ldr r0, [r6, r8, LSL #2]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L649
	ldr r9, [r7, #12]
	ldr r0, [r6, r9, LSL #2]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L649
@     if mem[av[1]+1] <= mem[av[3]+1] then
	add r0, r6, r8, LSL #2
	ldr r8, [r0, #4]
	add r0, r6, r9, LSL #2
	ldr r6, [r0, #4]
	cmp r8, r6
	bgt .L647
@       result := Unify(av[2], goalframe, 
	sub r0, r6, r8
	bl _NewInt
	mov r3, #0
	mov r2, r0
	ldr r0, =_goalframe
	ldr r1, [r0]
	ldr r0, [r7, #8]
	bl _Unify
	mov r5, r0
	b .L647
.L649:
@   elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
	ldr r6, =_mem
	ldr r7, =_av
	ldr r8, [r7, #8]
	ldr r0, [r6, r8, LSL #2]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L652
	ldr r9, [r7, #12]
	ldr r0, [r6, r9, LSL #2]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L652
@     if mem[av[2]+1] <= mem[av[3]+1] then
	add r0, r6, r8, LSL #2
	ldr r8, [r0, #4]
	add r0, r6, r9, LSL #2
	ldr r6, [r0, #4]
	cmp r8, r6
	bgt .L647
@       result := Unify(av[1], goalframe, NewInt(mem[av[3]+1] - mem[av[2]+1]), NULL)
	sub r0, r6, r8
	bl _NewInt
	mov r3, #0
	mov r2, r0
	ldr r0, =_goalframe
	ldr r1, [r0]
	ldr r0, [r7, #4]
	bl _Unify
	mov r5, r0
	b .L647
.L652:
@     newline(); print_string("Error: "); print_string("plus/3 needs at least two integers"); run := false
	bl newline
	mov r1, #8
	ldr r0, =g94
	bl print_string
	mov r1, #35
	ldr r0, =g95
	bl print_string
	mov r0, #0
	ldr r1, =_run
	strb r0, [r1]
.L647:
@   current := (current)+1;
	ldr r6, =_current
	ldr r0, [r6]
	add r0, r0, #1
	str r0, [r6]
@   return result
	mov r0, r5
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoTimes(): boolean;
_DoTimes:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   GetArgs();
	bl _GetArgs
@   result := false;
	mov r5, #0
@   if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
	ldr r6, =_mem
	ldr r7, =_av
	ldr r8, [r7, #4]
	ldr r0, [r6, r8, LSL #2]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L665
	ldr r9, [r7, #8]
	ldr r0, [r6, r9, LSL #2]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L665
@     result := Unify(av[3], goalframe, 
	add r0, r6, r8, LSL #2
	ldr r0, [r0, #4]
	add r1, r6, r9, LSL #2
	ldr r1, [r1, #4]
	mul r0, r0, r1
	bl _NewInt
	mov r3, #0
	mov r2, r0
	ldr r0, =_goalframe
	ldr r1, [r0]
	ldr r0, [r7, #12]
	bl _Unify
	mov r5, r0
	b .L666
.L665:
@   elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
	ldr r6, =_mem
	ldr r7, =_av
	ldr r8, [r7, #4]
	ldr r0, [r6, r8, LSL #2]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L668
	ldr r9, [r7, #12]
	ldr r0, [r6, r9, LSL #2]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L668
@     if mem[av[1]+1] <> 0 then
	add r0, r6, r8, LSL #2
	ldr r8, [r0, #4]
	cmp r8, #0
	beq .L666
@       if mem[av[3]+1] mod mem[av[1]+1] = 0 then
	mov r1, r8
	add r0, r6, r9, LSL #2
	ldr r0, [r0, #4]
	bl int_mod
	cmp r0, #0
	bne .L666
@         result := Unify(av[2], goalframe, 
	ldr r0, [r7, #4]
	add r0, r6, r0, LSL #2
	ldr r1, [r0, #4]
	ldr r0, [r7, #12]
	add r0, r6, r0, LSL #2
	ldr r0, [r0, #4]
	bl int_div
	bl _NewInt
	mov r3, #0
	mov r2, r0
	ldr r0, =_goalframe
	ldr r1, [r0]
	ldr r0, [r7, #8]
	bl _Unify
	mov r5, r0
	b .L666
.L668:
@   elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
	ldr r6, =_mem
	ldr r7, =_av
	ldr r8, [r7, #8]
	ldr r0, [r6, r8, LSL #2]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L671
	ldr r9, [r7, #12]
	ldr r0, [r6, r9, LSL #2]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L671
@     if mem[av[2]+1] <> 0 then
	add r0, r6, r8, LSL #2
	ldr r8, [r0, #4]
	cmp r8, #0
	beq .L666
@       if mem[av[3]+1] mod mem[av[2]+1] = 0 then
	mov r1, r8
	add r0, r6, r9, LSL #2
	ldr r0, [r0, #4]
	bl int_mod
	cmp r0, #0
	bne .L666
@         result := Unify(av[1], goalframe, 
	ldr r0, [r7, #8]
	add r0, r6, r0, LSL #2
	ldr r1, [r0, #4]
	ldr r0, [r7, #12]
	add r0, r6, r0, LSL #2
	ldr r0, [r0, #4]
	bl int_div
	bl _NewInt
	mov r3, #0
	mov r2, r0
	ldr r0, =_goalframe
	ldr r1, [r0]
	ldr r0, [r7, #4]
	bl _Unify
	mov r5, r0
	b .L666
.L671:
@     newline(); print_string("Error: "); print_string("times/3 needs at least two integers"); run := false
	bl newline
	mov r1, #8
	ldr r0, =g96
	bl print_string
	mov r1, #36
	ldr r0, =g97
	bl print_string
	mov r0, #0
	ldr r1, =_run
	strb r0, [r1]
.L666:
@   current := (current)+1;
	ldr r6, =_current
	ldr r0, [r6]
	add r0, r0, #1
	str r0, [r6]
@   return result
	mov r0, r5
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoEqual(): boolean;
_DoEqual:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   GetArgs();
	bl _GetArgs
@   current := (current)+1;
	ldr r5, =_current
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
@   return Unify(av[1], goalframe, av[2], goalframe)
	ldr r5, =_av
	ldr r0, =_goalframe
	ldr r6, [r0]
	mov r3, r6
	ldr r2, [r5, #8]
	mov r1, r6
	ldr r0, [r5, #4]
	bl _Unify
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc DoInteger(): boolean;
_DoInteger:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   GetArgs();
	bl _GetArgs
@   current := (current)+1;
	ldr r5, =_current
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
@   return (lsr(mem[av[1]], 8) = INT)
	ldr r0, =_mem
	ldr r1, =_av
	ldr r1, [r1, #4]
	ldr r0, [r0, r1, LSL #2]
	lsr r0, r0, #8
	cmp r0, #2
	mov r0, #0
	moveq r0, #1
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc DoChar(): boolean;
_DoChar:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   GetArgs();
	bl _GetArgs
@   current := (current)+1;
	ldr r5, =_current
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
@   return (lsr(mem[av[1]], 8) = CHRCTR)
	ldr r0, =_mem
	ldr r1, =_av
	ldr r1, [r1, #4]
	ldr r0, [r0, r1, LSL #2]
	lsr r0, r0, #8
	cmp r0, #3
	mov r0, #0
	moveq r0, #1
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc DoPrint(): boolean;
_DoPrint:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   GetArgs();
	bl _GetArgs
@   PrintTerm(av[1], goalframe, MAXPRIO);
	mov r2, #2
	ldr r0, =_goalframe
	ldr r1, [r0]
	ldr r0, =_av
	ldr r0, [r0, #4]
	bl _PrintTerm
@   current := (current)+1;
	ldr r5, =_current
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
@   return true
	mov r0, #1
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc DoNl(): boolean;
_DoNl:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   newline();
	bl newline
@   current := (current)+1;
	ldr r5, =_current
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
@   return true
	mov r0, #1
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc DoBuiltin(action: integer): boolean;
_DoBuiltin:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   case action of
	ldr r0, [fp, #16]
	sub r0, r0, #1
	cmp r0, #11
	ldrlo pc, [pc, r0, LSL #2]
	b .L694
	.word .L696
	.word .L697
	.word .L698
	.word .L699
	.word .L700
	.word .L701
	.word .L702
	.word .L703
	.word .L704
	.word .L705
	.word .L706
.L696:
@     CUT:      return DoCut()
	bl _DoCut
	b .L693
.L697:
@   | CALL:     return DoCall()
	bl _DoCall
	b .L693
.L698:
@   | PLUS:     return DoPlus()
	bl _DoPlus
	b .L693
.L699:
@   | TIMES:    return DoTimes()
	bl _DoTimes
	b .L693
.L700:
@   | ISINT:    return DoInteger()
	bl _DoInteger
	b .L693
.L701:
@   | ISCHAR:   return DoChar()
	bl _DoChar
	b .L693
.L702:
@   | NAFF:     return DoNot()
	bl _DoNot
	b .L693
.L703:
@   | EQUALITY: return DoEqual()
	bl _DoEqual
	b .L693
.L704:
@   | FAIL:     return false
	mov r0, #0
	b .L693
.L705:
@   | PRINT:    return DoPrint()
	bl _DoPrint
	b .L693
.L706:
@   | NL:	      return DoNl()
	bl _DoNl
	b .L693
.L694:
@     newline(); print_string("Panic: "); print_string("bad tag" (*action:1, " in ", "DoBuiltin"*)); newline(); exit(2)
	bl newline
	mov r1, #8
	ldr r0, =g98
	bl print_string
	mov r1, #8
	ldr r0, =g99
	bl print_string
	bl newline
	mov r0, #2
	bl exit
.L693:
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

@ proc Initialize();
_Initialize:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   dflag := false; errcount := 0;
	mov r0, #0
	ldr r1, =_dflag
	strb r0, [r1]
	mov r0, #0
	ldr r1, =_errcount
	str r0, [r1]
@   pbchar := ENDFILE; charptr := 0;
	mov r0, #127
	ldr r1, =_pbchar
	strb r0, [r1]
	mov r0, #0
	ldr r1, =_charptr
	str r0, [r1]
@   hp := 0; InitSymbols();
	mov r0, #0
	ldr r1, =_hp
	str r0, [r1]
	bl _InitSymbols
@   for i := 1 to MAXARITY do
	mov r5, #1
	mov r7, #63
.L708:
	cmp r5, r7
	bgt .L709
@     p := HeapAlloc(TERM_SIZE);
	mov r0, #2
	bl _HeapAlloc
	mov r6, r0
@     mem[p] := lsl(REF, 8) + TERM_SIZE;
	ldr r8, =_mem
	ldr r0, =1282
	str r0, [r8, r6, LSL #2]
@     mem[p+1] := i; refnode[i] := p
	add r0, r8, r6, LSL #2
	str r5, [r0, #4]
	ldr r0, =_refnode
	str r6, [r0, r5, LSL #2]
	add r5, r5, #1
	b .L708
.L709:
@   callbody := HeapAlloc(2);
	mov r0, #2
	bl _HeapAlloc
	ldr r8, =_callbody
	str r0, [r8]
@   mem[callbody] := MakeRef(1);
	mov r0, #1
	bl _MakeRef
	ldr r9, =_mem
	ldr r1, [r8]
	str r0, [r9, r1, LSL #2]
@   mem[(callbody)+1] := NULL
	mov r0, #0
	ldr r1, [r8]
	add r1, r9, r1, LSL #2
	str r0, [r1, #4]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc ReadFile();
_ReadFile:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   lineno := 1;
	mov r0, #1
	ldr r1, =_lineno
	str r0, [r1]
.L711:
@     hmark := hp;
	ldr r0, =_hp
	ldr r0, [r0]
	ldr r1, =_hmark
	str r0, [r1]
@     c := ReadClause();
	bl _ReadClause
	mov r5, r0
@     if c <> NULL then
	cmp r5, #0
	beq .L715
@       if dflag then PrintClause(c) end;	
	ldr r0, =_dflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L718
	mov r0, r5
	bl _PrintClause
.L718:
@       if mem[c+3] <> NULL then
	ldr r0, =_mem
	add r0, r0, r5, LSL #2
	ldr r0, [r0, #12]
	cmp r0, #0
	beq .L720
@         AddClause(c)
	mov r0, r5
	bl _AddClause
	b .L715
.L720:
@         Execute(c);
	mov r0, r5
	bl _Execute
@ 	hp := hmark
	ldr r0, =_hmark
	ldr r0, [r0]
	ldr r1, =_hp
	str r0, [r1]
.L715:
	cmp r5, #0
	bne .L711
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   prog("subject(                                                    ");
	ldr r0, =g100
	bl _prog
@   prog("  <store,                                                   ");
	ldr r0, =g101
	bl _prog
@   prog("    <load,                                                  ");
	ldr r0, =g102
	bl _prog
@   prog("      <plusa,                                               ");
	ldr r0, =g103
	bl _prog
@   prog("        <global(a)>,                                        ");
	ldr r0, =g104
	bl _prog
@   prog("        <lsl, <load, <local(16)>>, <const(2)>>>>,           ");
	ldr r0, =g105
	bl _prog
@   prog("    <local(20)>>                                            ");
	ldr r0, =g106
	bl _prog
@   prog(") :- .                                                      ");
	ldr r0, =g107
	bl _prog
@   prog("rule(""*str"", stmt, <store, reg, addr>) :- .                 ");
	ldr r0, =g108
	bl _prog
@   prog("rule(""*ldr"", reg,  <load, addr>) :- .                       ");
	ldr r0, =g109
	bl _prog
@   prog("rule(""*addfp"", reg, <local(N)>) :- .                        ");
	ldr r0, =g110
	bl _prog
@   prog("rule(""local"", addr, <local(N)>) :- .                        ");
	ldr r0, =g111
	bl _prog
@   prog("rule(""*add"", reg, <plusa, reg, rand>) :- .                  ");
	ldr r0, =g112
	bl _prog
@   prog("rule(""index"", addr, <plusa, reg, reg>) :- .                 ");
	ldr r0, =g113
	bl _prog
@   prog("rule(""scale"", addr,                                         ");
	ldr r0, =g114
	bl _prog
@   prog("       <plusa, reg, <lsl, reg, <const(N)>>>) :- .           ");
	ldr r0, =g115
	bl _prog
@   prog("rule(""*global"", reg, <global(X)>) :- .                      ");
	ldr r0, =g116
	bl _prog
@   prog("rule(""*lsl"", reg, <lsl, reg, rand>) :- .                    ");
	ldr r0, =g117
	bl _prog
@   prog("rule(""lshiftc"", rand, <lsl, reg, <const(N)>>) :- .          ");
	ldr r0, =g118
	bl _prog
@   prog("rule(""lshiftr"", rand, <lsl, reg, reg>) :- .                 ");
	ldr r0, =g119
	bl _prog
@   prog("rule(""*mov"", reg, <const(N)>) :- .                          ");
	ldr r0, =g120
	bl _prog
@   prog("rule(""const"", rand, <const(N)>) :- .                        ");
	ldr r0, =g121
	bl _prog
@   prog("rule(""reg"", rand, reg) :- .                                 ");
	ldr r0, =g122
	bl _prog
@   prog("rule(""indir"", addr, reg) :- .                               ");
	ldr r0, =g123
	bl _prog
@   prog("use_rule(NT, Tree, node(Name, Kids)) :-                     ");
	ldr r0, =g124
	bl _prog
@   prog("  rule(Name, NT, RHS), match(RHS, Tree, Kids, nil).         ");
	ldr r0, =g125
	bl _prog
@   prog("match(NT, Tree, Parse:Kids0, Kids0) :-                      ");
	ldr r0, =g126
	bl _prog
@   prog("  use_rule(NT, Tree, Parse).                                ");
	ldr r0, =g127
	bl _prog
@   prog("match(node(W, PS), node(W, TS), Kids, Kids0) :-             ");
	ldr r0, =g128
	bl _prog
@   prog("  matchall(PS, TS, Kids, Kids0).                            ");
	ldr r0, =g129
	bl _prog
@   prog("matchall(nil, nil, Kids0, Kids0) :- .                       ");
	ldr r0, =g130
	bl _prog
@   prog("matchall(P:PS, T:TS, Kids, Kids0) :-                        ");
	ldr r0, =g131
	bl _prog
@   prog("  match(P, T, Kids, Kids1), matchall(PS, TS, Kids1, Kids0). ");
	ldr r0, =g132
	bl _prog
@   prog("cost(node(X, TS), C) :-                                     ");
	ldr r0, =g133
	bl _prog
@   prog("  opcost(X, A), allcosts(TS, B), plus(A, B, C).             ");
	ldr r0, =g134
	bl _prog
@   prog("allcosts(nil, 0) :- .                                       ");
	ldr r0, =g135
	bl _prog
@   prog("allcosts(T:TS, C) :-                                        ");
	ldr r0, =g136
	bl _prog
@   prog("  cost(T, A), allcosts(TS, B), plus(A, B, C).               ");
	ldr r0, =g137
	bl _prog
@   prog("opcost('*':_, 1) :- !.                                      ");
	ldr r0, =g138
	bl _prog
@   prog("opcost(_, 0) :- .                                           ");
	ldr r0, =g139
	bl _prog
@   prog("answer(P, C) :-                                             ");
	ldr r0, =g140
	bl _prog
@   prog("  subject(T), use_rule(stmt, T, P), cost(P, C).             ");
	ldr r0, =g141
	bl _prog
@   prog("min(N, P) :- min1(N, 0, P).                                 ");
	ldr r0, =g142
	bl _prog
@   prog("min1(N, N, P) :- call(P), !.                                ");
	ldr r0, =g143
	bl _prog
@   prog("min1(N, N0, P) :- plus(N0, 1, N1), min1(N, N1, P).          ");
	ldr r0, =g144
	bl _prog
@   prog("# :- answer(P, C).                                          ");
	ldr r0, =g145
	bl _prog
@   Initialize();
	bl _Initialize
@   ReadFile()
	bl _ReadFile
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

	.comm _run, 1, 4
	.comm _dflag, 1, 4
	.comm _charptr, 4, 4
	.comm _charbuf, 2048, 4
	.comm _lsp, 4, 4
	.comm _gsp, 4, 4
	.comm _hp, 4, 4
	.comm _hmark, 4, 4
	.comm _mem, 100004, 4
	.comm _infile, 3000, 4
	.comm _pin, 4, 4
	.comm _pout, 4, 4
	.comm _pbchar, 1, 4
	.comm _lineno, 4, 4
	.comm _current, 4, 4
	.comm _call, 4, 4
	.comm _goalframe, 4, 4
	.comm _choice, 4, 4
	.comm _base, 4, 4
	.comm _prok, 4, 4
	.comm _nsymbols, 4, 4
	.comm _symtab, 8208, 4
	.comm _cons, 4, 4
	.comm _eqsym, 4, 4
	.comm _cutsym, 4, 4
	.comm _nilsym, 4, 4
	.comm _notsym, 4, 4
	.comm _node, 4, 4
	.comm _refnode, 256, 4
	.comm _token, 4, 4
	.comm _tokval, 4, 4
	.comm _tokival, 4, 4
	.comm _toksval, 128, 4
	.comm _errflag, 1, 4
	.comm _errcount, 4, 4
	.comm _nvars, 4, 4
	.comm _vartable, 256, 4
	.comm _trhead, 4, 4
	.comm _ok, 1, 4
	.comm _av, 256, 4
	.comm _callbody, 4, 4
	.section .rodata
g1:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g2:
	.byte 111, 117, 116, 32, 111, 102, 32, 115, 116, 114
	.byte 105, 110, 103, 32, 115, 112, 97, 99, 101
	.byte 0
g3:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g4:
	.byte 111, 117, 116, 32, 111, 102, 32, 115, 116, 97
	.byte 99, 107, 32, 115, 112, 97, 99, 101
	.byte 0
g5:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g6:
	.byte 111, 117, 116, 32, 111, 102, 32, 115, 116, 97
	.byte 99, 107, 32, 115, 112, 97, 99, 101
	.byte 0
g7:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g8:
	.byte 111, 117, 116, 32, 111, 102, 32, 104, 101, 97
	.byte 112, 32, 115, 112, 97, 99, 101
	.byte 0
g9:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g10:
	.byte 68, 101, 114, 101, 102
	.byte 0
g11:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g12:
	.byte 111, 117, 116, 32, 111, 102, 32, 115, 121, 109
	.byte 98, 111, 108, 32, 115, 112, 97, 99, 101
	.byte 0
g13:
	.byte 58, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g14:
	.byte 33, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g15:
	.byte 61, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g16:
	.byte 110, 105, 108, 32, 32, 32, 32, 32
	.byte 0
g17:
	.byte 110, 111, 116, 32, 32, 32, 32, 32
	.byte 0
g18:
	.byte 110, 111, 100, 101, 32, 32, 32, 32
	.byte 0
g19:
	.byte 99, 97, 108, 108, 32, 32, 32, 32
	.byte 0
g20:
	.byte 112, 108, 117, 115, 32, 32, 32, 32
	.byte 0
g21:
	.byte 116, 105, 109, 101, 115, 32, 32, 32
	.byte 0
g22:
	.byte 105, 110, 116, 101, 103, 101, 114, 32
	.byte 0
g23:
	.byte 99, 104, 97, 114, 32, 32, 32, 32
	.byte 0
g24:
	.byte 102, 97, 108, 115, 101, 32, 32, 32
	.byte 0
g25:
	.byte 112, 114, 105, 110, 116, 32, 32, 32
	.byte 0
g26:
	.byte 110, 108, 32, 32, 32, 32, 32, 32
	.byte 0
g27:
	.byte 69, 114, 114, 111, 114, 58, 32
	.byte 0
g28:
	.byte 99, 97, 110, 110, 111, 116, 32, 97, 100, 100
	.byte 32, 99, 108, 97, 117, 115, 101, 115, 32, 116
	.byte 111, 32, 98, 117, 105, 108, 116, 45, 105, 110
	.byte 32, 114, 101, 108, 97, 116, 105, 111, 110, 32
	.byte 0
g29:
	.byte 32, 61, 32
	.byte 0
g30:
	.byte 110, 111, 116, 32
	.byte 0
g31:
	.byte 44, 32
	.byte 0
g32:
	.byte 44, 32
	.byte 0
g33:
	.byte 42, 110, 117, 108, 108, 45, 116, 101, 114, 109
	.byte 42
	.byte 0
g34:
	.byte 42, 117, 110, 107, 110, 111, 119, 110, 45, 116
	.byte 101, 114, 109, 40, 116, 97, 103, 61
	.byte 0
g35:
	.byte 41, 42
	.byte 0
g36:
	.byte 42, 110, 117, 108, 108, 45, 99, 108, 97, 117
	.byte 115, 101, 42
	.byte 0
g37:
	.byte 58, 45, 32
	.byte 0
g38:
	.byte 44, 32
	.byte 0
g39:
	.byte 76, 105, 110, 101, 32
	.byte 0
g40:
	.byte 83, 121, 110, 116, 97, 120, 32, 101, 114, 114
	.byte 111, 114, 32, 45, 32
	.byte 0
g41:
	.byte 84, 111, 111, 32, 109, 97, 110, 121, 32, 101
	.byte 114, 114, 111, 114, 115, 58, 32, 73, 32, 97
	.byte 109, 32, 103, 105, 118, 105, 110, 103, 32, 117
	.byte 112
	.byte 0
g42:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g43:
	.byte 105, 100, 101, 110, 116, 105, 102, 105, 101, 114
	.byte 32, 116, 111, 111, 32, 108, 111, 110, 103
	.byte 0
g44:
	.byte 98, 97, 100, 32, 116, 111, 107, 101, 110, 32
	.byte 47
	.byte 0
g45:
	.byte 101, 110, 100, 32, 111, 102, 32, 102, 105, 108
	.byte 101, 32, 105, 110, 32, 99, 111, 109, 109, 101
	.byte 110, 116
	.byte 0
g46:
	.byte 109, 105, 115, 115, 105, 110, 103, 32, 113, 117
	.byte 111, 116, 101
	.byte 0
g47:
	.byte 117, 110, 116, 101, 114, 109, 105, 110, 97, 116
	.byte 101, 100, 32, 115, 116, 114, 105, 110, 103
	.byte 0
g48:
	.byte 105, 108, 108, 101, 103, 97, 108, 32, 99, 104
	.byte 97, 114, 97, 99, 116, 101, 114
	.byte 0
g49:
	.byte 105, 100, 101, 110, 116, 105, 102, 105, 101, 114
	.byte 32
	.byte 0
g50:
	.byte 118, 97, 114, 105, 97, 98, 108, 101, 32
	.byte 0
g51:
	.byte 110, 117, 109, 98, 101, 114
	.byte 0
g52:
	.byte 99, 104, 97, 114, 32, 99, 111, 110, 115, 116
	.byte 97, 110, 116
	.byte 0
g53:
	.byte 58, 45
	.byte 0
g54:
	.byte 40
	.byte 0
g55:
	.byte 41
	.byte 0
g56:
	.byte 44
	.byte 0
g57:
	.byte 46
	.byte 0
g58:
	.byte 58
	.byte 0
g59:
	.byte 61
	.byte 0
g60:
	.byte 115, 116, 114, 105, 110, 103, 32, 99, 111, 110
	.byte 115, 116, 97, 110, 116
	.byte 0
g61:
	.byte 60
	.byte 0
g62:
	.byte 62
	.byte 0
g63:
	.byte 35
	.byte 0
g64:
	.byte 117, 110, 107, 110, 111, 119, 110, 32, 116, 111
	.byte 107, 101, 110
	.byte 0
g65:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g66:
	.byte 116, 111, 111, 32, 109, 97, 110, 121, 32, 118
	.byte 97, 114, 105, 97, 98, 108, 101, 115
	.byte 0
g67:
	.byte 121, 101, 115
	.byte 0
g68:
	.byte 32, 61, 32
	.byte 0
g69:
	.byte 101, 120, 112, 101, 99, 116, 101, 100, 32
	.byte 0
g70:
	.byte 44, 32, 102, 111, 117, 110, 100, 32
	.byte 0
g71:
	.byte 119, 114, 111, 110, 103, 32, 110, 117, 109, 98
	.byte 101, 114, 32, 111, 102, 32, 97, 114, 103, 115
	.byte 0
g72:
	.byte 101, 120, 112, 101, 99, 116, 101, 100, 32, 97
	.byte 32, 116, 101, 114, 109
	.byte 0
g73:
	.byte 108, 105, 116, 101, 114, 97, 108, 32, 109, 117
	.byte 115, 116, 32, 98, 101, 32, 97, 32, 99, 111
	.byte 109, 112, 111, 117, 110, 100, 32, 116, 101, 114
	.byte 109
	.byte 0
g74:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g75:
	.byte 98, 97, 100, 32, 116, 97, 103
	.byte 0
g76:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g77:
	.byte 75, 101, 121
	.byte 0
g78:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g79:
	.byte 98, 97, 100, 32, 116, 97, 103
	.byte 0
g80:
	.byte 40, 84, 82, 79, 41
	.byte 0
g81:
	.byte 69, 120, 105, 116
	.byte 0
g82:
	.byte 58, 32
	.byte 0
g83:
	.byte 82, 101, 100, 111
	.byte 0
g84:
	.byte 58, 32
	.byte 0
g85:
	.byte 67, 97, 108, 108
	.byte 0
g86:
	.byte 58, 32
	.byte 0
g87:
	.byte 69, 114, 114, 111, 114, 58, 32
	.byte 0
g88:
	.byte 99, 97, 108, 108, 32, 116, 111, 32, 117, 110
	.byte 100, 101, 102, 105, 110, 101, 100, 32, 114, 101
	.byte 108, 97, 116, 105, 111, 110, 32
	.byte 0
g89:
	.byte 110, 111
	.byte 0
g90:
	.byte 69, 114, 114, 111, 114, 58, 32
	.byte 0
g91:
	.byte 98, 97, 100, 32, 97, 114, 103, 117, 109, 101
	.byte 110, 116, 32, 116, 111, 32, 99, 97, 108, 108
	.byte 47, 49
	.byte 0
g92:
	.byte 69, 114, 114, 111, 114, 58, 32
	.byte 0
g93:
	.byte 98, 97, 100, 32, 97, 114, 103, 117, 109, 101
	.byte 110, 116, 32, 116, 111, 32, 99, 97, 108, 108
	.byte 47, 49
	.byte 0
g94:
	.byte 69, 114, 114, 111, 114, 58, 32
	.byte 0
g95:
	.byte 112, 108, 117, 115, 47, 51, 32, 110, 101, 101
	.byte 100, 115, 32, 97, 116, 32, 108, 101, 97, 115
	.byte 116, 32, 116, 119, 111, 32, 105, 110, 116, 101
	.byte 103, 101, 114, 115
	.byte 0
g96:
	.byte 69, 114, 114, 111, 114, 58, 32
	.byte 0
g97:
	.byte 116, 105, 109, 101, 115, 47, 51, 32, 110, 101
	.byte 101, 100, 115, 32, 97, 116, 32, 108, 101, 97
	.byte 115, 116, 32, 116, 119, 111, 32, 105, 110, 116
	.byte 101, 103, 101, 114, 115
	.byte 0
g98:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g99:
	.byte 98, 97, 100, 32, 116, 97, 103
	.byte 0
g100:
	.byte 115, 117, 98, 106, 101, 99, 116, 40, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g101:
	.byte 32, 32, 60, 115, 116, 111, 114, 101, 44, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g102:
	.byte 32, 32, 32, 32, 60, 108, 111, 97, 100, 44
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g103:
	.byte 32, 32, 32, 32, 32, 32, 60, 112, 108, 117
	.byte 115, 97, 44, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g104:
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 60, 103
	.byte 108, 111, 98, 97, 108, 40, 97, 41, 62, 44
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g105:
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 60, 108
	.byte 115, 108, 44, 32, 60, 108, 111, 97, 100, 44
	.byte 32, 60, 108, 111, 99, 97, 108, 40, 49, 54
	.byte 41, 62, 62, 44, 32, 60, 99, 111, 110, 115
	.byte 116, 40, 50, 41, 62, 62, 62, 62, 44, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g106:
	.byte 32, 32, 32, 32, 60, 108, 111, 99, 97, 108
	.byte 40, 50, 48, 41, 62, 62, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g107:
	.byte 41, 32, 58, 45, 32, 46, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g108:
	.byte 114, 117, 108, 101, 40, 34, 42, 115, 116, 114
	.byte 34, 44, 32, 115, 116, 109, 116, 44, 32, 60
	.byte 115, 116, 111, 114, 101, 44, 32, 114, 101, 103
	.byte 44, 32, 97, 100, 100, 114, 62, 41, 32, 58
	.byte 45, 32, 46, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g109:
	.byte 114, 117, 108, 101, 40, 34, 42, 108, 100, 114
	.byte 34, 44, 32, 114, 101, 103, 44, 32, 32, 60
	.byte 108, 111, 97, 100, 44, 32, 97, 100, 100, 114
	.byte 62, 41, 32, 58, 45, 32, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g110:
	.byte 114, 117, 108, 101, 40, 34, 42, 97, 100, 100
	.byte 102, 112, 34, 44, 32, 114, 101, 103, 44, 32
	.byte 60, 108, 111, 99, 97, 108, 40, 78, 41, 62
	.byte 41, 32, 58, 45, 32, 46, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g111:
	.byte 114, 117, 108, 101, 40, 34, 108, 111, 99, 97
	.byte 108, 34, 44, 32, 97, 100, 100, 114, 44, 32
	.byte 60, 108, 111, 99, 97, 108, 40, 78, 41, 62
	.byte 41, 32, 58, 45, 32, 46, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g112:
	.byte 114, 117, 108, 101, 40, 34, 42, 97, 100, 100
	.byte 34, 44, 32, 114, 101, 103, 44, 32, 60, 112
	.byte 108, 117, 115, 97, 44, 32, 114, 101, 103, 44
	.byte 32, 114, 97, 110, 100, 62, 41, 32, 58, 45
	.byte 32, 46, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g113:
	.byte 114, 117, 108, 101, 40, 34, 105, 110, 100, 101
	.byte 120, 34, 44, 32, 97, 100, 100, 114, 44, 32
	.byte 60, 112, 108, 117, 115, 97, 44, 32, 114, 101
	.byte 103, 44, 32, 114, 101, 103, 62, 41, 32, 58
	.byte 45, 32, 46, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g114:
	.byte 114, 117, 108, 101, 40, 34, 115, 99, 97, 108
	.byte 101, 34, 44, 32, 97, 100, 100, 114, 44, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g115:
	.byte 32, 32, 32, 32, 32, 32, 32, 60, 112, 108
	.byte 117, 115, 97, 44, 32, 114, 101, 103, 44, 32
	.byte 60, 108, 115, 108, 44, 32, 114, 101, 103, 44
	.byte 32, 60, 99, 111, 110, 115, 116, 40, 78, 41
	.byte 62, 62, 62, 41, 32, 58, 45, 32, 46, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g116:
	.byte 114, 117, 108, 101, 40, 34, 42, 103, 108, 111
	.byte 98, 97, 108, 34, 44, 32, 114, 101, 103, 44
	.byte 32, 60, 103, 108, 111, 98, 97, 108, 40, 88
	.byte 41, 62, 41, 32, 58, 45, 32, 46, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g117:
	.byte 114, 117, 108, 101, 40, 34, 42, 108, 115, 108
	.byte 34, 44, 32, 114, 101, 103, 44, 32, 60, 108
	.byte 115, 108, 44, 32, 114, 101, 103, 44, 32, 114
	.byte 97, 110, 100, 62, 41, 32, 58, 45, 32, 46
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g118:
	.byte 114, 117, 108, 101, 40, 34, 108, 115, 104, 105
	.byte 102, 116, 99, 34, 44, 32, 114, 97, 110, 100
	.byte 44, 32, 60, 108, 115, 108, 44, 32, 114, 101
	.byte 103, 44, 32, 60, 99, 111, 110, 115, 116, 40
	.byte 78, 41, 62, 62, 41, 32, 58, 45, 32, 46
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g119:
	.byte 114, 117, 108, 101, 40, 34, 108, 115, 104, 105
	.byte 102, 116, 114, 34, 44, 32, 114, 97, 110, 100
	.byte 44, 32, 60, 108, 115, 108, 44, 32, 114, 101
	.byte 103, 44, 32, 114, 101, 103, 62, 41, 32, 58
	.byte 45, 32, 46, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g120:
	.byte 114, 117, 108, 101, 40, 34, 42, 109, 111, 118
	.byte 34, 44, 32, 114, 101, 103, 44, 32, 60, 99
	.byte 111, 110, 115, 116, 40, 78, 41, 62, 41, 32
	.byte 58, 45, 32, 46, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g121:
	.byte 114, 117, 108, 101, 40, 34, 99, 111, 110, 115
	.byte 116, 34, 44, 32, 114, 97, 110, 100, 44, 32
	.byte 60, 99, 111, 110, 115, 116, 40, 78, 41, 62
	.byte 41, 32, 58, 45, 32, 46, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g122:
	.byte 114, 117, 108, 101, 40, 34, 114, 101, 103, 34
	.byte 44, 32, 114, 97, 110, 100, 44, 32, 114, 101
	.byte 103, 41, 32, 58, 45, 32, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g123:
	.byte 114, 117, 108, 101, 40, 34, 105, 110, 100, 105
	.byte 114, 34, 44, 32, 97, 100, 100, 114, 44, 32
	.byte 114, 101, 103, 41, 32, 58, 45, 32, 46, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g124:
	.byte 117, 115, 101, 95, 114, 117, 108, 101, 40, 78
	.byte 84, 44, 32, 84, 114, 101, 101, 44, 32, 110
	.byte 111, 100, 101, 40, 78, 97, 109, 101, 44, 32
	.byte 75, 105, 100, 115, 41, 41, 32, 58, 45, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g125:
	.byte 32, 32, 114, 117, 108, 101, 40, 78, 97, 109
	.byte 101, 44, 32, 78, 84, 44, 32, 82, 72, 83
	.byte 41, 44, 32, 109, 97, 116, 99, 104, 40, 82
	.byte 72, 83, 44, 32, 84, 114, 101, 101, 44, 32
	.byte 75, 105, 100, 115, 44, 32, 110, 105, 108, 41
	.byte 46, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g126:
	.byte 109, 97, 116, 99, 104, 40, 78, 84, 44, 32
	.byte 84, 114, 101, 101, 44, 32, 80, 97, 114, 115
	.byte 101, 58, 75, 105, 100, 115, 48, 44, 32, 75
	.byte 105, 100, 115, 48, 41, 32, 58, 45, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g127:
	.byte 32, 32, 117, 115, 101, 95, 114, 117, 108, 101
	.byte 40, 78, 84, 44, 32, 84, 114, 101, 101, 44
	.byte 32, 80, 97, 114, 115, 101, 41, 46, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g128:
	.byte 109, 97, 116, 99, 104, 40, 110, 111, 100, 101
	.byte 40, 87, 44, 32, 80, 83, 41, 44, 32, 110
	.byte 111, 100, 101, 40, 87, 44, 32, 84, 83, 41
	.byte 44, 32, 75, 105, 100, 115, 44, 32, 75, 105
	.byte 100, 115, 48, 41, 32, 58, 45, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g129:
	.byte 32, 32, 109, 97, 116, 99, 104, 97, 108, 108
	.byte 40, 80, 83, 44, 32, 84, 83, 44, 32, 75
	.byte 105, 100, 115, 44, 32, 75, 105, 100, 115, 48
	.byte 41, 46, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g130:
	.byte 109, 97, 116, 99, 104, 97, 108, 108, 40, 110
	.byte 105, 108, 44, 32, 110, 105, 108, 44, 32, 75
	.byte 105, 100, 115, 48, 44, 32, 75, 105, 100, 115
	.byte 48, 41, 32, 58, 45, 32, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g131:
	.byte 109, 97, 116, 99, 104, 97, 108, 108, 40, 80
	.byte 58, 80, 83, 44, 32, 84, 58, 84, 83, 44
	.byte 32, 75, 105, 100, 115, 44, 32, 75, 105, 100
	.byte 115, 48, 41, 32, 58, 45, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g132:
	.byte 32, 32, 109, 97, 116, 99, 104, 40, 80, 44
	.byte 32, 84, 44, 32, 75, 105, 100, 115, 44, 32
	.byte 75, 105, 100, 115, 49, 41, 44, 32, 109, 97
	.byte 116, 99, 104, 97, 108, 108, 40, 80, 83, 44
	.byte 32, 84, 83, 44, 32, 75, 105, 100, 115, 49
	.byte 44, 32, 75, 105, 100, 115, 48, 41, 46, 32
	.byte 0
g133:
	.byte 99, 111, 115, 116, 40, 110, 111, 100, 101, 40
	.byte 88, 44, 32, 84, 83, 41, 44, 32, 67, 41
	.byte 32, 58, 45, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g134:
	.byte 32, 32, 111, 112, 99, 111, 115, 116, 40, 88
	.byte 44, 32, 65, 41, 44, 32, 97, 108, 108, 99
	.byte 111, 115, 116, 115, 40, 84, 83, 44, 32, 66
	.byte 41, 44, 32, 112, 108, 117, 115, 40, 65, 44
	.byte 32, 66, 44, 32, 67, 41, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g135:
	.byte 97, 108, 108, 99, 111, 115, 116, 115, 40, 110
	.byte 105, 108, 44, 32, 48, 41, 32, 58, 45, 32
	.byte 46, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g136:
	.byte 97, 108, 108, 99, 111, 115, 116, 115, 40, 84
	.byte 58, 84, 83, 44, 32, 67, 41, 32, 58, 45
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g137:
	.byte 32, 32, 99, 111, 115, 116, 40, 84, 44, 32
	.byte 65, 41, 44, 32, 97, 108, 108, 99, 111, 115
	.byte 116, 115, 40, 84, 83, 44, 32, 66, 41, 44
	.byte 32, 112, 108, 117, 115, 40, 65, 44, 32, 66
	.byte 44, 32, 67, 41, 46, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g138:
	.byte 111, 112, 99, 111, 115, 116, 40, 39, 42, 39
	.byte 58, 95, 44, 32, 49, 41, 32, 58, 45, 32
	.byte 33, 46, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g139:
	.byte 111, 112, 99, 111, 115, 116, 40, 95, 44, 32
	.byte 48, 41, 32, 58, 45, 32, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g140:
	.byte 97, 110, 115, 119, 101, 114, 40, 80, 44, 32
	.byte 67, 41, 32, 58, 45, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g141:
	.byte 32, 32, 115, 117, 98, 106, 101, 99, 116, 40
	.byte 84, 41, 44, 32, 117, 115, 101, 95, 114, 117
	.byte 108, 101, 40, 115, 116, 109, 116, 44, 32, 84
	.byte 44, 32, 80, 41, 44, 32, 99, 111, 115, 116
	.byte 40, 80, 44, 32, 67, 41, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g142:
	.byte 109, 105, 110, 40, 78, 44, 32, 80, 41, 32
	.byte 58, 45, 32, 109, 105, 110, 49, 40, 78, 44
	.byte 32, 48, 44, 32, 80, 41, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g143:
	.byte 109, 105, 110, 49, 40, 78, 44, 32, 78, 44
	.byte 32, 80, 41, 32, 58, 45, 32, 99, 97, 108
	.byte 108, 40, 80, 41, 44, 32, 33, 46, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g144:
	.byte 109, 105, 110, 49, 40, 78, 44, 32, 78, 48
	.byte 44, 32, 80, 41, 32, 58, 45, 32, 112, 108
	.byte 117, 115, 40, 78, 48, 44, 32, 49, 44, 32
	.byte 78, 49, 41, 44, 32, 109, 105, 110, 49, 40
	.byte 78, 44, 32, 78, 49, 44, 32, 80, 41, 46
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g145:
	.byte 35, 32, 58, 45, 32, 97, 110, 115, 119, 101
	.byte 114, 40, 80, 44, 32, 67, 41, 46, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
@ End
]]*)
