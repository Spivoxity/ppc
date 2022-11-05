// picoPascal compiler output
	.global pmain

// proc StringLength(var s: tempstring): integer;
	.section .text
_StringLength:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0;
	mov w20, wzr
.L2:
//   while s[i] <> ENDSTR do i := i+1 end;
	ldr x0, [fp, #32]
	ldrb w0, [x0, w20, SXTW]
	cbz w0, .L4
	add w20, w20, #1
	b .L2
.L4:
//   return i
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc SaveString(var s: tempstring): permstring;
_SaveString:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if charptr + StringLength(s) + 1 > MAXCHARS then
	ldr x0, [fp, #64]
	bl _StringLength
	ldr x1, =_charptr
	ldr w1, [x1]
	add w0, w1, w0
	add w0, w0, #1
	cmp w0, #2048
	ble .L8
//     newline(); print_string("Panic: "); print_string("out of string space"); newline(); exit(2)
	bl newline
	mov w1, #8
	ldr x0, =g1
	bl print_string
	mov w1, #20
	ldr x0, =g2
	bl print_string
	bl newline
	mov w0, #2
	bl exit
.L8:
//   p := charptr; i := 0;
	ldr x0, =_charptr
	ldr w20, [x0]
	mov w21, wzr
.L9:
//     charbuf[charptr] := s[i]; charptr := charptr+1; i := i+1
	ldr x22, =_charbuf
	ldr x23, =_charptr
	ldr x0, [fp, #64]
	ldrb w0, [x0, w21, SXTW]
	ldr w1, [x23]
	strb w0, [x22, w1, SXTW]
	ldr w0, [x23]
	add w24, w0, #1
	str w24, [x23]
	add w21, w21, #1
	add x0, x22, w24, SXTW
	ldrb w0, [x0, #-1]
	cbnz w0, .L9
//   return p
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc StringEqual(var s1: tempstring; s2: permstring): boolean;
_StringEqual:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0;
	mov w20, wzr
.L12:
//   while (s1[i] <> ENDSTR) and (s1[i] = charbuf[s2+i]) do i := i+1 end;
	ldr x0, [fp, #48]
	ldrb w21, [x0, w20, SXTW]
	cbz w21, .L14
	ldr x0, =_charbuf
	ldr w1, [fp, #56]
	add x0, x0, w1, SXTW
	ldrb w0, [x0, w20, SXTW]
	cmp w21, w0
	bne .L14
	add w20, w20, #1
	b .L12
.L14:
//   return (s1[i] = charbuf[s2+i])
	ldr x0, [fp, #48]
	ldrb w0, [x0, w20, SXTW]
	ldr x1, =_charbuf
	ldr w2, [fp, #56]
	add x1, x1, w2, SXTW
	ldrb w1, [x1, w20, SXTW]
	cmp w0, w1
	cset w0, eq
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc WriteString(s: permstring);
_WriteString:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := s;
	ldr w20, [fp, #48]
.L17:
//   while charbuf[i] <> ENDSTR do
	ldr x0, =_charbuf
	ldrb w21, [x0, w20, SXTW]
	cbz w21, .L16
//     print_char(charbuf[i]); i := i+1
	mov x0, x21
	bl print_char
	add w20, w20, #1
	b .L17
.L16:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc LocAlloc(size: integer): ptr;
_LocAlloc:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if lsp + size >= gsp then newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2) end;
	ldr x0, =_lsp
	ldr w0, [x0]
	ldr w1, [fp, #48]
	add w0, w0, w1
	ldr x1, =_gsp
	ldr w1, [x1]
	cmp w0, w1
	blt .L23
	bl newline
	mov w1, #8
	ldr x0, =g3
	bl print_string
	mov w1, #19
	ldr x0, =g4
	bl print_string
	bl newline
	mov w0, #2
	bl exit
.L23:
//   p := lsp + 1; lsp := lsp + size; return p
	ldr x21, =_lsp
	ldr w22, [x21]
	add w20, w22, #1
	ldr w0, [fp, #48]
	add w0, w22, w0
	str w0, [x21]
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc GloAlloc(kind, size: integer): ptr;
_GloAlloc:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if gsp - size <= lsp then
	ldr x0, =_gsp
	ldr w0, [x0]
	ldr w1, [fp, #72]
	sub w0, w0, w1
	ldr x1, =_lsp
	ldr w1, [x1]
	cmp w0, w1
	bgt .L27
//     newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2)
	bl newline
	mov w1, #8
	ldr x0, =g5
	bl print_string
	mov w1, #19
	ldr x0, =g6
	bl print_string
	bl newline
	mov w0, #2
	bl exit
.L27:
//   gsp := gsp - size; p := gsp;
	ldr x21, =_gsp
	ldr w22, [fp, #72]
	ldr w0, [x21]
	sub w23, w0, w22
	str w23, [x21]
	mov w20, w23
//   mem[p] := lsl(kind, 8) + size;
	ldr w0, [fp, #64]
	lsl w0, w0, #8
	add w0, w0, w22
	ldr x1, =_mem
	str w0, [x1, w20, SXTW #2]
//   return p
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc HeapAlloc(size: integer): ptr;
_HeapAlloc:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if hp + size > MEMSIZE then newline(); print_string("Panic: "); print_string("out of heap space"); newline(); exit(2) end;
	ldr x0, =_hp
	ldr w0, [x0]
	ldr w1, [fp, #48]
	add w0, w0, w1
	ldr w1, =25000
	cmp w0, w1
	ble .L31
	bl newline
	mov w1, #8
	ldr x0, =g7
	bl print_string
	mov w1, #18
	ldr x0, =g8
	bl print_string
	bl newline
	mov w0, #2
	bl exit
.L31:
//   p := hp + 1; hp := hp + size; return p
	ldr x21, =_hp
	ldr w22, [x21]
	add w20, w22, #1
	ldr w0, [fp, #48]
	add w0, w22, w0
	str w0, [x21]
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc prog(line: array 61 of char);
_prog:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   for i := 0 to 59 do
	mov w20, wzr
	mov w21, #59
.L33:
	cmp w20, w21
	bgt .L34
//     infile[pin] := line[i]; pin := pin+1
	ldr x22, =_pin
	ldr x0, [fp, #48]
	ldrb w0, [x0, w20, SXTW]
	ldr x1, =_infile
	ldr w2, [x22]
	strb w0, [x1, w2, SXTW]
	ldr w0, [x22]
	add w0, w0, #1
	str w0, [x22]
	add w20, w20, #1
	b .L33
.L34:
//   infile[pin] := ENDLINE; pin := pin+1
	ldr x22, =_pin
	mov w0, #10
	ldr x1, =_infile
	ldr w2, [x22]
	strb w0, [x1, w2, SXTW]
	ldr w0, [x22]
	add w0, w0, #1
	str w0, [x22]
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc rdchar(var ch: char);
_rdchar:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if pout >= pin then
	ldr x0, =_pout
	ldr w0, [x0]
	ldr x1, =_pin
	ldr w1, [x1]
	cmp w0, w1
	blt .L37
//     ch := ENDFILE
	mov w0, #127
	ldr x1, [fp, #32]
	strb w0, [x1]
	b .L35
.L37:
//     ch := infile[pout]; pout := pout+1
	ldr x20, =_pout
	ldr x0, =_infile
	ldr w1, [x20]
	ldrb w0, [x0, w1, SXTW]
	ldr x1, [fp, #32]
	strb w0, [x1]
	ldr w0, [x20]
	add w0, w0, #1
	str w0, [x20]
.L35:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc GetChar(): char;
_GetChar:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #16
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if pbchar <> ENDFILE then
	ldr x20, =_pbchar
	ldrb w21, [x20]
	cmp w21, #127
	beq .L41
//     ch := pbchar; pbchar := ENDFILE
	strb w21, [fp, #31]
	mov w0, #127
	strb w0, [x20]
	b .L42
.L41:
//     rdchar(ch);
	add x0, fp, #31
	bl _rdchar
//     if ch = ENDLINE then lineno := lineno+1 end
	ldrb w0, [fp, #31]
	cmp w0, #10
	bne .L42
	ldr x20, =_lineno
	ldr w0, [x20]
	add w0, w0, #1
	str w0, [x20]
.L42:
//   return ch
	ldrb w0, [fp, #31]
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// proc PushBack(ch: char);
_PushBack:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   pbchar := ch
	ldrb w0, [fp, #16]
	ldr x1, =_pbchar
	strb w0, [x1]
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc Deref(t: term; e: frame): term;
_Deref:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if t = NULL then newline(); print_string("Panic: "); print_string("Deref"); newline(); exit(2) end;
	ldr w0, [fp, #48]
	cbnz w0, .L50
	bl newline
	mov w1, #8
	ldr x0, =g9
	bl print_string
	mov w1, #6
	ldr x0, =g10
	bl print_string
	bl newline
	mov w0, #2
	bl exit
.L50:
//   if (lsr(mem[t], 8) = REF) and (e <> NULL) then
	ldr x20, =_mem
	ldr w21, [fp, #48]
	ldr w0, [x20, w21, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #5
	bne .L55
	ldr w22, [fp, #56]
	cbz w22, .L55
//     t := (e+7+(mem[t+1]-1)*TERM_SIZE)
	add w0, w22, #7
	add x1, x20, w21, SXTW 2
	ldr w1, [x1, #4]
	lsl w1, w1, #1
	sub w1, w1, #2
	add w0, w0, w1
	str w0, [fp, #48]
.L55:
//   while (lsr(mem[t], 8) = CELL) and (mem[t+1] <> NULL) do
	ldr x20, =_mem
	ldr w21, [fp, #48]
	ldr w0, [x20, w21, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #4
	bne .L57
	add x0, x20, w21, SXTW 2
	ldr w20, [x0, #4]
	cbz w20, .L57
//     t := mem[t+1]
	str w20, [fp, #48]
	b .L55
.L57:
//   return t
	ldr w0, [fp, #48]
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc Lookup(var name: tempstring): symbol;
_Lookup:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   h := 0; i := 0;
	mov w20, wzr
	mov w21, wzr
.L60:
//   while name[i] <> ENDSTR do
	ldr x0, [fp, #64]
	ldrb w23, [x0, w21, SXTW]
	cbz w23, .L62
//     h := (5 * h + ord(name[i])) mod MAXSYMBOLS; i := i+1 
	mov w0, #5
	mul w0, w20, w0
	add w0, w0, w23
	and w20, w0, #511
	add w21, w21, #1
	b .L60
.L62:
//   p := h+1;
	add w22, w20, #1
.L63:
//   while symtab[p].name <> -1 do
	ldr x0, =_symtab
	add x0, x0, w22, SXTW 4
	ldr w23, [x0]
	cmp w23, #-1
	beq .L65
//     if StringEqual(name, symtab[p].name) then return p end;
	mov x1, x23
	ldr x0, [fp, #64]
	bl _StringEqual
	cbz w0, .L68
	mov w0, w22
	b .L59
.L68:
//     p := p-1;
	sub w22, w22, #1
//     if p = 0 then p := MAXSYMBOLS end
	cbnz w22, .L63
	mov w22, #512
	b .L63
.L65:
//   if nsymbols >= (MAXSYMBOLS div 10) * (HASHFACTOR div 10) then
	ldr x0, =_nsymbols
	ldr w0, [x0]
	cmp w0, #459
	blt .L74
//     newline(); print_string("Panic: "); print_string("out of symbol space"); newline(); exit(2)
	bl newline
	mov w1, #8
	ldr x0, =g11
	bl print_string
	mov w1, #20
	ldr x0, =g12
	bl print_string
	bl newline
	mov w0, #2
	bl exit
.L74:
//   symtab[p].name := SaveString(name);
	ldr x0, [fp, #64]
	bl _SaveString
	ldr x1, =_symtab
	add x23, x1, w22, SXTW 4
	str w0, [x23]
//   symtab[p].arity := -1;
	mov w0, #-1
	str w0, [x23, #4]
//   symtab[p].action := 0; symtab[p].prok := NULL;
	str wzr, [x23, #8]
	str wzr, [x23, #12]
//   return p
	mov w0, w22
.L59:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc Enter(name: keyword; arity: integer; action: integer): symbol;
_Enter:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #128
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0;
	mov w21, wzr
.L76:
//   while name[i] <> ' ' do
	ldr x0, [fp, #176]
	ldrb w22, [x0, w21, SXTW]
	cmp w22, #32
	beq .L78
//     temp[i] := name[i]; i := i+1 
	add x0, fp, #16
	strb w22, [x0, w21, SXTW]
	add w21, w21, #1
	b .L76
.L78:
//   temp[i] := ENDSTR; s := Lookup(temp);
	add x22, fp, #16
	strb wzr, [x22, w21, SXTW]
	mov x0, x22
	bl _Lookup
	mov w20, w0
//   symtab[s].arity := arity; symtab[s].action := action;
	ldr x0, =_symtab
	add x22, x0, w20, SXTW 4
	ldr w0, [fp, #184]
	str w0, [x22, #4]
	ldr w0, [fp, #192]
	str w0, [x22, #8]
//   return s
	mov w0, w20
	ldp fp, lr, [sp], #16
	add sp, sp, #128
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #32
	ret
	.pool

// proc InitSymbols();
_InitSymbols:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   nsymbols := 0;
	ldr x0, =_nsymbols
	str wzr, [x0]
//   for i := 1 to MAXSYMBOLS do symtab[i].name := -1 end;
	mov w20, #1
	mov w22, #512
.L80:
	cmp w20, w22
	bgt .L81
	mov w0, #-1
	ldr x1, =_symtab
	add x1, x1, w20, SXTW 4
	str w0, [x1]
	add w20, w20, #1
	b .L80
.L81:
//   cons   := Enter(":       ", 2, 0);
	mov w2, wzr
	mov w1, #2
	ldr x0, =g13
	bl _Enter
	ldr x1, =_cons
	str w0, [x1]
//   cutsym := Enter("!       ", 0, CUT);
	mov w2, #1
	mov w1, wzr
	ldr x0, =g14
	bl _Enter
	ldr x1, =_cutsym
	str w0, [x1]
//   eqsym  := Enter("=       ", 2, EQUALITY);
	mov w2, #8
	mov w1, #2
	ldr x0, =g15
	bl _Enter
	ldr x1, =_eqsym
	str w0, [x1]
//   nilsym := Enter("nil     ", 0, 0);
	mov w2, wzr
	mov w1, wzr
	ldr x0, =g16
	bl _Enter
	ldr x1, =_nilsym
	str w0, [x1]
//   notsym := Enter("not     ", 1, NAFF);
	mov w2, #7
	mov w1, #1
	ldr x0, =g17
	bl _Enter
	ldr x1, =_notsym
	str w0, [x1]
//   node   := Enter("node    ", 2, 0);
	mov w2, wzr
	mov w1, #2
	ldr x0, =g18
	bl _Enter
	ldr x1, =_node
	str w0, [x1]
//   dummy  := Enter("call    ", 1, CALL);
	mov w2, #2
	mov w1, #1
	ldr x0, =g19
	bl _Enter
	mov w21, w0
//   dummy  := Enter("plus    ", 3, PLUS);
	mov w2, #3
	mov w1, #3
	ldr x0, =g20
	bl _Enter
	mov w21, w0
//   dummy  := Enter("times   ", 3, TIMES);
	mov w2, #4
	mov w1, #3
	ldr x0, =g21
	bl _Enter
	mov w21, w0
//   dummy  := Enter("integer ", 1, ISINT);
	mov w2, #5
	mov w1, #1
	ldr x0, =g22
	bl _Enter
	mov w21, w0
//   dummy  := Enter("char    ", 1, ISCHAR);
	mov w2, #6
	mov w1, #1
	ldr x0, =g23
	bl _Enter
	mov w21, w0
//   dummy  := Enter("false   ", 0, FAIL);
	mov w2, #9
	mov w1, wzr
	ldr x0, =g24
	bl _Enter
	mov w21, w0
//   dummy  := Enter("print   ", 1, PRINT);
	mov w2, #10
	mov w1, #1
	ldr x0, =g25
	bl _Enter
	mov w21, w0
//   dummy  := Enter("nl      ", 0, NL)
	mov w2, #11
	mov w1, wzr
	ldr x0, =g26
	bl _Enter
	mov w21, w0
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// proc AddClause(c: clause);
_AddClause:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   s := mem[mem[c+3]+1];
	ldr x22, =_mem
	ldr w0, [fp, #64]
	add x0, x22, w0, SXTW 2
	ldr w0, [x0, #12]
	add x0, x22, w0, SXTW 2
	ldr w20, [x0, #4]
//   if symtab[s].action <> 0 then
	ldr x22, =_symtab
	add x0, x22, w20, SXTW 4
	ldr w0, [x0, #8]
	cbz w0, .L84
//     newline(); print_string("Error: "); print_string("cannot add clauses to built-in relation "); run := false;
	bl newline
	mov w1, #8
	ldr x0, =g27
	bl print_string
	mov w1, #41
	ldr x0, =g28
	bl print_string
	ldr x0, =_run
	strb wzr, [x0]
//     WriteString(symtab[s].name)
	add x0, x22, w20, SXTW 4
	ldr w0, [x0]
	bl _WriteString
	b .L82
.L84:
//   elsif symtab[s].prok = NULL then
	ldr x0, =_symtab
	add x22, x0, w20, SXTW 4
	ldr w23, =12
	ldr w0, [x22, w23, SXTW]
	cbnz w0, .L87
//     symtab[s].prok := c
	ldr w0, [fp, #64]
	str w0, [x22, w23, SXTW]
	b .L82
.L87:
//     p := symtab[s].prok;
	ldr x0, =_symtab
	add x0, x0, w20, SXTW 4
	ldr w21, [x0, #12]
.L89:
//     while mem[p+2] <> NULL do p := mem[p+2] end;
	ldr x0, =_mem
	add x0, x0, w21, SXTW 2
	ldr w22, [x0, #8]
	cbz w22, .L91
	mov w21, w22
	b .L89
.L91:
//     mem[p+2] := c
	ldr w0, [fp, #64]
	ldr x1, =_mem
	add x1, x1, w21, SXTW 2
	str w0, [x1, #8]
.L82:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc MakeCompound(fun: symbol; var arg: argbuf): term;
_MakeCompound:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   n := symtab[fun].arity;
	ldr x0, =_symtab
	ldr w1, [fp, #64]
	add x0, x0, w1, SXTW 4
	ldr w22, [x0, #4]
//   p := HeapAlloc(TERM_SIZE+n);
	add w0, w22, #2
	bl _HeapAlloc
	mov w20, w0
//   mem[p] := lsl(FUNC, 8) + TERM_SIZE+n;
	ldr x24, =_mem
	add w0, w22, #258
	str w0, [x24, w20, SXTW #2]
//   mem[p+1] := fun;
	ldr w0, [fp, #64]
	add x1, x24, w20, SXTW 2
	str w0, [x1, #4]
//   for i := 1 to n do mem[p+i+1] := arg[i] end;
	mov w21, #1
	mov w23, w22
.L93:
	cmp w21, w23
	bgt .L94
	ldr x0, [fp, #72]
	ldr w0, [x0, w21, SXTW #2]
	ldr x1, =_mem
	add w2, w20, w21
	add x1, x1, w2, SXTW 2
	str w0, [x1, #4]
	add w21, w21, #1
	b .L93
.L94:
//   return p
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc MakeNode(fun: symbol; a1, a2: term): term;
_MakeNode:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	sub sp, sp, #256
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   arg[1] := a1; arg[2] := a2;
	ldr w0, [fp, #280]
	str w0, [fp, #20]
	ldr w0, [fp, #288]
	str w0, [fp, #24]
//   return MakeCompound(fun, arg)
	add x1, fp, #16
	ldr w0, [fp, #272]
	bl _MakeCompound
	ldp fp, lr, [sp], #16
	add sp, sp, #256
	add sp, sp, #32
	ret
	.pool

// proc MakeRef(offset: integer): term;
_MakeRef:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   return refnode[offset]
	ldr x0, =_refnode
	ldr w1, [fp, #16]
	ldr w0, [x0, w1, SXTW #2]
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc MakeInt(i: integer): term;
_MakeInt:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   p := HeapAlloc(TERM_SIZE);
	mov w0, #2
	bl _HeapAlloc
	mov w20, w0
//   mem[p] := lsl(INT, 8) + TERM_SIZE;
	ldr x21, =_mem
	mov w0, #514
	str w0, [x21, w20, SXTW #2]
//   mem[p+1] := i; return p
	ldr w0, [fp, #48]
	add x1, x21, w20, SXTW 2
	str w0, [x1, #4]
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc MakeChar(c: char): term;
_MakeChar:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   p := HeapAlloc(TERM_SIZE);
	mov w0, #2
	bl _HeapAlloc
	mov w20, w0
//   mem[p] := lsl(CHRCTR, 8) + TERM_SIZE;
	ldr x21, =_mem
	mov w0, #770
	str w0, [x21, w20, SXTW #2]
//   mem[p+1] := ord(c); return p
	ldrb w0, [fp, #48]
	add x1, x21, w20, SXTW 2
	str w0, [x1, #4]
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc MakeString(var s: tempstring): term;
_MakeString:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := StringLength(s);
	ldr x0, [fp, #48]
	bl _StringLength
	mov w21, w0
//   p := MakeNode(nilsym, NULL, NULL);
	mov w2, wzr
	mov w1, wzr
	ldr x0, =_nilsym
	ldr w0, [x0]
	bl _MakeNode
	mov w20, w0
.L100:
//   while i > 0 do
	cmp w21, #0
	ble .L102
//     i := i-1; p := MakeNode(cons, MakeChar(s[i]), p)
	sub w21, w21, #1
	ldr x0, [fp, #48]
	ldrb w0, [x0, w21, SXTW]
	bl _MakeChar
	mov w2, w20
	mov x1, x0
	ldr x0, =_cons
	ldr w0, [x0]
	bl _MakeNode
	mov w20, w0
	b .L100
.L102:
//   return p
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc MakeClause(nvars: integer; head: term;
_MakeClause:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   p := HeapAlloc(CLAUSE_SIZE + nbody + 1);
	ldr w0, [fp, #88]
	add w0, w0, #5
	bl _HeapAlloc
	mov w20, w0
//   mem[p] := nvars; mem[p+2] := NULL; mem[p+3] := head;
	ldr x23, =_mem
	ldr w0, [fp, #64]
	str w0, [x23, w20, SXTW #2]
	add x23, x23, w20, SXTW 2
	str wzr, [x23, #8]
	ldr w0, [fp, #72]
	str w0, [x23, #12]
//   for i := 1 to nbody do mem[(p+4)+i-1] := body[i] end;
	mov w21, #1
	ldr w22, [fp, #88]
.L104:
	cmp w21, w22
	bgt .L105
	ldr x0, [fp, #80]
	ldr w0, [x0, w21, SXTW #2]
	ldr x1, =_mem
	add w2, w20, #4
	add w2, w2, w21
	add x1, x1, w2, SXTW 2
	str w0, [x1, #-4]
	add w21, w21, #1
	b .L104
.L105:
//   mem[(p+4)+nbody+1-1] := NULL;
	ldr x23, =_mem
	add w0, w20, #4
	ldr w1, [fp, #88]
	add w0, w0, w1
	str wzr, [x23, w0, SXTW #2]
//   if head = NULL then 
	ldr w0, [fp, #72]
	cbnz w0, .L107
//     mem[p+1] := 0
	add x0, x23, w20, SXTW 2
	str wzr, [x0, #4]
	b .L108
.L107:
//     mem[p+1] := Key(head, NULL)
	mov w1, wzr
	ldr w0, [fp, #72]
	bl _Key
	ldr x1, =_mem
	add x1, x1, w20, SXTW 2
	str w0, [x1, #4]
.L108:
//   return p
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #32
	ret
	.pool

// proc IsString(t: term; e: frame): boolean;
_IsString:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0; t := Deref(t, e);
	mov w20, wzr
	ldr w1, [fp, #56]
	ldr w0, [fp, #48]
	bl _Deref
	str w0, [fp, #48]
.L110:
//   while i < limit do
	cmp w20, #128
	bge .L112
//     if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
	ldr x21, =_mem
	ldr w22, [fp, #48]
	ldr w0, [x21, w22, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #1
	bne .L113
	add x0, x21, w22, SXTW 2
	ldr w0, [x0, #4]
	ldr x1, =_cons
	ldr w1, [x1]
	cmp w0, w1
	beq .L114
.L113:
//       return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
	ldr x21, =_mem
	ldr w22, [fp, #48]
	ldr w0, [x21, w22, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #1
	bne .L120
	add x0, x21, w22, SXTW 2
	ldr w0, [x0, #4]
	ldr x1, =_nilsym
	ldr w1, [x1]
	cmp w0, w1
	cset w21, eq
	b .L121
.L120:
	mov w21, wzr
.L121:
	mov w0, w21
	b .L109
.L114:
//     elsif lsr(mem[Deref(mem[t+1+1], e)], 8) <> CHRCTR then
	ldr x21, =_mem
	ldr w1, [fp, #56]
	ldr w0, [fp, #48]
	add x0, x21, w0, SXTW 2
	ldr w0, [x0, #8]
	bl _Deref
	ldr w0, [x21, w0, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #3
	beq .L117
//       return false
	mov w0, wzr
	b .L109
.L117:
//       i := i+1; t := Deref(mem[t+2+1], e) 
	add w20, w20, #1
	ldr w1, [fp, #56]
	ldr x0, =_mem
	ldr w2, [fp, #48]
	add x0, x0, w2, SXTW 2
	ldr w0, [x0, #12]
	bl _Deref
	str w0, [fp, #48]
	b .L110
.L112:
//   return false
	mov w0, wzr
.L109:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc IsList(t: term; e: frame): boolean;
_IsList:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0; t := Deref(t, e);
	mov w20, wzr
	ldr w1, [fp, #56]
	ldr w0, [fp, #48]
	bl _Deref
	str w0, [fp, #48]
.L124:
//   while i < limit do
	cmp w20, #128
	bge .L126
//     if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
	ldr x21, =_mem
	ldr w22, [fp, #48]
	ldr w0, [x21, w22, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #1
	bne .L127
	add x0, x21, w22, SXTW 2
	ldr w0, [x0, #4]
	ldr x1, =_cons
	ldr w1, [x1]
	cmp w0, w1
	beq .L128
.L127:
//       return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
	ldr x21, =_mem
	ldr w22, [fp, #48]
	ldr w0, [x21, w22, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #1
	bne .L131
	add x0, x21, w22, SXTW 2
	ldr w0, [x0, #4]
	ldr x1, =_nilsym
	ldr w1, [x1]
	cmp w0, w1
	cset w21, eq
	b .L132
.L131:
	mov w21, wzr
.L132:
	mov w0, w21
	b .L123
.L128:
//       i := i+1; t := Deref(mem[t+2+1], e)
	add w20, w20, #1
	ldr w1, [fp, #56]
	ldr x0, =_mem
	ldr w2, [fp, #48]
	add x0, x0, w2, SXTW 2
	ldr w0, [x0, #12]
	bl _Deref
	str w0, [fp, #48]
	b .L124
.L126:
//   return false
	mov w0, wzr
.L123:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc ShowString(t: term; e: frame);
_ShowString:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   t := Deref(t, e);
	ldr w1, [fp, #56]
	ldr w0, [fp, #48]
	bl _Deref
	str w0, [fp, #48]
//   print_char('"');
	mov w0, #34
	bl print_char
.L135:
//   while mem[t+1] <> nilsym do
	ldr x20, =_mem
	ldr w0, [fp, #48]
	add x21, x20, w0, SXTW 2
	ldr w0, [x21, #4]
	ldr x1, =_nilsym
	ldr w1, [x1]
	cmp w0, w1
	beq .L137
//     print_char(chr(mem[Deref(mem[t+1+1], e)+1]));
	ldr w1, [fp, #56]
	ldr w0, [x21, #8]
	bl _Deref
	mov x21, x0
	add x0, x20, w21, SXTW 2
	ldr w0, [x0, #4]
	bl print_char
//     t := Deref(mem[t+2+1], e)
	ldr w1, [fp, #56]
	ldr w0, [fp, #48]
	add x0, x20, w0, SXTW 2
	ldr w0, [x0, #12]
	bl _Deref
	str w0, [fp, #48]
	b .L135
.L137:
//   print_char('"')
	mov w0, #34
	bl print_char
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc PrintCompound(t: term; e: frame; prio: integer);
_PrintCompound:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   f := mem[t+1];
	ldr w23, [fp, #64]
	ldr x0, =_mem
	add x0, x0, w23, SXTW 2
	ldr w20, [x0, #4]
//   if f = cons then
	ldr x0, =_cons
	ldr w0, [x0]
	cmp w20, w0
	bne .L140
//     if IsString(t, e) then
	ldr w1, [fp, #72]
	mov x0, x23
	bl _IsString
	cbz w0, .L164
//       ShowString(t, e)
	ldr w1, [fp, #72]
	ldr w0, [fp, #64]
	bl _ShowString
	b .L138
.L164:
//       if prio < CONSPRIO then print_char('(') end;
	ldr w0, [fp, #80]
	cmp w0, #1
	bge .L168
	mov w0, #40
	bl print_char
.L168:
//       PrintTerm(mem[t+1+1], e, CONSPRIO-1);
	ldr x23, =_mem
	mov w2, wzr
	ldr w1, [fp, #72]
	ldr w0, [fp, #64]
	add x0, x23, w0, SXTW 2
	ldr w0, [x0, #8]
	bl _PrintTerm
//       print_char(':');
	mov w0, #58
	bl print_char
//       PrintTerm(mem[t+2+1], e, CONSPRIO);
	mov w2, #1
	ldr w1, [fp, #72]
	ldr w0, [fp, #64]
	add x0, x23, w0, SXTW 2
	ldr w0, [x0, #12]
	bl _PrintTerm
//       if prio < CONSPRIO then print_char(')') end
	ldr w0, [fp, #80]
	cmp w0, #1
	bge .L138
	mov w0, #41
	bl print_char
	b .L138
.L140:
//   elsif f = eqsym then
	ldr x0, =_eqsym
	ldr w0, [x0]
	cmp w20, w0
	bne .L143
//     if prio < EQPRIO then print_char('(') end;
	ldr w0, [fp, #80]
	cmp w0, #2
	bge .L159
	mov w0, #40
	bl print_char
.L159:
//     PrintTerm(mem[t+1+1], e, EQPRIO-1);
	ldr x23, =_mem
	mov w2, #1
	ldr w1, [fp, #72]
	ldr w0, [fp, #64]
	add x0, x23, w0, SXTW 2
	ldr w0, [x0, #8]
	bl _PrintTerm
//     print_string(" = ");
	mov w1, #4
	ldr x0, =g29
	bl print_string
//     PrintTerm(mem[t+2+1], e, EQPRIO-1);
	mov w2, #1
	ldr w1, [fp, #72]
	ldr w0, [fp, #64]
	add x0, x23, w0, SXTW 2
	ldr w0, [x0, #12]
	bl _PrintTerm
//     if prio < EQPRIO then print_char(')') end
	ldr w0, [fp, #80]
	cmp w0, #2
	bge .L138
	mov w0, #41
	bl print_char
	b .L138
.L143:
//   elsif f = notsym then
	ldr x0, =_notsym
	ldr w0, [x0]
	cmp w20, w0
	bne .L146
//     print_string("not ");
	mov w1, #5
	ldr x0, =g30
	bl print_string
//     PrintTerm(mem[t+1+1], e, MAXPRIO)
	mov w2, #2
	ldr w1, [fp, #72]
	ldr x0, =_mem
	ldr w3, [fp, #64]
	add x0, x0, w3, SXTW 2
	ldr w0, [x0, #8]
	bl _PrintTerm
	b .L138
.L146:
//   elsif (f = node) and IsList(mem[t+2+1], e) then
	ldr x0, =_node
	ldr w0, [x0]
	cmp w20, w0
	bne .L149
	ldr w1, [fp, #72]
	ldr x0, =_mem
	ldr w2, [fp, #64]
	add x0, x0, w2, SXTW 2
	ldr w0, [x0, #12]
	bl _IsList
	cbz w0, .L149
//     PrintNode(t, e)
	ldr w1, [fp, #72]
	ldr w0, [fp, #64]
	bl _PrintNode
	b .L138
.L149:
//     WriteString(symtab[f].name);
	ldr x23, =_symtab
	add x0, x23, w20, SXTW 4
	ldr w0, [x0]
	bl _WriteString
//     if symtab[f].arity > 0 then
	ldr w24, =4
	add x0, x23, w20, SXTW 4
	ldr w0, [x0, w24, SXTW]
	cmp w0, #0
	ble .L138
//       print_char('(');
	mov w0, #40
	bl print_char
//       PrintTerm(mem[t+1+1], e, ARGPRIO);
	mov w2, #2
	ldr w1, [fp, #72]
	ldr x0, =_mem
	ldr w3, [fp, #64]
	add x0, x0, w3, SXTW 2
	ldr w0, [x0, #8]
	bl _PrintTerm
//       for i := 2 to symtab[f].arity do
	mov w21, #2
	add x0, x23, w20, SXTW 4
	ldr w22, [x0, w24, SXTW]
.L154:
	cmp w21, w22
	bgt .L155
//         print_string(", ");
	mov w1, #3
	ldr x0, =g31
	bl print_string
//         PrintTerm(mem[t+i+1], e, ARGPRIO)
	mov w2, #2
	ldr w1, [fp, #72]
	ldr x0, =_mem
	ldr w3, [fp, #64]
	add w3, w3, w21
	add x0, x0, w3, SXTW 2
	ldr w0, [x0, #4]
	bl _PrintTerm
	add w21, w21, #1
	b .L154
.L155:
//       print_char(')')
	mov w0, #41
	bl print_char
.L138:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #32
	ret
	.pool

// proc PrintNode(t: term; e: frame);
_PrintNode:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_char('<');
	mov w0, #60
	bl print_char
//   PrintTerm(mem[t+1+1], e, MAXPRIO);
	ldr x21, =_mem
	mov w2, #2
	ldr w1, [fp, #56]
	ldr w0, [fp, #48]
	add x0, x21, w0, SXTW 2
	ldr w0, [x0, #8]
	bl _PrintTerm
//   u := Deref(mem[t+2+1], e);
	ldr w1, [fp, #56]
	ldr w0, [fp, #48]
	add x0, x21, w0, SXTW 2
	ldr w0, [x0, #12]
	bl _Deref
	mov w20, w0
.L173:
//   while mem[u+1] <> nilsym do
	ldr x21, =_mem
	add x0, x21, w20, SXTW 2
	ldr w0, [x0, #4]
	ldr x1, =_nilsym
	ldr w1, [x1]
	cmp w0, w1
	beq .L175
//     print_string(", ");
	mov w1, #3
	ldr x0, =g32
	bl print_string
//     PrintTerm(mem[u+1+1], e, MAXPRIO);
	mov w2, #2
	ldr w1, [fp, #56]
	add x0, x21, w20, SXTW 2
	ldr w0, [x0, #8]
	bl _PrintTerm
//     u := Deref(mem[u+2+1], e)
	ldr w1, [fp, #56]
	add x0, x21, w20, SXTW 2
	ldr w0, [x0, #12]
	bl _Deref
	mov w20, w0
	b .L173
.L175:
//   print_char('>');
	mov w0, #62
	bl print_char
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc PrintTerm(t: term; e: frame; prio: integer);
_PrintTerm:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   t := Deref(t, e);
	ldr w1, [fp, #24]
	ldr w0, [fp, #16]
	bl _Deref
	str w0, [fp, #16]
//   if t = NULL then
	cbnz w0, .L178
//     print_string("*null-term*")
	mov w1, #12
	ldr x0, =g33
	bl print_string
	b .L176
.L178:
//     case lsr(mem[t], 8) of
	ldr x0, =_mem
	ldr w1, [fp, #16]
	ldr w0, [x0, w1, SXTW #2]
	lsr w0, w0, #8
	sub w0, w0, #1
	cmp w0, #5
	bhs .L180
	adr ip0, .L190
	ldr ip0, [ip0, w0, SXTW #3]
	br ip0
	.p2align 3
.L190:
	.quad .L182
	.quad .L183
	.quad .L184
	.quad .L185
	.quad .L186
.L182:
//         PrintCompound(t, e, prio)
	ldr w2, [fp, #32]
	ldr w1, [fp, #24]
	ldr w0, [fp, #16]
	bl _PrintCompound
	b .L176
.L183:
//         print_num(mem[t+1])
	ldr x0, =_mem
	ldr w1, [fp, #16]
	add x0, x0, w1, SXTW 2
	ldr w0, [x0, #4]
	bl print_num
	b .L176
.L184:
//         print_char(''''); print_char(chr(mem[t+1])); print_char('''')
	mov w0, #39
	bl print_char
	ldr x0, =_mem
	ldr w1, [fp, #16]
	add x0, x0, w1, SXTW 2
	ldr w0, [x0, #4]
	bl print_char
	mov w0, #39
	bl print_char
	b .L176
.L185:
//         if (t >= gsp) then
	ldr w0, [fp, #16]
	ldr x1, =_gsp
	ldr w1, [x1]
	cmp w0, w1
	blt .L188
//           print_char('G'); print_num((MEMSIZE - t) div TERM_SIZE)
	mov w0, #71
	bl print_char
	ldr w0, =25000
	ldr w1, [fp, #16]
	sub w0, w0, w1
	asr w0, w0, #1
	bl print_num
	b .L176
.L188:
//           print_char('L'); print_num((t - hp) div TERM_SIZE)
	mov w0, #76
	bl print_char
	ldr w0, [fp, #16]
	ldr x1, =_hp
	ldr w1, [x1]
	sub w0, w0, w1
	asr w0, w0, #1
	bl print_num
	b .L176
.L186:
//         print_char('@'); print_num(mem[t+1])
	mov w0, #64
	bl print_char
	ldr x0, =_mem
	ldr w1, [fp, #16]
	add x0, x0, w1, SXTW 2
	ldr w0, [x0, #4]
	bl print_num
	b .L176
.L180:
//       print_string("*unknown-term(tag="); 
	mov w1, #19
	ldr x0, =g34
	bl print_string
//       print_num(lsr(mem[t], 8)); print_string(")*")
	ldr x0, =_mem
	ldr w1, [fp, #16]
	ldr w0, [x0, w1, SXTW #2]
	lsr w0, w0, #8
	bl print_num
	mov w1, #3
	ldr x0, =g35
	bl print_string
.L176:
	ldp fp, lr, [sp], #16
	add sp, sp, #32
	ret
	.pool

// proc PrintClause(c: clause);
_PrintClause:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if c = NULL then
	ldr w0, [fp, #48]
	cbnz w0, .L193
//     print_string("*null-clause*"); newline();
	mov w1, #14
	ldr x0, =g36
	bl print_string
	bl newline
	b .L191
.L193:
//     if mem[c+3] <> NULL then
	ldr x0, =_mem
	ldr w1, [fp, #48]
	add x0, x0, w1, SXTW 2
	ldr w21, [x0, #12]
	cbz w21, .L197
//       PrintTerm(mem[c+3], NULL, MAXPRIO);
	mov w2, #2
	mov w1, wzr
	mov x0, x21
	bl _PrintTerm
//       print_char(' ')
	mov w0, #32
	bl print_char
.L197:
//     print_string(":- ");
	mov w1, #4
	ldr x0, =g37
	bl print_string
//     if mem[(c+4)+1-1] <> NULL then
	ldr x0, =_mem
	ldr w1, [fp, #48]
	add x0, x0, w1, SXTW 2
	ldr w21, [x0, #16]
	cbz w21, .L200
//       PrintTerm(mem[(c+4)+1-1], NULL, MAXPRIO);
	mov w2, #2
	mov w1, wzr
	mov x0, x21
	bl _PrintTerm
//       i := 2;
	mov w20, #2
.L201:
//       while mem[(c+4)+i-1] <> NULL do
	ldr x21, =_mem
	ldr w0, [fp, #48]
	add w0, w0, #4
	add w0, w0, w20
	add x0, x21, w0, SXTW 2
	ldr w0, [x0, #-4]
	cbz w0, .L200
// 	print_string(", ");
	mov w1, #3
	ldr x0, =g38
	bl print_string
// 	PrintTerm(mem[(c+4)+i-1], NULL, MAXPRIO);
	mov w2, #2
	mov w1, wzr
	ldr w0, [fp, #48]
	add w0, w0, #4
	add w0, w0, w20
	add x0, x21, w0, SXTW 2
	ldr w0, [x0, #-4]
	bl _PrintTerm
// 	i := i+1
	add w20, w20, #1
	b .L201
.L200:
//     print_char('.'); newline()
	mov w0, #46
	bl print_char
	bl newline
.L191:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc ShowError();
_ShowError:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   errflag := true; errcount := errcount+1;
	mov w0, #1
	ldr x1, =_errflag
	strb w0, [x1]
	ldr x20, =_errcount
	ldr w0, [x20]
	add w0, w0, #1
	str w0, [x20]
//   print_string("Line "); print_num(lineno); print_char(' ');
	mov w1, #6
	ldr x0, =g39
	bl print_string
	ldr x0, =_lineno
	ldr w0, [x0]
	bl print_num
	mov w0, #32
	bl print_char
//   print_string("Syntax error - ")
	mov w1, #16
	ldr x0, =g40
	bl print_string
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

// proc Recover();
_Recover:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if errcount >= 20 then
	ldr x0, =_errcount
	ldr w0, [x0]
	cmp w0, #20
	blt .L208
//     print_string("Too many errors: I am giving up"); newline(); exit(2) 
	mov w1, #32
	ldr x0, =g41
	bl print_string
	bl newline
	mov w0, #2
	bl exit
.L208:
//   if token <> DOT then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #10
	beq .L205
.L212:
//       ch := GetChar()
	bl _GetChar
	mov w20, w0
	cmp w20, #46
	beq .L213
	cmp w20, #127
	bne .L212
.L213:
//     token := DOT
	mov w0, #10
	ldr x1, =_token
	str w0, [x1]
.L205:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

// proc Scan();
_Scan:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   ch := GetChar(); token := 0;
	bl _GetChar
	mov w20, w0
	ldr x0, =_token
	str wzr, [x0]
.L216:
//   while token = 0 do
	ldr x23, =_token
	ldr w0, [x23]
	cbnz w0, .L215
//     if ch = ENDFILE then
	cmp w20, #127
	bne .L220
//       token := EOFTOK
	mov w0, #14
	str w0, [x23]
	b .L216
.L220:
//     elsif (ch = ' ') or (ch = TAB) or (ch = ENDLINE) then
	cmp w20, #32
	beq .L222
	cmp w20, #9
	beq .L222
	cmp w20, #10
	bne .L223
.L222:
//       ch := GetChar()
	bl _GetChar
	mov w20, w0
	b .L216
.L223:
//     elsif ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) then
	cmp w20, #65
	blt .L312
	cmp w20, #90
	ble .L225
.L312:
	cmp w20, #95
	beq .L225
	cmp w20, #97
	blt .L226
	cmp w20, #122
	bgt .L226
.L225:
//       if (((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) then 
	cmp w20, #65
	blt .L293
	cmp w20, #90
	ble .L290
.L293:
	cmp w20, #95
	bne .L291
.L290:
// 	 token := VARIABLE
	mov w0, #2
	ldr x1, =_token
	str w0, [x1]
	b .L292
.L291:
// 	 token := IDENT
	mov w0, #1
	ldr x1, =_token
	str w0, [x1]
.L292:
//       i := 0;
	mov w22, wzr
.L295:
//       while ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) or ((ch >= '0') and (ch <= '9')) do
	cmp w20, #65
	blt .L305
	cmp w20, #90
	ble .L296
.L305:
	cmp w20, #95
	beq .L296
	cmp w20, #97
	blt .L301
	cmp w20, #122
	ble .L296
.L301:
	cmp w20, #48
	blt .L297
	cmp w20, #57
	bgt .L297
.L296:
//         if i > MAXSTRING then
	cmp w22, #128
	ble .L300
//           newline(); print_string("Panic: "); print_string("identifier too long"); newline(); exit(2)
	bl newline
	mov w1, #8
	ldr x0, =g42
	bl print_string
	mov w1, #20
	ldr x0, =g43
	bl print_string
	bl newline
	mov w0, #2
	bl exit
.L300:
//         toksval[i] := ch; ch := GetChar(); i := i+1
	ldr x0, =_toksval
	strb w20, [x0, w22, SXTW]
	bl _GetChar
	mov w20, w0
	add w22, w22, #1
	b .L295
.L297:
//       PushBack(ch);
	mov w0, w20
	bl _PushBack
//       toksval[i] := ENDSTR; tokval := Lookup(toksval);
	ldr x23, =_toksval
	strb wzr, [x23, w22, SXTW]
	mov x0, x23
	bl _Lookup
	ldr x1, =_tokval
	str w0, [x1]
//       if tokval = notsym then token := NEGATE end
	ldr x1, =_notsym
	ldr w1, [x1]
	cmp w0, w1
	bne .L216
	mov w0, #13
	ldr x1, =_token
	str w0, [x1]
	b .L216
.L226:
//     elsif ((ch >= '0') and (ch <= '9')) then
	cmp w20, #48
	blt .L229
	cmp w20, #57
	bgt .L229
//       token := NUMBER; tokival := 0;
	mov w0, #3
	ldr x1, =_token
	str w0, [x1]
	ldr x0, =_tokival
	str wzr, [x0]
.L285:
//       while ((ch >= '0') and (ch <= '9')) do
	cmp w20, #48
	blt .L287
	cmp w20, #57
	bgt .L287
//         tokival := 10 * tokival + (ord(ch) - ord('0'));
	ldr x23, =_tokival
	ldr w0, [x23]
	mov w1, #10
	mul w0, w0, w1
	sub w1, w20, #48
	add w0, w0, w1
	str w0, [x23]
//         ch := GetChar()
	bl _GetChar
	mov w20, w0
	b .L285
.L287:
//       PushBack(ch)
	mov w0, w20
	bl _PushBack
	b .L216
.L229:
//       case ch of
	sub w0, w20, #33
	cmp w0, #30
	bhs .L231
	adr ip0, .L316
	ldr ip0, [ip0, w0, SXTW #3]
	br ip0
	.p2align 3
.L316:
	.quad .L241
	.quad .L245
	.quad .L240
	.quad .L231
	.quad .L231
	.quad .L231
	.quad .L244
	.quad .L233
	.quad .L234
	.quad .L231
	.quad .L231
	.quad .L235
	.quad .L231
	.quad .L236
	.quad .L242
	.quad .L231
	.quad .L231
	.quad .L231
	.quad .L231
	.quad .L231
	.quad .L231
	.quad .L231
	.quad .L231
	.quad .L231
	.quad .L231
	.quad .L243
	.quad .L231
	.quad .L238
	.quad .L237
	.quad .L239
.L233:
//         '(': token := LPAR
	mov w0, #7
	ldr x1, =_token
	str w0, [x1]
	b .L216
.L234:
//       | ')': token := RPAR
	mov w0, #8
	ldr x1, =_token
	str w0, [x1]
	b .L216
.L235:
//       | ',': token := COMMA
	mov w0, #9
	ldr x1, =_token
	str w0, [x1]
	b .L216
.L236:
//       | '.': token := DOT
	mov w0, #10
	ldr x1, =_token
	str w0, [x1]
	b .L216
.L237:
//       | '=': token := EQUAL
	mov w0, #12
	ldr x1, =_token
	str w0, [x1]
	b .L216
.L238:
//       | '<': token := LANGLE
	mov w0, #15
	ldr x1, =_token
	str w0, [x1]
	b .L216
.L239:
//       | '>': token := RANGLE
	mov w0, #16
	ldr x1, =_token
	str w0, [x1]
	b .L216
.L240:
//       | '#': token := HASH
	mov w0, #17
	ldr x1, =_token
	str w0, [x1]
	b .L216
.L241:
//       | '!': token := IDENT; tokval := cutsym
	mov w0, #1
	ldr x1, =_token
	str w0, [x1]
	ldr x0, =_cutsym
	ldr w0, [x0]
	ldr x1, =_tokval
	str w0, [x1]
	b .L216
.L242:
// 	  ch := GetChar();
	bl _GetChar
	mov w20, w0
// 	  if ch <> '*' then
	cmp w20, #42
	beq .L250
// 	    if not errflag then ShowError(); print_string("bad token /"); newline(); Recover() end
	ldr x0, =_errflag
	ldrb w0, [x0]
	cbnz w0, .L216
	bl _ShowError
	mov w1, #12
	ldr x0, =g44
	bl print_string
	bl newline
	bl _Recover
	b .L216
.L250:
// 	    ch2 := ' '; ch := GetChar();
	mov w21, #32
	bl _GetChar
	mov w20, w0
.L252:
// 	    while (ch <> ENDFILE) and not ((ch2 = '*') and (ch = '/')) do
	cmp w20, #127
	beq .L254
	cmp w21, #42
	bne .L253
	cmp w20, #47
	beq .L254
.L253:
// 	      ch2 := ch; ch := GetChar() 
	mov w21, w20
	bl _GetChar
	mov w20, w0
	b .L252
.L254:
// 	    if ch = ENDFILE then
	cmp w20, #127
	bne .L258
// 	      if not errflag then ShowError(); print_string("end of file in comment"); newline(); Recover() end
	ldr x0, =_errflag
	ldrb w0, [x0]
	cbnz w0, .L216
	bl _ShowError
	mov w1, #23
	ldr x0, =g45
	bl print_string
	bl newline
	bl _Recover
	b .L216
.L258:
// 	      ch := GetChar()
	bl _GetChar
	mov w20, w0
	b .L216
.L243:
// 	  ch := GetChar();
	bl _GetChar
	mov w20, w0
// 	  if ch = '-' then
	cmp w20, #45
	bne .L267
// 	    token := ARROW
	mov w0, #6
	ldr x1, =_token
	str w0, [x1]
	b .L216
.L267:
// 	    PushBack(ch); token := COLON 
	mov w0, w20
	bl _PushBack
	mov w0, #11
	ldr x1, =_token
	str w0, [x1]
	b .L216
.L244:
// 	  token := CHCON; tokival := ord(GetChar()); ch := GetChar();
	mov w0, #4
	ldr x1, =_token
	str w0, [x1]
	bl _GetChar
	ldr x1, =_tokival
	str w0, [x1]
	bl _GetChar
	mov w20, w0
// 	  if ch <> '''' then if not errflag then ShowError(); print_string("missing quote"); newline(); Recover() end end
	cmp w20, #39
	beq .L216
	ldr x0, =_errflag
	ldrb w0, [x0]
	cbnz w0, .L216
	bl _ShowError
	mov w1, #14
	ldr x0, =g46
	bl print_string
	bl newline
	bl _Recover
	b .L216
.L245:
// 	  token := STRCON; i := 0; ch := GetChar();
	mov w0, #5
	ldr x1, =_token
	str w0, [x1]
	mov w22, wzr
	bl _GetChar
	mov w20, w0
.L275:
// 	  while (ch <> '"') and (ch <> ENDLINE) do
	cmp w20, #34
	beq .L277
	cmp w20, #10
	beq .L277
// 	    toksval[i] := ch; ch := GetChar(); i := i+1 
	ldr x0, =_toksval
	strb w20, [x0, w22, SXTW]
	bl _GetChar
	mov w20, w0
	add w22, w22, #1
	b .L275
.L277:
// 	  toksval[i] := ENDSTR;
	ldr x0, =_toksval
	strb wzr, [x0, w22, SXTW]
// 	  if ch = ENDLINE then
	cmp w20, #10
	bne .L216
// 	    if not errflag then ShowError(); print_string("unterminated string"); newline(); Recover() end;
	ldr x0, =_errflag
	ldrb w0, [x0]
	cbnz w0, .L284
	bl _ShowError
	mov w1, #20
	ldr x0, =g47
	bl print_string
	bl newline
	bl _Recover
.L284:
// 	    PushBack(ch)
	mov w0, w20
	bl _PushBack
	b .L216
.L231:
// 	if not errflag then ShowError(); print_string("illegal character"); newline(); Recover() end; print_char(ch); newline()
	ldr x0, =_errflag
	ldrb w0, [x0]
	cbnz w0, .L248
	bl _ShowError
	mov w1, #18
	ldr x0, =g48
	bl print_string
	bl newline
	bl _Recover
.L248:
	mov w0, w20
	bl print_char
	bl newline
	b .L216
.L215:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

// proc PrintToken(t: integer);
_PrintToken:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   case t of
	ldr w0, [fp, #16]
	sub w0, w0, #1
	cmp w0, #17
	bhs .L318
	adr ip0, .L335
	ldr ip0, [ip0, w0, SXTW #3]
	br ip0
	.p2align 3
.L335:
	.quad .L320
	.quad .L321
	.quad .L322
	.quad .L323
	.quad .L331
	.quad .L324
	.quad .L325
	.quad .L326
	.quad .L327
	.quad .L328
	.quad .L329
	.quad .L330
	.quad .L318
	.quad .L318
	.quad .L332
	.quad .L333
	.quad .L334
.L320:
//       print_string("identifier "); WriteString(symtab[tokval].name)
	mov w1, #12
	ldr x0, =g49
	bl print_string
	ldr x0, =_symtab
	ldr x1, =_tokval
	ldr w1, [x1]
	add x0, x0, w1, SXTW 4
	ldr w0, [x0]
	bl _WriteString
	b .L317
.L321:
//       print_string("variable "); WriteString(symtab[tokval].name)
	mov w1, #10
	ldr x0, =g50
	bl print_string
	ldr x0, =_symtab
	ldr x1, =_tokval
	ldr w1, [x1]
	add x0, x0, w1, SXTW 4
	ldr w0, [x0]
	bl _WriteString
	b .L317
.L322:
//   | NUMBER: print_string("number");
	mov w1, #7
	ldr x0, =g51
	bl print_string
	b .L317
.L323:
//   | CHCON:  print_string("char constant");
	mov w1, #14
	ldr x0, =g52
	bl print_string
	b .L317
.L324:
//   | ARROW:  print_string(":-");
	mov w1, #3
	ldr x0, =g53
	bl print_string
	b .L317
.L325:
//   | LPAR:   print_string("(");
	mov w1, #2
	ldr x0, =g54
	bl print_string
	b .L317
.L326:
//   | RPAR:   print_string(")");
	mov w1, #2
	ldr x0, =g55
	bl print_string
	b .L317
.L327:
//   | COMMA:  print_string(",");
	mov w1, #2
	ldr x0, =g56
	bl print_string
	b .L317
.L328:
//   | DOT:    print_string(".");
	mov w1, #2
	ldr x0, =g57
	bl print_string
	b .L317
.L329:
//   | COLON:  print_string(":");
	mov w1, #2
	ldr x0, =g58
	bl print_string
	b .L317
.L330:
//   | EQUAL:  print_string("=");
	mov w1, #2
	ldr x0, =g59
	bl print_string
	b .L317
.L331:
//   | STRCON: print_string("string constant")
	mov w1, #16
	ldr x0, =g60
	bl print_string
	b .L317
.L332:
//   | LANGLE: print_string("<")
	mov w1, #2
	ldr x0, =g61
	bl print_string
	b .L317
.L333:
//   | RANGLE: print_string(">")
	mov w1, #2
	ldr x0, =g62
	bl print_string
	b .L317
.L334:
//   | HASH:   print_string("#")
	mov w1, #2
	ldr x0, =g63
	bl print_string
	b .L317
.L318:
//     print_string("unknown token")
	mov w1, #14
	ldr x0, =g64
	bl print_string
.L317:
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc VarRep(name: symbol): term;
_VarRep:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if nvars = MAXARITY then newline(); print_string("Panic: "); print_string("too many variables"); newline(); exit(2) end;
	ldr x0, =_nvars
	ldr w0, [x0]
	cmp w0, #63
	bne .L339
	bl newline
	mov w1, #8
	ldr x0, =g65
	bl print_string
	mov w1, #19
	ldr x0, =g66
	bl print_string
	bl newline
	mov w0, #2
	bl exit
.L339:
//   i := 1; vartable[nvars+1] := name;  (* sentinel *)
	mov w20, #1
	ldr w0, [fp, #48]
	ldr x1, =_vartable
	ldr x2, =_nvars
	ldr w2, [x2]
	add x1, x1, w2, SXTW 2
	str w0, [x1, #4]
.L340:
//   while name <> vartable[i] do i := i+1 end;
	ldr w0, [fp, #48]
	ldr x1, =_vartable
	ldr w1, [x1, w20, SXTW #2]
	cmp w0, w1
	beq .L342
	add w20, w20, #1
	b .L340
.L342:
//   if i = nvars+1 then nvars := nvars+1 end;
	ldr x21, =_nvars
	ldr w0, [x21]
	add w22, w0, #1
	cmp w20, w22
	bne .L345
	str w22, [x21]
.L345:
//   return MakeRef(i)
	mov w0, w20
	bl _MakeRef
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc ShowAnswer(bindings: frame);
_ShowAnswer:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if nvars = 0 then
	ldr x0, =_nvars
	ldr w0, [x0]
	cbnz w0, .L348
//     print_string("yes"); newline()
	mov w1, #4
	ldr x0, =g67
	bl print_string
	bl newline
	b .L346
.L348:
//     for i := 1 to nvars do
	mov w20, #1
	ldr x0, =_nvars
	ldr w23, [x0]
.L350:
	cmp w20, w23
	bgt .L346
//       WriteString(symtab[vartable[i]].name); print_string(" = ");
	ldr x0, =_symtab
	ldr x1, =_vartable
	ldr w1, [x1, w20, SXTW #2]
	add x0, x0, w1, SXTW 4
	ldr w0, [x0]
	bl _WriteString
	mov w1, #4
	ldr x0, =g68
	bl print_string
//       PrintTerm((bindings+7+(i-1)*TERM_SIZE), NULL, EQPRIO-1);
	mov w2, #1
	mov w1, wzr
	ldr w0, [fp, #64]
	add w0, w0, #7
	lsl w3, w20, #1
	sub w3, w3, #2
	add w0, w0, w3
	bl _PrintTerm
//       newline()
	bl newline
	add w20, w20, #1
	b .L350
.L346:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc Eat(expected: integer);
_Eat:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if token = expected then
	ldr x0, =_token
	ldr w20, [x0]
	ldr w0, [fp, #32]
	cmp w20, w0
	bne .L354
//     if token <> DOT then Scan() end
	cmp w20, #10
	beq .L352
	bl _Scan
	b .L352
.L354:
//   elsif not errflag then
	ldr x0, =_errflag
	ldrb w0, [x0]
	cbnz w0, .L352
//     ShowError();
	bl _ShowError
//     print_string("expected "); PrintToken(expected);
	mov w1, #10
	ldr x0, =g69
	bl print_string
	ldr w0, [fp, #32]
	bl _PrintToken
//     print_string(", found "); PrintToken(token); newline();
	mov w1, #9
	ldr x0, =g70
	bl print_string
	ldr x0, =_token
	ldr w0, [x0]
	bl _PrintToken
	bl newline
//     Recover()
	bl _Recover
.L352:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc ParseCompound(): term;
_ParseCompound:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #256
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   fun := tokval; n := 0; Eat(IDENT);
	ldr x0, =_tokval
	ldr w20, [x0]
	mov w21, wzr
	mov w0, #1
	bl _Eat
//   if token = LPAR then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #7
	bne .L365
//     Eat(LPAR); n := 1; arg[1] := ParseTerm();
	mov w0, #7
	bl _Eat
	mov w21, #1
	bl _ParseTerm
	str w0, [fp, #20]
.L366:
//     while token = COMMA do
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #9
	bne .L368
//       Eat(COMMA); n := n+1; arg[n] := ParseTerm()
	mov w0, #9
	bl _Eat
	add w21, w21, #1
	bl _ParseTerm
	add x1, fp, #16
	str w0, [x1, w21, SXTW #2]
	b .L366
.L368:
//     Eat(RPAR)
	mov w0, #8
	bl _Eat
.L365:
//   if symtab[fun].arity = -1 then
	ldr x0, =_symtab
	add x22, x0, w20, SXTW 4
	ldr w23, =4
	ldr w0, [x22, w23, SXTW]
	cmp w0, #-1
	bne .L370
//     symtab[fun].arity := n
	str w21, [x22, w23, SXTW]
	b .L371
.L370:
//   elsif symtab[fun].arity <> n then
	ldr x0, =_symtab
	add x0, x0, w20, SXTW 4
	ldr w0, [x0, #4]
	cmp w0, w21
	beq .L371
//     if not errflag then ShowError(); print_string("wrong number of args"); newline(); Recover() end
	ldr x0, =_errflag
	ldrb w0, [x0]
	cbnz w0, .L371
	bl _ShowError
	mov w1, #21
	ldr x0, =g71
	bl print_string
	bl newline
	bl _Recover
.L371:
//   return MakeCompound(fun, arg)
	add x1, fp, #16
	mov w0, w20
	bl _MakeCompound
	ldp fp, lr, [sp], #16
	add sp, sp, #256
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

// proc ParsePrimary(): term;
_ParsePrimary:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if token = IDENT then t := ParseCompound()
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #1
	bne .L380
	bl _ParseCompound
	mov w20, w0
	b .L381
.L380:
//   elsif token = VARIABLE then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #2
	bne .L383
//     t := VarRep(tokval); Eat(VARIABLE)
	ldr x0, =_tokval
	ldr w0, [x0]
	bl _VarRep
	mov w20, w0
	mov w0, #2
	bl _Eat
	b .L381
.L383:
//   elsif token = NUMBER then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #3
	bne .L386
//     t := MakeInt(tokival); Eat(NUMBER)
	ldr x0, =_tokival
	ldr w0, [x0]
	bl _MakeInt
	mov w20, w0
	mov w0, #3
	bl _Eat
	b .L381
.L386:
//   elsif token = CHCON then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #4
	bne .L389
//     t := MakeChar(chr(tokival)); Eat(CHCON)
	ldr x0, =_tokival
	ldr w0, [x0]
	bl _MakeChar
	mov w20, w0
	mov w0, #4
	bl _Eat
	b .L381
.L389:
//   elsif token = STRCON then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #5
	bne .L392
//     t := MakeString(toksval); Eat(STRCON)
	ldr x0, =_toksval
	bl _MakeString
	mov w20, w0
	mov w0, #5
	bl _Eat
	b .L381
.L392:
//   elsif token = LPAR then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #7
	bne .L395
//     Eat(LPAR); t := ParseTerm(); Eat(RPAR)
	mov w0, #7
	bl _Eat
	bl _ParseTerm
	mov w20, w0
	mov w0, #8
	bl _Eat
	b .L381
.L395:
//   elsif token = LANGLE then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #15
	bne .L398
//     t := ParseNode()
	bl _ParseNode
	mov w20, w0
	b .L381
.L398:
//     if not errflag then ShowError(); print_string("expected a term"); newline(); Recover() end; t := NULL
	ldr x0, =_errflag
	ldrb w0, [x0]
	cbnz w0, .L402
	bl _ShowError
	mov w1, #16
	ldr x0, =g72
	bl print_string
	bl newline
	bl _Recover
.L402:
	mov w20, wzr
.L381:
//   return t
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

// proc ParseNode(): term;
_ParseNode:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   Eat(LANGLE);
	mov w0, #15
	bl _Eat
//   tag := ParseTerm();
	bl _ParseTerm
	mov w20, w0
//   kids := ParseKids();
	bl _ParseKids
	mov w21, w0
//   Eat(RANGLE);
	mov w0, #16
	bl _Eat
//   return MakeNode(node, tag, kids)
	mov w2, w21
	mov w1, w20
	ldr x0, =_node
	ldr w0, [x0]
	bl _MakeNode
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// proc ParseKids(): term;
_ParseKids:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if token <> COMMA then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #9
	beq .L406
//     return MakeNode(nilsym, NULL, NULL)
	mov w2, wzr
	mov w1, wzr
	ldr x0, =_nilsym
	ldr w0, [x0]
	bl _MakeNode
	b .L404
.L406:
//     Eat(COMMA);
	mov w0, #9
	bl _Eat
//     head := ParseTerm();
	bl _ParseTerm
	mov w20, w0
//     tail := ParseKids();
	bl _ParseKids
	mov w21, w0
//     return MakeNode(cons, head, tail)
	mov w2, w21
	mov w1, w20
	ldr x0, =_cons
	ldr w0, [x0]
	bl _MakeNode
.L404:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// proc ParseFactor(): term;
_ParseFactor:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   t := ParsePrimary();
	bl _ParsePrimary
	mov w20, w0
//   if token <> COLON then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #11
	beq .L410
//     return t
	mov w0, w20
	b .L408
.L410:
//     Eat(COLON);
	mov w0, #11
	bl _Eat
//     return MakeNode(cons, t, ParseFactor())
	bl _ParseFactor
	mov x2, x0
	mov w1, w20
	ldr x0, =_cons
	ldr w0, [x0]
	bl _MakeNode
.L408:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

// proc ParseTerm(): term;
_ParseTerm:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   t := ParseFactor();
	bl _ParseFactor
	mov w20, w0
//   if token <> EQUAL then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #12
	beq .L414
//     return t
	mov w0, w20
	b .L412
.L414:
//     Eat(EQUAL);
	mov w0, #12
	bl _Eat
//     return MakeNode(eqsym, t, ParseFactor())
	bl _ParseFactor
	mov x2, x0
	mov w1, w20
	ldr x0, =_eqsym
	ldr w0, [x0]
	bl _MakeNode
.L412:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

// proc CheckAtom(a: term);
_CheckAtom:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if lsr(mem[a], 8) <> FUNC then
	ldr x0, =_mem
	ldr w1, [fp, #16]
	ldr w0, [x0, w1, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #1
	beq .L416
//     if not errflag then ShowError(); print_string("literal must be a compound term"); newline(); Recover() end
	ldr x0, =_errflag
	ldrb w0, [x0]
	cbnz w0, .L416
	bl _ShowError
	mov w1, #32
	ldr x0, =g73
	bl print_string
	bl newline
	bl _Recover
.L416:
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc ParseClause(): clause;
_ParseClause:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #272
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if token = HASH then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #17
	bne .L425
//     Eat(HASH); head := NULL
	mov w0, #17
	bl _Eat
	mov w20, wzr
	b .L426
.L425:
//     head := ParseTerm();
	bl _ParseTerm
	mov w20, w0
//     CheckAtom(head)
	mov w0, w20
	bl _CheckAtom
.L426:
//   Eat(ARROW);
	mov w0, #6
	bl _Eat
//   n := 0;
	mov w22, wzr
//   if token <> DOT then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #10
	beq .L429
//     more := true;
	mov w0, #1
	strb w0, [fp, #31]
.L430:
//     while more do
	ldrb w0, [fp, #31]
	cbz w0, .L429
//       n := n+1; minus := false;
	add w22, w22, #1
	mov w23, wzr
//       if token = NEGATE then
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #13
	bne .L435
// 	Eat(NEGATE); minus := true 
	mov w0, #13
	bl _Eat
	mov w23, #1
.L435:
//       t := ParseTerm(); CheckAtom(t);
	bl _ParseTerm
	mov w21, w0
	mov w0, w21
	bl _CheckAtom
//       if minus then 
	cbz w23, .L437
// 	body[n] := MakeNode(notsym, t, NULL)
	mov w2, wzr
	mov w1, w21
	ldr x0, =_notsym
	ldr w0, [x0]
	bl _MakeNode
	add x1, fp, #32
	str w0, [x1, w22, SXTW #2]
	b .L438
.L437:
//         body[n] := t
	add x0, fp, #32
	str w21, [x0, w22, SXTW #2]
.L438:
//       if token = COMMA then Eat(COMMA) else more := false end
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #9
	bne .L440
	mov w0, #9
	bl _Eat
	b .L430
.L440:
	strb wzr, [fp, #31]
	b .L430
.L429:
//   Eat(DOT);
	mov w0, #10
	bl _Eat
//   if errflag then 
	ldr x0, =_errflag
	ldrb w0, [x0]
	cbz w0, .L443
//     return NULL
	mov w0, wzr
	b .L423
.L443:
//     return MakeClause(nvars, head, body, n)
	mov w3, w22
	add x2, fp, #32
	mov w1, w20
	ldr x0, =_nvars
	ldr w0, [x0]
	bl _MakeClause
.L423:
	ldp fp, lr, [sp], #16
	add sp, sp, #272
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

// proc ReadClause(): clause;
_ReadClause:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
.L446:
//     hp := hmark; nvars := 0; errflag := false;
	ldr x0, =_hmark
	ldr w0, [x0]
	ldr x1, =_hp
	str w0, [x1]
	ldr x0, =_nvars
	str wzr, [x0]
	ldr x0, =_errflag
	strb wzr, [x0]
//     Scan();
	bl _Scan
//     if token = EOFTOK then 
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #14
	bne .L450
//       c := NULL
	mov w20, wzr
	b .L451
.L450:
//       c := ParseClause()
	bl _ParseClause
	mov w20, w0
.L451:
	ldr x0, =_errflag
	ldrb w0, [x0]
	cbz w0, .L447
	ldr x0, =_token
	ldr w0, [x0]
	cmp w0, #14
	bne .L446
.L447:
//   return c
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

// proc Save(v: term);
_Save:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if ((v < choice) or (v >= mem[choice+4])) then
	ldr w21, [fp, #48]
	ldr x0, =_choice
	ldr w22, [x0]
	cmp w21, w22
	blt .L453
	ldr x0, =_mem
	add x0, x0, w22, SXTW 2
	ldr w0, [x0, #16]
	cmp w21, w0
	blt .L452
.L453:
//     p := GloAlloc(UNDO, TRAIL_SIZE);
	mov w1, #3
	mov w0, #6
	bl _GloAlloc
	mov w20, w0
//     mem[p+1] := v; mem[p+2] := trhead; trhead := p
	ldr x0, =_mem
	add x21, x0, w20, SXTW 2
	ldr w0, [fp, #48]
	str w0, [x21, #4]
	ldr x22, =_trhead
	ldr w0, [x22]
	str w0, [x21, #8]
	str w20, [x22]
.L452:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc Restore();
_Restore:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
.L458:
//   while (trhead <> mem[choice+5]) do
	ldr x0, =_trhead
	ldr w21, [x0]
	ldr x22, =_mem
	ldr x0, =_choice
	ldr w0, [x0]
	add x0, x22, w0, SXTW 2
	ldr w0, [x0, #20]
	cmp w21, w0
	beq .L457
//     v := mem[trhead+1];
	add x0, x22, w21, SXTW 2
	ldr w20, [x0, #4]
//     if v <> NULL then mem[v+1] := NULL end;
	cbz w20, .L463
	add x0, x22, w20, SXTW 2
	str wzr, [x0, #4]
.L463:
//     trhead := mem[trhead+2]
	ldr x21, =_trhead
	ldr x0, =_mem
	ldr w1, [x21]
	add x0, x0, w1, SXTW 2
	ldr w0, [x0, #8]
	str w0, [x21]
	b .L458
.L457:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// proc Commit();
_Commit:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   p := trhead;
	ldr x0, =_trhead
	ldr w20, [x0]
.L465:
//   while (p <> NULL) and (p < mem[choice+4]) do
	cbz w20, .L464
	ldr x21, =_mem
	ldr x0, =_choice
	ldr w22, [x0]
	add x0, x21, w22, SXTW 2
	ldr w23, [x0, #16]
	cmp w20, w23
	bge .L464
//     if (mem[p+1] <> NULL) and not ((mem[p+1] < choice) or (mem[p+1] >= mem[choice+4])) then
	add x21, x21, w20, SXTW 2
	ldr w24, [x21, #4]
	cbz w24, .L470
	cmp w24, w22
	blt .L470
	cmp w24, w23
	bge .L470
//       mem[p+1] := NULL
	str wzr, [x21, #4]
.L470:
//     p := mem[p+2]
	ldr x0, =_mem
	add x0, x0, w20, SXTW 2
	ldr w20, [x0, #8]
	b .L465
.L464:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

// proc GloCopy(t: term; e: frame): term;
_GloCopy:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   t := Deref(t, e);
	ldr w1, [fp, #72]
	ldr w0, [fp, #64]
	bl _Deref
	str w0, [fp, #64]
//   if (t >= gsp) then
	ldr x1, =_gsp
	ldr w1, [x1]
	cmp w0, w1
	blt .L476
//     return t
	b .L474
.L476:
//     case lsr(mem[t], 8) of
	ldr x0, =_mem
	ldr w1, [fp, #64]
	ldr w0, [x0, w1, SXTW #2]
	lsr w0, w0, #8
	sub w0, w0, #1
	cmp w0, #4
	bhs .L478
	adr ip0, .L488
	ldr ip0, [ip0, w0, SXTW #3]
	br ip0
	.p2align 3
.L488:
	.quad .L480
	.quad .L478
	.quad .L478
	.quad .L481
.L480:
// 	n := symtab[mem[t+1]].arity;
	ldr w24, [fp, #64]
	ldr x0, =_symtab
	ldr x1, =_mem
	add x1, x1, w24, SXTW 2
	ldr w1, [x1, #4]
	add x0, x0, w1, SXTW 4
	ldr w22, [x0, #4]
// 	if (t <= hp) and (n = 0) then 
	ldr x0, =_hp
	ldr w0, [x0]
	cmp w24, w0
	bgt .L483
	cbnz w22, .L483
// 	  return t
	mov w0, w24
	b .L474
.L483:
// 	  tt := GloAlloc(FUNC, TERM_SIZE+n);
	add w1, w22, #2
	mov w0, #1
	bl _GloAlloc
	mov w20, w0
// 	  mem[tt+1] := mem[t+1];
	ldr x24, =_mem
	ldr w0, [fp, #64]
	add x0, x24, w0, SXTW 2
	ldr w0, [x0, #4]
	add x1, x24, w20, SXTW 2
	str w0, [x1, #4]
// 	  for i := 1 to n do
	mov w21, #1
	mov w23, w22
.L485:
	cmp w21, w23
	bgt .L486
// 	    mem[tt+i+1] := GloCopy(mem[t+i+1], e)
	ldr x24, =_mem
	ldr w1, [fp, #72]
	ldr w0, [fp, #64]
	add w0, w0, w21
	add x0, x24, w0, SXTW 2
	ldr w0, [x0, #4]
	bl _GloCopy
	add w1, w20, w21
	add x1, x24, w1, SXTW 2
	str w0, [x1, #4]
	add w21, w21, #1
	b .L485
.L486:
// 	  return tt
	mov w0, w20
	b .L474
.L481:
//         tt := GloAlloc(CELL, TERM_SIZE);
	mov w1, #2
	mov w0, #4
	bl _GloAlloc
	mov w20, w0
//         mem[tt+1] := NULL;
	ldr x24, =_mem
	add x0, x24, w20, SXTW 2
	str wzr, [x0, #4]
// 	Save(t); mem[t+1] := tt;
	ldr w0, [fp, #64]
	bl _Save
	ldr w0, [fp, #64]
	add x0, x24, w0, SXTW 2
	str w20, [x0, #4]
//         return tt
	mov w0, w20
	b .L474
.L478:
//       return t
	ldr w0, [fp, #64]
.L474:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc Share(v1, v2: term);
_Share:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if (v1 * (2 * ord((v1 >= gsp)) - 1)) <= (v2 * (2 * ord((v2 >= gsp)) - 1)) then
	ldr w20, [fp, #48]
	ldr x0, =_gsp
	ldr w21, [x0]
	ldr w22, [fp, #56]
	cmp w20, w21
	cset w0, ge
	lsl w0, w0, #1
	sub w0, w0, #1
	mul w0, w20, w0
	cmp w22, w21
	cset w1, ge
	lsl w1, w1, #1
	sub w1, w1, #1
	mul w1, w22, w1
	cmp w0, w1
	bgt .L491
//     Save(v1); mem[v1+1] := v2
	mov x0, x20
	bl _Save
	ldr w0, [fp, #56]
	ldr x1, =_mem
	ldr w2, [fp, #48]
	add x1, x1, w2, SXTW 2
	str w0, [x1, #4]
	b .L489
.L491:
//     Save(v2); mem[v2+1] := v1 
	ldr w0, [fp, #56]
	bl _Save
	ldr w0, [fp, #48]
	ldr x1, =_mem
	ldr w2, [fp, #56]
	add x1, x1, w2, SXTW 2
	str w0, [x1, #4]
.L489:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc Unify(t1: term; e1: frame; t2: term; e2: frame): boolean;
_Unify:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   t1 := Deref(t1, e1); t2 := Deref(t2, e2);
	ldr w1, [fp, #72]
	ldr w0, [fp, #64]
	bl _Deref
	str w0, [fp, #64]
	ldr w1, [fp, #88]
	ldr w0, [fp, #80]
	bl _Deref
	str w0, [fp, #80]
//   if t1 = t2 then  (* Includes unifying a var with itself *)
	ldr w1, [fp, #64]
	cmp w1, w0
	bne .L495
//     return true
	mov w0, #1
	b .L493
.L495:
//   elsif (lsr(mem[t1], 8) = CELL) and (lsr(mem[t2], 8) = CELL) then
	ldr x22, =_mem
	ldr w23, [fp, #64]
	ldr w0, [x22, w23, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #4
	bne .L498
	ldr w24, [fp, #80]
	ldr w0, [x22, w24, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #4
	bne .L498
//     Share(t1, t2); return true
	mov x1, x24
	mov x0, x23
	bl _Share
	mov w0, #1
	b .L493
.L498:
//   elsif lsr(mem[t1], 8) = CELL then
	ldr x22, =_mem
	ldr w23, [fp, #64]
	ldr w0, [x22, w23, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #4
	bne .L501
//     Save(t1); mem[t1+1] := GloCopy(t2, e2); return true
	mov x0, x23
	bl _Save
	ldr w1, [fp, #88]
	ldr w0, [fp, #80]
	bl _GloCopy
	ldr w1, [fp, #64]
	add x1, x22, w1, SXTW 2
	str w0, [x1, #4]
	mov w0, #1
	b .L493
.L501:
//   elsif lsr(mem[t2], 8) = CELL then
	ldr x22, =_mem
	ldr w23, [fp, #80]
	ldr w0, [x22, w23, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #4
	bne .L504
//     Save(t2); mem[t2+1] := GloCopy(t1, e1); return true
	mov x0, x23
	bl _Save
	ldr w1, [fp, #72]
	ldr w0, [fp, #64]
	bl _GloCopy
	ldr w1, [fp, #80]
	add x1, x22, w1, SXTW 2
	str w0, [x1, #4]
	mov w0, #1
	b .L493
.L504:
//   elsif lsr(mem[t1], 8) <> lsr(mem[t2], 8) then
	ldr x22, =_mem
	ldr w0, [fp, #64]
	ldr w0, [x22, w0, SXTW #2]
	lsr w0, w0, #8
	ldr w1, [fp, #80]
	ldr w1, [x22, w1, SXTW #2]
	lsr w1, w1, #8
	cmp w0, w1
	beq .L507
//     return false
	mov w0, wzr
	b .L493
.L507:
//     case lsr(mem[t1], 8) of
	ldr x0, =_mem
	ldr w1, [fp, #64]
	ldr w0, [x0, w1, SXTW #2]
	lsr w0, w0, #8
	sub w0, w0, #1
	cmp w0, #3
	bhs .L509
	adr ip0, .L522
	ldr ip0, [ip0, w0, SXTW #3]
	br ip0
	.p2align 3
.L522:
	.quad .L511
	.quad .L512
	.quad .L513
.L511:
//         if (mem[t1+1] <> mem[t2+1]) then
	ldr x22, =_mem
	ldr w0, [fp, #64]
	add x0, x22, w0, SXTW 2
	ldr w0, [x0, #4]
	ldr w1, [fp, #80]
	add x1, x22, w1, SXTW 2
	ldr w1, [x1, #4]
	cmp w0, w1
	beq .L515
//           return false
	mov w0, wzr
	b .L493
.L515:
//           i := 1; match := true;
	mov w20, #1
	mov w21, #1
.L517:
//           while match and (i <= symtab[mem[t1+1]].arity) do
	cbz w21, .L519
	ldr x22, =_mem
	ldr w23, [fp, #64]
	ldr x0, =_symtab
	add x1, x22, w23, SXTW 2
	ldr w1, [x1, #4]
	add x0, x0, w1, SXTW 4
	ldr w0, [x0, #4]
	cmp w20, w0
	bgt .L519
//             match := Unify(mem[t1+i+1], e1, mem[t2+i+1], e2);
	ldr w3, [fp, #88]
	ldr w0, [fp, #80]
	add w0, w0, w20
	add x0, x22, w0, SXTW 2
	ldr w2, [x0, #4]
	ldr w1, [fp, #72]
	add w0, w23, w20
	add x0, x22, w0, SXTW 2
	ldr w0, [x0, #4]
	bl _Unify
	mov w21, w0
//             i := i+1
	add w20, w20, #1
	b .L517
.L519:
//           return match
	mov w0, w21
	b .L493
.L512:
//         return (mem[t1+1] = mem[t2+1])
	ldr x22, =_mem
	ldr w0, [fp, #64]
	add x0, x22, w0, SXTW 2
	ldr w0, [x0, #4]
	ldr w1, [fp, #80]
	add x1, x22, w1, SXTW 2
	ldr w1, [x1, #4]
	cmp w0, w1
	cset w0, eq
	b .L493
.L513:
//         return (mem[t1+1] = mem[t2+1])
	ldr x22, =_mem
	ldr w0, [fp, #64]
	add x0, x22, w0, SXTW 2
	ldr w0, [x0, #4]
	ldr w1, [fp, #80]
	add x1, x22, w1, SXTW 2
	ldr w1, [x1, #4]
	cmp w0, w1
	cset w0, eq
	b .L493
.L509:
//       newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t1):1, " in ", "Unify"*)); newline(); exit(2)
	bl newline
	mov w1, #8
	ldr x0, =g74
	bl print_string
	mov w1, #8
	ldr x0, =g75
	bl print_string
	bl newline
	mov w0, #2
	bl exit
.L493:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #32
	ret
	.pool

// proc Key(t: term; e: frame): integer;
_Key:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if t = NULL then newline(); print_string("Panic: "); print_string("Key"); newline(); exit(2) end;
	ldr w0, [fp, #48]
	cbnz w0, .L526
	bl newline
	mov w1, #8
	ldr x0, =g76
	bl print_string
	mov w1, #4
	ldr x0, =g77
	bl print_string
	bl newline
	mov w0, #2
	bl exit
.L526:
//   if lsr(mem[t], 8) <> FUNC then newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t):1, " in ", "Key1"*)); newline(); exit(2) end;
	ldr x0, =_mem
	ldr w1, [fp, #48]
	ldr w0, [x0, w1, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #1
	beq .L529
	bl newline
	mov w1, #8
	ldr x0, =g78
	bl print_string
	mov w1, #8
	ldr x0, =g79
	bl print_string
	bl newline
	mov w0, #2
	bl exit
.L529:
//   if symtab[mem[t+1]].arity = 0 then
	ldr x0, =_symtab
	ldr x1, =_mem
	ldr w2, [fp, #48]
	add x1, x1, w2, SXTW 2
	ldr w1, [x1, #4]
	add x0, x0, w1, SXTW 4
	ldr w0, [x0, #4]
	cbnz w0, .L531
//     return 0
	mov w0, wzr
	b .L523
.L531:
//     t0 := Deref(mem[t+1+1], e);
	ldr x21, =_mem
	ldr w1, [fp, #56]
	ldr w0, [fp, #48]
	add x0, x21, w0, SXTW 2
	ldr w0, [x0, #8]
	bl _Deref
	mov w20, w0
//     case lsr(mem[t0], 8) of
	ldr w0, [x21, w20, SXTW #2]
	lsr w0, w0, #8
	sub w0, w0, #1
	cmp w0, #3
	bhs .L533
	adr ip0, .L538
	ldr ip0, [ip0, w0, SXTW #3]
	br ip0
	.p2align 3
.L538:
	.quad .L535
	.quad .L536
	.quad .L537
.L535:
//         FUNC:      return mem[t0+1]
	ldr x0, =_mem
	add x0, x0, w20, SXTW 2
	ldr w0, [x0, #4]
	b .L523
.L536:
//       | INT:       return mem[t0+1] + 1
	ldr x0, =_mem
	add x0, x0, w20, SXTW 2
	ldr w0, [x0, #4]
	add w0, w0, #1
	b .L523
.L537:
//       | CHRCTR:    return mem[t0+1] + 1
	ldr x0, =_mem
	add x0, x0, w20, SXTW 2
	ldr w0, [x0, #4]
	add w0, w0, #1
	b .L523
.L533:
//       return 0
	mov w0, wzr
.L523:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc Search(t: term; e: frame; p: clause): clause;
_Search:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   k := Key(t, e);
	ldr w1, [fp, #56]
	ldr w0, [fp, #48]
	bl _Key
	mov w20, w0
//   if k <> 0 then
	cbz w20, .L542
.L543:
//     while (p <> NULL) and (mem[p+1] <> 0) and (mem[p+1] <> k) do
	ldr w21, [fp, #64]
	cbz w21, .L542
	ldr x0, =_mem
	add x21, x0, w21, SXTW 2
	ldr w22, [x21, #4]
	cbz w22, .L542
	cmp w22, w20
	beq .L542
//       p := mem[p+2]
	ldr w0, [x21, #8]
	str w0, [fp, #64]
	b .L543
.L542:
//   return p
	ldr w0, [fp, #64]
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #32
	ret
	.pool

// proc PushFrame(nvars: integer; retry: clause);
_PushFrame:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   f := LocAlloc((FRAME_SIZE + (nvars)*TERM_SIZE));
	ldr w0, [fp, #64]
	lsl w0, w0, #1
	add w0, w0, #7
	bl _LocAlloc
	mov w20, w0
//   mem[f] := current; mem[f+1] := goalframe;
	ldr x23, =_mem
	ldr x0, =_current
	ldr w0, [x0]
	str w0, [x23, w20, SXTW #2]
	add x23, x23, w20, SXTW 2
	ldr x0, =_goalframe
	ldr w0, [x0]
	str w0, [x23, #4]
//   mem[f+2] := retry; mem[f+3] := choice;
	ldr w0, [fp, #72]
	str w0, [x23, #8]
	ldr x0, =_choice
	ldr w0, [x0]
	str w0, [x23, #12]
//   mem[f+4] := gsp; mem[f+5] := trhead;
	ldr x0, =_gsp
	ldr w0, [x0]
	str w0, [x23, #16]
	ldr x0, =_trhead
	ldr w0, [x0]
	str w0, [x23, #20]
//   mem[f+6] := nvars;
	ldr w24, [fp, #64]
	str w24, [x23, #24]
//   for i := 1 to nvars do
	mov w21, #1
	mov w22, w24
.L549:
	cmp w21, w22
	bgt .L550
//     mem[(f+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
	ldr x23, =_mem
	add w0, w20, #7
	lsl w1, w21, #1
	sub w1, w1, #2
	add w24, w0, w1
	mov w0, #1026
	str w0, [x23, w24, SXTW #2]
//     mem[(f+7+(i-1)*TERM_SIZE)+1] := NULL
	add x0, x23, w24, SXTW 2
	str wzr, [x0, #4]
	add w21, w21, #1
	b .L549
.L550:
//   goalframe := f;
	ldr x0, =_goalframe
	str w20, [x0]
//   if retry <> NULL then choice := goalframe end
	ldr w0, [fp, #72]
	cbz w0, .L548
	ldr x0, =_goalframe
	ldr w23, [x0]
	ldr x0, =_choice
	str w23, [x0]
.L548:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc TroStep();
_TroStep:
	stp x25, x26, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #16
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if dflag then print_string("(TRO)"); newline() end;
	ldr x0, =_dflag
	ldrb w0, [x0]
	cbz w0, .L557
	mov w1, #6
	ldr x0, =g80
	bl print_string
	bl newline
.L557:
//   oldsize := (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE); (* size of old frame *)
	ldr x24, =_mem
	ldr x25, =_goalframe
	ldr w0, [x25]
	add x0, x24, w0, SXTW 2
	ldr w0, [x0, #24]
	lsl w0, w0, #1
	add w21, w0, #7
//   newsize := (FRAME_SIZE + (mem[prok])*TERM_SIZE); (* size of new frame *)
	ldr x0, =_prok
	ldr w0, [x0]
	ldr w0, [x24, w0, SXTW #2]
	lsl w0, w0, #1
	add w22, w0, #7
//   temp := LocAlloc(newsize);
	mov w0, w22
	bl _LocAlloc
	mov w20, w0
//   temp := goalframe + newsize; (* copy old frame here *)
	ldr w0, [x25]
	add w20, w0, w22
//   for i := 1 to oldsize do 
	mov w23, #1
	str w21, [fp, #28]
.L558:
	ldr w0, [fp, #28]
	cmp w23, w0
	bgt .L559
//     mem[temp+oldsize-i] := mem[goalframe+oldsize-i]
	ldr x24, =_mem
	ldr x0, =_goalframe
	ldr w0, [x0]
	add w0, w0, w21
	sub w0, w0, w23
	ldr w0, [x24, w0, SXTW #2]
	add w1, w20, w21
	sub w1, w1, w23
	str w0, [x24, w1, SXTW #2]
	add w23, w23, #1
	b .L558
.L559:
//   for i := 1 to mem[goalframe+6] do
	mov w23, #1
	ldr x0, =_mem
	ldr x1, =_goalframe
	ldr w1, [x1]
	add x0, x0, w1, SXTW 2
	ldr w0, [x0, #24]
	str w0, [fp, #24]
.L560:
	ldr w0, [fp, #24]
	cmp w23, w0
	bgt .L561
//     if (lsr(mem[(temp+7+(i-1)*TERM_SIZE)], 8) = CELL)
	ldr x24, =_mem
	add w0, w20, #7
	lsl w1, w23, #1
	sub w1, w1, #2
	add w25, w0, w1
	ldr w0, [x24, w25, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #4
	bne .L564
	add x24, x24, w25, SXTW 2
	ldr w25, [x24, #4]
	cbz w25, .L564
	ldr x0, =_goalframe
	ldr w26, [x0]
	cmp w26, w25
	bgt .L564
	add w0, w26, w21
	cmp w25, w0
	bge .L564
//       mem[(temp+7+(i-1)*TERM_SIZE)+1] := mem[(temp+7+(i-1)*TERM_SIZE)+1] + newsize
	add w0, w25, w22
	str w0, [x24, #4]
.L564:
	add w23, w23, #1
	b .L560
.L561:
//   mem[goalframe+6] := mem[prok];
	ldr x24, =_mem
	ldr x25, =_goalframe
	ldr x0, =_prok
	ldr w0, [x0]
	ldr w0, [x24, w0, SXTW #2]
	ldr w1, [x25]
	add x1, x24, w1, SXTW 2
	str w0, [x1, #24]
//   for i := 1 to mem[goalframe+6] do
	mov w23, #1
	ldr w0, [x25]
	add x0, x24, w0, SXTW 2
	ldr w0, [x0, #24]
	str w0, [fp, #20]
.L568:
	ldr w0, [fp, #20]
	cmp w23, w0
	bgt .L569
//     mem[(goalframe+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
	ldr x24, =_mem
	ldr x25, =_goalframe
	lsl w0, w23, #1
	sub w26, w0, #2
	mov w0, #1026
	ldr w1, [x25]
	add w1, w1, #7
	add w1, w1, w26
	str w0, [x24, w1, SXTW #2]
//     mem[(goalframe+7+(i-1)*TERM_SIZE)+1] := NULL
	ldr w0, [x25]
	add w0, w0, #7
	add w0, w0, w26
	add x0, x24, w0, SXTW 2
	str wzr, [x0, #4]
	add w23, w23, #1
	b .L568
.L569:
//   ok := Unify(call, temp, mem[prok+3], goalframe);
	ldr x24, =_prok
	ldr x0, =_goalframe
	ldr w3, [x0]
	ldr x0, =_mem
	ldr w1, [x24]
	add x0, x0, w1, SXTW 2
	ldr w2, [x0, #12]
	mov w1, w20
	ldr x0, =_call
	ldr w0, [x0]
	bl _Unify
	ldr x1, =_ok
	strb w0, [x1]
//   current := (prok+4);
	ldr w0, [x24]
	add w0, w0, #4
	ldr x1, =_current
	str w0, [x1]
//   lsp := temp-1
	sub w0, w20, #1
	ldr x1, =_lsp
	str w0, [x1]
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ldp x25, x26, [sp], #16
	ret
	.pool

// proc Step();
_Step:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if symtab[mem[call+1]].action <> 0 then
	ldr x0, =_symtab
	ldr x1, =_mem
	ldr x2, =_call
	ldr w2, [x2]
	add x1, x1, w2, SXTW 2
	ldr w1, [x1, #4]
	add x0, x0, w1, SXTW 4
	ldr w21, [x0, #8]
	cbz w21, .L572
//     ok := DoBuiltin(symtab[mem[call+1]].action)
	mov x0, x21
	bl _DoBuiltin
	ldr x1, =_ok
	strb w0, [x1]
	b .L573
.L572:
//   elsif prok = NULL then
	ldr x0, =_prok
	ldr w0, [x0]
	cbnz w0, .L575
//     ok := false
	ldr x0, =_ok
	strb wzr, [x0]
	b .L573
.L575:
//     retry := Search(call, goalframe, mem[prok+2]);
	ldr x21, =_goalframe
	ldr x22, =_mem
	ldr x0, =_prok
	ldr w0, [x0]
	add x0, x22, w0, SXTW 2
	ldr w2, [x0, #8]
	ldr w1, [x21]
	ldr x0, =_call
	ldr w0, [x0]
	bl _Search
	mov w20, w0
//     if (mem[(current)+1] = NULL) and (choice < goalframe)
	ldr x0, =_current
	ldr w0, [x0]
	add x0, x22, w0, SXTW 2
	ldr w0, [x0, #4]
	cbnz w0, .L578
	ldr w21, [x21]
	ldr x0, =_choice
	ldr w0, [x0]
	cmp w0, w21
	bge .L578
	cbnz w20, .L578
	ldr x0, =_base
	ldr w0, [x0]
	cmp w21, w0
	beq .L578
//       TroStep()
	bl _TroStep
	b .L573
.L578:
//       PushFrame(mem[prok], retry);
	ldr x21, =_mem
	ldr x22, =_prok
	mov w1, w20
	ldr w0, [x22]
	ldr w0, [x21, w0, SXTW #2]
	bl _PushFrame
//       ok := Unify(call, mem[goalframe+1], mem[prok+3], goalframe);
	ldr x0, =_goalframe
	ldr w23, [x0]
	mov x3, x23
	ldr w0, [x22]
	add x0, x21, w0, SXTW 2
	ldr w2, [x0, #12]
	add x0, x21, w23, SXTW 2
	ldr w1, [x0, #4]
	ldr x0, =_call
	ldr w0, [x0]
	bl _Unify
	ldr x1, =_ok
	strb w0, [x1]
//       current := (prok+4);
	ldr w0, [x22]
	add w0, w0, #4
	ldr x1, =_current
	str w0, [x1]
.L573:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

// proc Unwind();
_Unwind:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
.L584:
//   while (mem[current] = NULL) and (goalframe <> base) do
	ldr x20, =_mem
	ldr x0, =_current
	ldr w0, [x0]
	ldr w0, [x20, w0, SXTW #2]
	cbnz w0, .L583
	ldr x21, =_goalframe
	ldr w0, [x21]
	ldr x1, =_base
	ldr w1, [x1]
	cmp w0, w1
	beq .L583
//     if dflag then 
	ldr x0, =_dflag
	ldrb w0, [x0]
	cbz w0, .L589
//     print_string("Exit"); print_string(": "); 
	mov w1, #5
	ldr x0, =g81
	bl print_string
	mov w1, #3
	ldr x0, =g82
	bl print_string
//     PrintTerm(mem[mem[goalframe]], mem[goalframe+1], MAXPRIO); newline()
	ldr w21, [x21]
	mov w2, #2
	add x0, x20, w21, SXTW 2
	ldr w1, [x0, #4]
	ldr w0, [x20, w21, SXTW #2]
	ldr w0, [x20, w0, SXTW #2]
	bl _PrintTerm
	bl newline
.L589:
//     current := (mem[goalframe])+1;
	ldr x0, =_goalframe
	ldr w20, [x0]
	ldr x0, =_mem
	ldr w0, [x0, w20, SXTW #2]
	add w0, w0, #1
	ldr x1, =_current
	str w0, [x1]
//     if goalframe > choice then lsp := goalframe-1 end;
	ldr x0, =_choice
	ldr w0, [x0]
	cmp w20, w0
	ble .L592
	sub w0, w20, #1
	ldr x1, =_lsp
	str w0, [x1]
.L592:
//     goalframe := mem[goalframe+1]
	ldr x20, =_goalframe
	ldr x0, =_mem
	ldr w1, [x20]
	add x0, x0, w1, SXTW 2
	ldr w0, [x0, #4]
	str w0, [x20]
	b .L584
.L583:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// proc Backtrack();
_Backtrack:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   Restore();
	bl _Restore
//   current := mem[choice]; goalframe := mem[choice+1];
	ldr x20, =_mem
	ldr x21, =_choice
	ldr w22, [x21]
	ldr w23, [x20, w22, SXTW #2]
	ldr x0, =_current
	str w23, [x0]
	add x0, x20, w22, SXTW 2
	ldr w22, [x0, #4]
	ldr x24, =_goalframe
	str w22, [x24]
//   call := Deref(mem[current], goalframe);
	mov x1, x22
	ldr w0, [x20, w23, SXTW #2]
	bl _Deref
	ldr x22, =_call
	str w0, [x22]
//   prok := mem[choice+2]; gsp := mem[choice+4];
	ldr w23, [x21]
	add x20, x20, w23, SXTW 2
	ldr w0, [x20, #8]
	ldr x1, =_prok
	str w0, [x1]
	ldr w0, [x20, #16]
	ldr x1, =_gsp
	str w0, [x1]
//   lsp := choice-1; choice := mem[choice+3];
	sub w0, w23, #1
	ldr x1, =_lsp
	str w0, [x1]
	ldr w0, [x20, #12]
	str w0, [x21]
//   if dflag then 
	ldr x0, =_dflag
	ldrb w0, [x0]
	cbz w0, .L597
//     print_string("Redo"); print_string(": "); 
	mov w1, #5
	ldr x0, =g83
	bl print_string
	mov w1, #3
	ldr x0, =g84
	bl print_string
//     PrintTerm(call, goalframe, MAXPRIO); newline()
	mov w2, #2
	ldr w1, [x24]
	ldr w0, [x22]
	bl _PrintTerm
	bl newline
.L597:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

// proc Resume(flag: boolean);
_Resume:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   ok := flag;
	ldrb w0, [fp, #64]
	ldr x1, =_ok
	strb w0, [x1]
.L599:
//   while run do
	ldr x0, =_run
	ldrb w0, [x0]
	cbz w0, .L598
//     if ok then
	ldr x0, =_ok
	ldrb w0, [x0]
	cbz w0, .L603
//       if mem[current] = NULL then return end;
	ldr x0, =_mem
	ldr x1, =_current
	ldr w1, [x1]
	ldr w20, [x0, w1, SXTW #2]
	cbz w20, .L598
//       call := Deref(mem[current], goalframe);
	ldr x21, =_goalframe
	ldr w1, [x21]
	mov x0, x20
	bl _Deref
	ldr x20, =_call
	str w0, [x20]
//       if dflag then 
	ldr x0, =_dflag
	ldrb w0, [x0]
	cbz w0, .L613
//     print_string("Call"); print_string(": "); 
	mov w1, #5
	ldr x0, =g85
	bl print_string
	mov w1, #3
	ldr x0, =g86
	bl print_string
//     PrintTerm(call, goalframe, MAXPRIO); newline()
	mov w2, #2
	ldr w1, [x21]
	ldr w0, [x20]
	bl _PrintTerm
	bl newline
.L613:
//       if (symtab[mem[call+1]].prok = NULL)
	ldr x20, =_symtab
	ldr x21, =_mem
	ldr x22, =_call
	ldr w0, [x22]
	add x0, x21, w0, SXTW 2
	ldr w0, [x0, #4]
	add x23, x20, w0, SXTW 4
	ldr w0, [x23, #12]
	cbnz w0, .L616
	ldr w0, [x23, #8]
	cbnz w0, .L616
// 	newline(); print_string("Error: "); print_string("call to undefined relation "); run := false;
	bl newline
	mov w1, #8
	ldr x0, =g87
	bl print_string
	mov w1, #28
	ldr x0, =g88
	bl print_string
	ldr x0, =_run
	strb wzr, [x0]
// 	WriteString(symtab[mem[call+1]].name);
	ldr w0, [x22]
	add x0, x21, w0, SXTW 2
	ldr w0, [x0, #4]
	add x0, x20, w0, SXTW 4
	ldr w0, [x0]
	bl _WriteString
	b .L598
.L616:
//       prok := Search(call, goalframe, symtab[mem[call+1]].prok)
	ldr x0, =_call
	ldr w20, [x0]
	ldr x0, =_symtab
	ldr x1, =_mem
	add x1, x1, w20, SXTW 2
	ldr w1, [x1, #4]
	add x0, x0, w1, SXTW 4
	ldr w2, [x0, #12]
	ldr x0, =_goalframe
	ldr w1, [x0]
	mov x0, x20
	bl _Search
	ldr x1, =_prok
	str w0, [x1]
	b .L604
.L603:
//       if choice <= base then return end;
	ldr x0, =_choice
	ldr w0, [x0]
	ldr x1, =_base
	ldr w1, [x1]
	cmp w0, w1
	ble .L598
//       Backtrack()
	bl _Backtrack
.L604:
//     Step();
	bl _Step
//     if ok then Unwind() end;
	ldr x0, =_ok
	ldrb w0, [x0]
	cbz w0, .L599
	bl _Unwind
	b .L599
.L598:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc Execute(g: clause);
_Execute:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   lsp := hp; gsp := MEMSIZE+1;
	ldr x0, =_hp
	ldr w0, [x0]
	ldr x1, =_lsp
	str w0, [x1]
	ldr w0, =25001
	ldr x1, =_gsp
	str w0, [x1]
//   current := NULL; goalframe := NULL; choice := NULL; trhead := NULL;
	ldr x21, =_current
	str wzr, [x21]
	ldr x22, =_goalframe
	str wzr, [x22]
	ldr x23, =_choice
	str wzr, [x23]
	ldr x0, =_trhead
	str wzr, [x0]
//   PushFrame(mem[g], NULL);
	mov w1, wzr
	ldr x0, =_mem
	ldr w2, [fp, #64]
	ldr w0, [x0, w2, SXTW #2]
	bl _PushFrame
//   choice := goalframe; base := goalframe; current := (g+4);
	ldr w22, [x22]
	str w22, [x23]
	ldr x0, =_base
	str w22, [x0]
	ldr w0, [fp, #64]
	add w0, w0, #4
	str w0, [x21]
//   run := true;
	ldr x21, =_run
	mov w0, #1
	strb w0, [x21]
//   Resume(true);
	mov w0, #1
	bl _Resume
//   if not run then return end;
	ldrb w0, [x21]
	cbz w0, .L621
.L625:
//   while ok do
	ldr x0, =_ok
	ldrb w0, [x0]
	cbz w0, .L627
//     nsoln := nsoln+1;
	add w20, w20, #1
//     ShowAnswer(base);
	ldr x0, =_base
	ldr w0, [x0]
	bl _ShowAnswer
//     newline();
	bl newline
//     Resume(false);
	mov w0, wzr
	bl _Resume
//     if not run then return end;
	ldr x0, =_run
	ldrb w0, [x0]
	cbz w0, .L621
	b .L625
.L627:
//   if nsoln = 0 then
	cbnz w20, .L621
//     print_string("no"); newline(); newline();
	mov w1, #3
	ldr x0, =g89
	bl print_string
	bl newline
	bl newline
.L621:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc GetArgs();
_GetArgs:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   for i := 1 to symtab[mem[call+1]].arity do
	mov w20, #1
	ldr x0, =_symtab
	ldr x1, =_mem
	ldr x2, =_call
	ldr w2, [x2]
	add x1, x1, w2, SXTW 2
	ldr w1, [x1, #4]
	add x0, x0, w1, SXTW 4
	ldr w21, [x0, #4]
.L635:
	cmp w20, w21
	bgt .L634
//     av[i] := Deref(mem[call+i+1], goalframe)
	ldr x0, =_goalframe
	ldr w1, [x0]
	ldr x0, =_mem
	ldr x2, =_call
	ldr w2, [x2]
	add w2, w2, w20
	add x0, x0, w2, SXTW 2
	ldr w0, [x0, #4]
	bl _Deref
	ldr x1, =_av
	str w0, [x1, w20, SXTW #2]
	add w20, w20, #1
	b .L635
.L634:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// proc NewInt(n: integer): term;
_NewInt:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   t := GloAlloc(INT, TERM_SIZE);
	mov w1, #2
	mov w0, #2
	bl _GloAlloc
	mov w20, w0
//   mem[t+1] := n;
	ldr w0, [fp, #32]
	ldr x1, =_mem
	add x1, x1, w20, SXTW 2
	str w0, [x1, #4]
//   return t
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc DoCut(): boolean;
_DoCut:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   choice := mem[goalframe+3];
	ldr x0, =_goalframe
	ldr w20, [x0]
	ldr x0, =_mem
	add x21, x0, w20, SXTW 2
	ldr w0, [x21, #12]
	ldr x1, =_choice
	str w0, [x1]
//   lsp := goalframe + (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE) - 1;
	ldr w0, [x21, #24]
	lsl w0, w0, #1
	add w0, w0, #7
	add w0, w20, w0
	sub w0, w0, #1
	ldr x1, =_lsp
	str w0, [x1]
//   Commit();
	bl _Commit
//   current := (current)+1;
	ldr x20, =_current
	ldr w0, [x20]
	add w0, w0, #1
	str w0, [x20]
//   return true
	mov w0, #1
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// proc DoCall(): boolean;
_DoCall:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   GetArgs();
	bl _GetArgs
//   if not (lsr(mem[av[1]], 8) = FUNC) then
	ldr x0, =_mem
	ldr x1, =_av
	ldr w1, [x1, #4]
	ldr w0, [x0, w1, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #1
	beq .L641
//     newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
	bl newline
	mov w1, #8
	ldr x0, =g90
	bl print_string
	mov w1, #23
	ldr x0, =g91
	bl print_string
	ldr x0, =_run
	strb wzr, [x0]
//     return false
	mov w0, wzr
	b .L639
.L641:
//     PushFrame(1, NULL);
	mov w1, wzr
	mov w0, #1
	bl _PushFrame
//     mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
	ldr x20, =_mem
	ldr x21, =_goalframe
	ldr w0, [x21]
	add x0, x20, w0, SXTW 2
	ldr w1, [x0, #4]
	ldr x0, =_av
	ldr w0, [x0, #4]
	bl _GloCopy
	ldr w1, [x21]
	add x1, x20, w1, SXTW 2
	str w0, [x1, #32]
//     current := callbody;
	ldr x0, =_callbody
	ldr w0, [x0]
	ldr x1, =_current
	str w0, [x1]
//     return true
	mov w0, #1
.L639:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// proc DoNot(): boolean;
_DoNot:
	stp x25, x26, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   GetArgs();
	bl _GetArgs
//   if not (lsr(mem[av[1]], 8) = FUNC) then
	ldr x0, =_mem
	ldr x1, =_av
	ldr w1, [x1, #4]
	ldr w0, [x0, w1, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #1
	beq .L645
//     newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
	bl newline
	mov w1, #8
	ldr x0, =g92
	bl print_string
	mov w1, #23
	ldr x0, =g93
	bl print_string
	ldr x0, =_run
	strb wzr, [x0]
//     return false
	mov w0, wzr
	b .L643
.L645:
//     PushFrame(1, NULL);
	mov w1, wzr
	mov w0, #1
	bl _PushFrame
//     savebase := base; base := goalframe; choice := goalframe;
	ldr x21, =_base
	ldr w20, [x21]
	ldr x22, =_goalframe
	ldr w23, [x22]
	str w23, [x21]
	ldr x24, =_choice
	str w23, [x24]
//     mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
	ldr x25, =_mem
	add x0, x25, w23, SXTW 2
	ldr w1, [x0, #4]
	ldr x0, =_av
	ldr w0, [x0, #4]
	bl _GloCopy
	ldr w1, [x22]
	add x1, x25, w1, SXTW 2
	str w0, [x1, #32]
//     current := callbody;
	ldr x23, =_current
	ldr x0, =_callbody
	ldr w0, [x0]
	str w0, [x23]
//     Resume(true);
	mov w0, #1
	bl _Resume
//     choice := mem[base+3]; goalframe := mem[base+1];
	ldr w21, [x21]
	add x26, x25, w21, SXTW 2
	ldr w0, [x26, #12]
	str w0, [x24]
	ldr w0, [x26, #4]
	str w0, [x22]
//     if not ok then
	ldr x0, =_ok
	ldrb w0, [x0]
	cbnz w0, .L648
//       current := (mem[base])+1;
	ldr w0, [x25, w21, SXTW #2]
	add w0, w0, #1
	str w0, [x23]
//       return true
	mov w0, #1
	b .L643
.L648:
//       Commit();
	bl _Commit
//       return false
	mov w0, wzr
.L643:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ldp x25, x26, [sp], #16
	ret
	.pool

// proc DoPlus(): boolean;
_DoPlus:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   GetArgs();
	bl _GetArgs
//   result := false;
	mov w20, wzr
//   if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
	ldr x21, =_mem
	ldr x22, =_av
	ldr w23, [x22, #4]
	ldr w0, [x21, w23, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #2
	bne .L652
	ldr w24, [x22, #8]
	ldr w0, [x21, w24, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #2
	bne .L652
//     result := Unify(av[3], goalframe, NewInt(mem[av[1]+1] + mem[av[2]+1]), NULL)
	add x0, x21, w23, SXTW 2
	ldr w0, [x0, #4]
	add x1, x21, w24, SXTW 2
	ldr w1, [x1, #4]
	add w0, w0, w1
	bl _NewInt
	mov w3, wzr
	mov x2, x0
	ldr x0, =_goalframe
	ldr w1, [x0]
	ldr w0, [x22, #12]
	bl _Unify
	mov w20, w0
	b .L653
.L652:
//   elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
	ldr x21, =_mem
	ldr x22, =_av
	ldr w23, [x22, #4]
	ldr w0, [x21, w23, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #2
	bne .L655
	ldr w24, [x22, #12]
	ldr w0, [x21, w24, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #2
	bne .L655
//     if mem[av[1]+1] <= mem[av[3]+1] then
	add x0, x21, w23, SXTW 2
	ldr w23, [x0, #4]
	add x0, x21, w24, SXTW 2
	ldr w21, [x0, #4]
	cmp w23, w21
	bgt .L653
//       result := Unify(av[2], goalframe, 
	sub w0, w21, w23
	bl _NewInt
	mov w3, wzr
	mov x2, x0
	ldr x0, =_goalframe
	ldr w1, [x0]
	ldr w0, [x22, #8]
	bl _Unify
	mov w20, w0
	b .L653
.L655:
//   elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
	ldr x21, =_mem
	ldr x22, =_av
	ldr w23, [x22, #8]
	ldr w0, [x21, w23, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #2
	bne .L658
	ldr w24, [x22, #12]
	ldr w0, [x21, w24, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #2
	bne .L658
//     if mem[av[2]+1] <= mem[av[3]+1] then
	add x0, x21, w23, SXTW 2
	ldr w23, [x0, #4]
	add x0, x21, w24, SXTW 2
	ldr w21, [x0, #4]
	cmp w23, w21
	bgt .L653
//       result := Unify(av[1], goalframe, NewInt(mem[av[3]+1] - mem[av[2]+1]), NULL)
	sub w0, w21, w23
	bl _NewInt
	mov w3, wzr
	mov x2, x0
	ldr x0, =_goalframe
	ldr w1, [x0]
	ldr w0, [x22, #4]
	bl _Unify
	mov w20, w0
	b .L653
.L658:
//     newline(); print_string("Error: "); print_string("plus/3 needs at least two integers"); run := false
	bl newline
	mov w1, #8
	ldr x0, =g94
	bl print_string
	mov w1, #35
	ldr x0, =g95
	bl print_string
	ldr x0, =_run
	strb wzr, [x0]
.L653:
//   current := (current)+1;
	ldr x21, =_current
	ldr w0, [x21]
	add w0, w0, #1
	str w0, [x21]
//   return result
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

// proc DoTimes(): boolean;
_DoTimes:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   GetArgs();
	bl _GetArgs
//   result := false;
	mov w20, wzr
//   if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
	ldr x21, =_mem
	ldr x22, =_av
	ldr w23, [x22, #4]
	ldr w0, [x21, w23, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #2
	bne .L671
	ldr w24, [x22, #8]
	ldr w0, [x21, w24, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #2
	bne .L671
//     result := Unify(av[3], goalframe, 
	add x0, x21, w23, SXTW 2
	ldr w0, [x0, #4]
	add x1, x21, w24, SXTW 2
	ldr w1, [x1, #4]
	mul w0, w0, w1
	bl _NewInt
	mov w3, wzr
	mov x2, x0
	ldr x0, =_goalframe
	ldr w1, [x0]
	ldr w0, [x22, #12]
	bl _Unify
	mov w20, w0
	b .L672
.L671:
//   elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
	ldr x21, =_mem
	ldr x22, =_av
	ldr w23, [x22, #4]
	ldr w0, [x21, w23, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #2
	bne .L674
	ldr w24, [x22, #12]
	ldr w0, [x21, w24, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #2
	bne .L674
//     if mem[av[1]+1] <> 0 then
	add x0, x21, w23, SXTW 2
	ldr w23, [x0, #4]
	cbz w23, .L672
//       if mem[av[3]+1] mod mem[av[1]+1] = 0 then
	mov x1, x23
	add x0, x21, w24, SXTW 2
	ldr w0, [x0, #4]
	bl int_mod
	cbnz w0, .L672
//         result := Unify(av[2], goalframe, 
	ldr w0, [x22, #4]
	add x0, x21, w0, SXTW 2
	ldr w1, [x0, #4]
	ldr w0, [x22, #12]
	add x0, x21, w0, SXTW 2
	ldr w0, [x0, #4]
	bl int_div
	bl _NewInt
	mov w3, wzr
	mov x2, x0
	ldr x0, =_goalframe
	ldr w1, [x0]
	ldr w0, [x22, #8]
	bl _Unify
	mov w20, w0
	b .L672
.L674:
//   elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
	ldr x21, =_mem
	ldr x22, =_av
	ldr w23, [x22, #8]
	ldr w0, [x21, w23, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #2
	bne .L677
	ldr w24, [x22, #12]
	ldr w0, [x21, w24, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #2
	bne .L677
//     if mem[av[2]+1] <> 0 then
	add x0, x21, w23, SXTW 2
	ldr w23, [x0, #4]
	cbz w23, .L672
//       if mem[av[3]+1] mod mem[av[2]+1] = 0 then
	mov x1, x23
	add x0, x21, w24, SXTW 2
	ldr w0, [x0, #4]
	bl int_mod
	cbnz w0, .L672
//         result := Unify(av[1], goalframe, 
	ldr w0, [x22, #8]
	add x0, x21, w0, SXTW 2
	ldr w1, [x0, #4]
	ldr w0, [x22, #12]
	add x0, x21, w0, SXTW 2
	ldr w0, [x0, #4]
	bl int_div
	bl _NewInt
	mov w3, wzr
	mov x2, x0
	ldr x0, =_goalframe
	ldr w1, [x0]
	ldr w0, [x22, #4]
	bl _Unify
	mov w20, w0
	b .L672
.L677:
//     newline(); print_string("Error: "); print_string("times/3 needs at least two integers"); run := false
	bl newline
	mov w1, #8
	ldr x0, =g96
	bl print_string
	mov w1, #36
	ldr x0, =g97
	bl print_string
	ldr x0, =_run
	strb wzr, [x0]
.L672:
//   current := (current)+1;
	ldr x21, =_current
	ldr w0, [x21]
	add w0, w0, #1
	str w0, [x21]
//   return result
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

// proc DoEqual(): boolean;
_DoEqual:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   GetArgs();
	bl _GetArgs
//   current := (current)+1;
	ldr x20, =_current
	ldr w0, [x20]
	add w0, w0, #1
	str w0, [x20]
//   return Unify(av[1], goalframe, av[2], goalframe)
	ldr x20, =_av
	ldr x0, =_goalframe
	ldr w21, [x0]
	mov x3, x21
	ldr w2, [x20, #8]
	mov x1, x21
	ldr w0, [x20, #4]
	bl _Unify
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// proc DoInteger(): boolean;
_DoInteger:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   GetArgs();
	bl _GetArgs
//   current := (current)+1;
	ldr x20, =_current
	ldr w0, [x20]
	add w0, w0, #1
	str w0, [x20]
//   return (lsr(mem[av[1]], 8) = INT)
	ldr x0, =_mem
	ldr x1, =_av
	ldr w1, [x1, #4]
	ldr w0, [x0, w1, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #2
	cset w0, eq
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

// proc DoChar(): boolean;
_DoChar:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   GetArgs();
	bl _GetArgs
//   current := (current)+1;
	ldr x20, =_current
	ldr w0, [x20]
	add w0, w0, #1
	str w0, [x20]
//   return (lsr(mem[av[1]], 8) = CHRCTR)
	ldr x0, =_mem
	ldr x1, =_av
	ldr w1, [x1, #4]
	ldr w0, [x0, w1, SXTW #2]
	lsr w0, w0, #8
	cmp w0, #3
	cset w0, eq
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

// proc DoPrint(): boolean;
_DoPrint:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   GetArgs();
	bl _GetArgs
//   PrintTerm(av[1], goalframe, MAXPRIO);
	mov w2, #2
	ldr x0, =_goalframe
	ldr w1, [x0]
	ldr x0, =_av
	ldr w0, [x0, #4]
	bl _PrintTerm
//   current := (current)+1;
	ldr x20, =_current
	ldr w0, [x20]
	add w0, w0, #1
	str w0, [x20]
//   return true
	mov w0, #1
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

// proc DoNl(): boolean;
_DoNl:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   newline();
	bl newline
//   current := (current)+1;
	ldr x20, =_current
	ldr w0, [x20]
	add w0, w0, #1
	str w0, [x20]
//   return true
	mov w0, #1
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

// proc DoBuiltin(action: integer): boolean;
_DoBuiltin:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   case action of
	ldr w0, [fp, #16]
	sub w0, w0, #1
	cmp w0, #11
	bhs .L700
	adr ip0, .L713
	ldr ip0, [ip0, w0, SXTW #3]
	br ip0
	.p2align 3
.L713:
	.quad .L702
	.quad .L703
	.quad .L704
	.quad .L705
	.quad .L706
	.quad .L707
	.quad .L708
	.quad .L709
	.quad .L710
	.quad .L711
	.quad .L712
.L702:
//     CUT:      return DoCut()
	bl _DoCut
	b .L699
.L703:
//   | CALL:     return DoCall()
	bl _DoCall
	b .L699
.L704:
//   | PLUS:     return DoPlus()
	bl _DoPlus
	b .L699
.L705:
//   | TIMES:    return DoTimes()
	bl _DoTimes
	b .L699
.L706:
//   | ISINT:    return DoInteger()
	bl _DoInteger
	b .L699
.L707:
//   | ISCHAR:   return DoChar()
	bl _DoChar
	b .L699
.L708:
//   | NAFF:     return DoNot()
	bl _DoNot
	b .L699
.L709:
//   | EQUALITY: return DoEqual()
	bl _DoEqual
	b .L699
.L710:
//   | FAIL:     return false
	mov w0, wzr
	b .L699
.L711:
//   | PRINT:    return DoPrint()
	bl _DoPrint
	b .L699
.L712:
//   | NL:	      return DoNl()
	bl _DoNl
	b .L699
.L700:
//     newline(); print_string("Panic: "); print_string("bad tag" (*action:1, " in ", "DoBuiltin"*)); newline(); exit(2)
	bl newline
	mov w1, #8
	ldr x0, =g98
	bl print_string
	mov w1, #8
	ldr x0, =g99
	bl print_string
	bl newline
	mov w0, #2
	bl exit
.L699:
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc Initialize();
_Initialize:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   dflag := false; errcount := 0;
	ldr x0, =_dflag
	strb wzr, [x0]
	ldr x0, =_errcount
	str wzr, [x0]
//   pbchar := ENDFILE; charptr := 0;
	mov w0, #127
	ldr x1, =_pbchar
	strb w0, [x1]
	ldr x0, =_charptr
	str wzr, [x0]
//   hp := 0; InitSymbols();
	ldr x0, =_hp
	str wzr, [x0]
	bl _InitSymbols
//   for i := 1 to MAXARITY do
	mov w20, #1
	mov w22, #63
.L715:
	cmp w20, w22
	bgt .L716
//     p := HeapAlloc(TERM_SIZE);
	mov w0, #2
	bl _HeapAlloc
	mov w21, w0
//     mem[p] := lsl(REF, 8) + TERM_SIZE;
	ldr x23, =_mem
	mov w0, #1282
	str w0, [x23, w21, SXTW #2]
//     mem[p+1] := i; refnode[i] := p
	add x0, x23, w21, SXTW 2
	str w20, [x0, #4]
	ldr x0, =_refnode
	str w21, [x0, w20, SXTW #2]
	add w20, w20, #1
	b .L715
.L716:
//   callbody := HeapAlloc(2);
	mov w0, #2
	bl _HeapAlloc
	ldr x23, =_callbody
	str w0, [x23]
//   mem[callbody] := MakeRef(1);
	mov w0, #1
	bl _MakeRef
	ldr x24, =_mem
	ldr w1, [x23]
	str w0, [x24, w1, SXTW #2]
//   mem[(callbody)+1] := NULL
	ldr w0, [x23]
	add x0, x24, w0, SXTW 2
	str wzr, [x0, #4]
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

// proc ReadFile();
_ReadFile:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   lineno := 1;
	mov w0, #1
	ldr x1, =_lineno
	str w0, [x1]
.L718:
//     hmark := hp;
	ldr x0, =_hp
	ldr w0, [x0]
	ldr x1, =_hmark
	str w0, [x1]
//     c := ReadClause();
	bl _ReadClause
	mov w20, w0
//     if c <> NULL then
	cbz w20, .L722
//       if dflag then PrintClause(c) end;	
	ldr x0, =_dflag
	ldrb w0, [x0]
	cbz w0, .L725
	mov w0, w20
	bl _PrintClause
.L725:
//       if mem[c+3] <> NULL then
	ldr x0, =_mem
	add x0, x0, w20, SXTW 2
	ldr w0, [x0, #12]
	cbz w0, .L727
//         AddClause(c)
	mov w0, w20
	bl _AddClause
	b .L722
.L727:
//         Execute(c);
	mov w0, w20
	bl _Execute
// 	hp := hmark
	ldr x0, =_hmark
	ldr w0, [x0]
	ldr x1, =_hp
	str w0, [x1]
.L722:
	cbnz w20, .L718
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   prog("subject(                                                    ");
	ldr x0, =g100
	bl _prog
//   prog("  <store,                                                   ");
	ldr x0, =g101
	bl _prog
//   prog("    <load,                                                  ");
	ldr x0, =g102
	bl _prog
//   prog("      <plusa,                                               ");
	ldr x0, =g103
	bl _prog
//   prog("        <global(a)>,                                        ");
	ldr x0, =g104
	bl _prog
//   prog("        <lsl, <load, <local(16)>>, <const(2)>>>>,           ");
	ldr x0, =g105
	bl _prog
//   prog("    <local(20)>>                                            ");
	ldr x0, =g106
	bl _prog
//   prog(") :- .                                                      ");
	ldr x0, =g107
	bl _prog
//   prog("rule(""*str"", stmt, <store, reg, addr>) :- .                 ");
	ldr x0, =g108
	bl _prog
//   prog("rule(""*ldr"", reg,  <load, addr>) :- .                       ");
	ldr x0, =g109
	bl _prog
//   prog("rule(""*addfp"", reg, <local(N)>) :- .                        ");
	ldr x0, =g110
	bl _prog
//   prog("rule(""local"", addr, <local(N)>) :- .                        ");
	ldr x0, =g111
	bl _prog
//   prog("rule(""*add"", reg, <plusa, reg, rand>) :- .                  ");
	ldr x0, =g112
	bl _prog
//   prog("rule(""index"", addr, <plusa, reg, reg>) :- .                 ");
	ldr x0, =g113
	bl _prog
//   prog("rule(""scale"", addr,                                         ");
	ldr x0, =g114
	bl _prog
//   prog("       <plusa, reg, <lsl, reg, <const(N)>>>) :- .           ");
	ldr x0, =g115
	bl _prog
//   prog("rule(""*global"", reg, <global(X)>) :- .                      ");
	ldr x0, =g116
	bl _prog
//   prog("rule(""*lsl"", reg, <lsl, reg, rand>) :- .                    ");
	ldr x0, =g117
	bl _prog
//   prog("rule(""lshiftc"", rand, <lsl, reg, <const(N)>>) :- .          ");
	ldr x0, =g118
	bl _prog
//   prog("rule(""lshiftr"", rand, <lsl, reg, reg>) :- .                 ");
	ldr x0, =g119
	bl _prog
//   prog("rule(""*mov"", reg, <const(N)>) :- .                          ");
	ldr x0, =g120
	bl _prog
//   prog("rule(""const"", rand, <const(N)>) :- .                        ");
	ldr x0, =g121
	bl _prog
//   prog("rule(""reg"", rand, reg) :- .                                 ");
	ldr x0, =g122
	bl _prog
//   prog("rule(""indir"", addr, reg) :- .                               ");
	ldr x0, =g123
	bl _prog
//   prog("use_rule(NT, Tree, node(Name, Kids)) :-                     ");
	ldr x0, =g124
	bl _prog
//   prog("  rule(Name, NT, RHS), match(RHS, Tree, Kids, nil).         ");
	ldr x0, =g125
	bl _prog
//   prog("match(NT, Tree, Parse:Kids0, Kids0) :-                      ");
	ldr x0, =g126
	bl _prog
//   prog("  use_rule(NT, Tree, Parse).                                ");
	ldr x0, =g127
	bl _prog
//   prog("match(node(W, PS), node(W, TS), Kids, Kids0) :-             ");
	ldr x0, =g128
	bl _prog
//   prog("  matchall(PS, TS, Kids, Kids0).                            ");
	ldr x0, =g129
	bl _prog
//   prog("matchall(nil, nil, Kids0, Kids0) :- .                       ");
	ldr x0, =g130
	bl _prog
//   prog("matchall(P:PS, T:TS, Kids, Kids0) :-                        ");
	ldr x0, =g131
	bl _prog
//   prog("  match(P, T, Kids, Kids1), matchall(PS, TS, Kids1, Kids0). ");
	ldr x0, =g132
	bl _prog
//   prog("cost(node(X, TS), C) :-                                     ");
	ldr x0, =g133
	bl _prog
//   prog("  opcost(X, A), allcosts(TS, B), plus(A, B, C).             ");
	ldr x0, =g134
	bl _prog
//   prog("allcosts(nil, 0) :- .                                       ");
	ldr x0, =g135
	bl _prog
//   prog("allcosts(T:TS, C) :-                                        ");
	ldr x0, =g136
	bl _prog
//   prog("  cost(T, A), allcosts(TS, B), plus(A, B, C).               ");
	ldr x0, =g137
	bl _prog
//   prog("opcost('*':_, 1) :- !.                                      ");
	ldr x0, =g138
	bl _prog
//   prog("opcost(_, 0) :- .                                           ");
	ldr x0, =g139
	bl _prog
//   prog("answer(P, C) :-                                             ");
	ldr x0, =g140
	bl _prog
//   prog("  subject(T), use_rule(stmt, T, P), cost(P, C).             ");
	ldr x0, =g141
	bl _prog
//   prog("min(N, P) :- min1(N, 0, P).                                 ");
	ldr x0, =g142
	bl _prog
//   prog("min1(N, N, P) :- call(P), !.                                ");
	ldr x0, =g143
	bl _prog
//   prog("min1(N, N0, P) :- plus(N0, 1, N1), min1(N, N1, P).          ");
	ldr x0, =g144
	bl _prog
//   prog("# :- answer(P, C).                                          ");
	ldr x0, =g145
	bl _prog
//   Initialize();
	bl _Initialize
//   ReadFile()
	bl _ReadFile
	ldp fp, lr, [sp], #16
	ret
	.pool

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
// End
