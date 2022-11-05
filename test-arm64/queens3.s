// picoPascal compiler output
	.global pmain

// proc queens(k: integer);
	.section .text
_queens:
	stp x0, x1, [sp, -16]!
	stp x27, x28, [sp, -16]!
	stp x25, x26, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if k = N then
	ldr w0, [fp, #96]
	cmp w0, #8
	bne .L3
//     print()
	bl _print
	b .L1
.L3:
//     y := 0;
	mov w20, wzr
.L5:
//     while y < N do
	cmp w20, #8
	bge .L1
//       if rank[y] and diagup[k+y] and diagdown[k+(N-1)-y] then
	ldr x24, =_rank
	ldrb w0, [x24, w20, SXTW]
	cbz w0, .L10
	ldr x25, =_diagup
	ldr w26, [fp, #96]
	add x27, x25, w26, SXTW
	ldrb w0, [x27, w20, SXTW]
	cbz w0, .L10
	ldr x28, =_diagdown
	add w0, w26, #7
	sub w0, w0, w20
	ldrb w1, [x28, w0, SXTW]
	cbz w1, .L10
// 	rank[y] := false; diagup[k+y] := false; diagdown[k+(N-1)-y] := false;
	strb wzr, [x24, w20, SXTW]
	strb wzr, [x27, w20, SXTW]
	strb wzr, [x28, w0, SXTW]
// 	choice[k] := y; queens(k+1);
	ldr x0, =_choice
	str w20, [x0, w26, SXTW #2]
	add w0, w26, #1
	bl _queens
// 	rank[y] := true; diagup[k+y] := true; diagdown[k+(N-1)-y] := true;
	mov w0, #1
	strb w0, [x24, w20, SXTW]
	ldr w24, [fp, #96]
	mov w0, #1
	add x1, x25, w24, SXTW
	strb w0, [x1, w20, SXTW]
	mov w0, #1
	add w1, w24, #7
	sub w1, w1, w20
	strb w0, [x28, w1, SXTW]
.L10:
//       y := y+1
	add w20, w20, #1
	b .L5
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ldp x25, x26, [sp], #16
	ldp x27, x28, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc print();
_print:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := 0;
	mov w20, wzr
.L14:
//   while x < N do
	cmp w20, #8
	bge .L16
//     print_num(choice[x]+1);
	ldr x0, =_choice
	ldr w0, [x0, w20, SXTW #2]
	add w0, w0, #1
	bl print_num
//     x := x+1
	add w20, w20, #1
	b .L14
.L16:
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

// proc init();
_init:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0; 
	mov w20, wzr
.L18:
//   while i < N do 
	cmp w20, #8
	bge .L20
//     rank[i] := true; 
	mov w0, #1
	ldr x1, =_rank
	strb w0, [x1, w20, SXTW]
//     i := i+1 
	add w20, w20, #1
	b .L18
.L20:
//   i := 0; 
	mov w20, wzr
.L21:
//   while i < 2*N-1 do 
	cmp w20, #15
	bge .L17
//     diagup[i] := true; diagdown[i] := true ;
	mov w0, #1
	ldr x1, =_diagup
	strb w0, [x1, w20, SXTW]
	mov w0, #1
	ldr x1, =_diagdown
	strb w0, [x1, w20, SXTW]
//     i := i+1
	add w20, w20, #1
	b .L21
.L17:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   init();
	bl _init
//   queens(0)
	mov w0, wzr
	bl _queens
	ldp fp, lr, [sp], #16
	ret
	.pool

	.comm _choice, 32, 4
	.comm _rank, 8, 4
	.comm _diagup, 15, 4
	.comm _diagdown, 15, 4
// End
