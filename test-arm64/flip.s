// picoPascal compiler output
	.global pmain

// proc flip(x: integer): integer;
	.section .text
_flip:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if x = 0 then return 1 else return 2 * flop(x-1) end
	ldr w0, [fp, #32]
	cbnz w0, .L3
	mov w0, #1
	b .L1
.L3:
	ldr w0, [fp, #32]
	sub w0, w0, #1
	add x19, fp, #16
	bl _flop
	lsl w0, w0, #1
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

//   proc flop(y: integer): integer;
_flop:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     if y = 0 then return 1 else return flip(y-1) + x end
	ldr w0, [fp, #32]
	cbnz w0, .L7
	mov w0, #1
	b .L5
.L7:
	ldr w0, [fp, #32]
	sub w0, w0, #1
	bl _flip
	ldr x1, [fp, #16]
	ldr w1, [x1, #16]
	add w0, w0, w1
.L5:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(flip(5));
	mov w0, #5
	bl _flip
	bl print_num
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
