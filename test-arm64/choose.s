// picoPascal compiler output
	.global pmain

// proc choose(n, k: integer): integer;
	.section .text
_choose:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if n = 0 then
	ldr w0, [fp, #32]
	cbnz w0, .L3
//     if k = 0 then
	ldr w0, [fp, #40]
	cbnz w0, .L6
//       return 1
	mov w0, #1
	b .L1
.L6:
//       return 0
	mov w0, wzr
	b .L1
.L3:
//     return choose(n-1, k-1) + choose(n-1, k)
	ldr w0, [fp, #40]
	sub w1, w0, #1
	ldr w0, [fp, #32]
	sub w0, w0, #1
	bl _choose
	ldr w1, [fp, #40]
	mov x20, x0
	ldr w0, [fp, #32]
	sub w0, w0, #1
	bl _choose
	add w0, w20, w0
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(choose(6,4));
	mov w1, #4
	mov w0, #6
	bl _choose
	bl print_num
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
