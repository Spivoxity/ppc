// picoPascal compiler output
	.global pmain

// proc fac(n: integer): integer;
	.section .text
_fac:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if n = 0 then
	ldr w0, [fp, #32]
	cbnz w0, .L3
//     return 1
	mov w0, #1
	b .L1
.L3:
//     return n * fac(n-1)
	ldr w20, [fp, #32]
	sub w0, w20, #1
	bl _fac
	mul w0, w20, w0
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   f := fac(10);
	mov w0, #10
	bl _fac
	ldr x1, =_f
	str w0, [x1]
//   print_num(f);
	bl print_num
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

	.comm _f, 4, 4
// End
