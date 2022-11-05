// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   for i := 1 to 5 do
	mov w0, #1
	ldr x1, =_i
	str w0, [x1]
	mov w20, #5
.L2:
	ldr x21, =_i
	ldr w22, [x21]
	cmp w22, w20
	bgt .L1
//     print_num(i);
	mov x0, x22
	bl print_num
//     newline()
	bl newline
	ldr w0, [x21]
	add w0, w0, #1
	str w0, [x21]
	b .L2
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

	.comm _i, 4, 4
// End
