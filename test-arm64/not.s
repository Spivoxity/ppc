// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   b := not b;
	ldr x20, =_b
	ldrb w0, [x20]
	eor w21, w0, #1
	strb w21, [x20]
//   if b then print_string("ok"); newline() end
	cbz w21, .L1
	mov w1, #3
	ldr x0, =g1
	bl print_string
	bl newline
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

	.comm _b, 1, 4
	.section .rodata
g1:
	.byte 111, 107
	.byte 0
// End
