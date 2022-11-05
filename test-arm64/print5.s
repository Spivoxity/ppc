// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_string("five"); newline()
	mov w1, #5
	ldr x0, =g1
	bl print_string
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

	.section .rodata
g1:
	.byte 102, 105, 118, 101
	.byte 0
// End
