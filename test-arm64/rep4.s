// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     print_string("Hello"); newline()
	mov w1, #6
	ldr x0, =g1
	bl print_string
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

	.section .rodata
g1:
	.byte 72, 101, 108, 108, 111
	.byte 0
// End
