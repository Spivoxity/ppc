// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(2); newline()
	mov w0, #2
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
