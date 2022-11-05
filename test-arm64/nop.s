// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
