// picoPascal compiler output
	.global pmain

// proc P();
	.section .text
_P:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #48
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0;
	mov w20, wzr
//   x := a[i]
	add x0, fp, #24
	ldr w21, [x0, w20, SXTW #2]
	ldp fp, lr, [sp], #16
	add sp, sp, #48
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   P()
	bl _P
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
