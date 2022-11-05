// picoPascal compiler output
	.global pmain

// proc repchar(c: char; n: integer);
	.section .text
_repchar:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if n > 0 then
	ldr w0, [fp, #24]
	cmp w0, #0
	ble .L1
//     print_char(c);
	ldrb w0, [fp, #16]
	bl print_char
//     repchar(c, n-1)
	ldr w0, [fp, #24]
	sub w1, w0, #1
	ldrb w0, [fp, #16]
	bl _repchar
.L1:
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   repchar('A', 3);
	mov w1, #3
	mov w0, #65
	bl _repchar
//   repchar('B', 5);
	mov w1, #5
	mov w0, #66
	bl _repchar
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
