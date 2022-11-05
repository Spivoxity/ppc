// picoPascal compiler output
	.global pmain

// proc choose(k, n: integer);
	.section .text
_choose:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if k <= n then
	ldr w20, [fp, #48]
	ldr w0, [fp, #56]
	cmp w20, w0
	bgt .L4
//     if k = 0 then
	cbnz w20, .L6
//       print_string(buf); newline()
	mov w1, #10
	ldr x0, =_buf
	bl print_string
	bl newline
	b .L4
.L6:
//       choose(k, n-1);
	ldr w0, [fp, #56]
	sub w1, w0, #1
	ldr w0, [fp, #48]
	bl _choose
//       buf[k-1] := letters[n-1];
	ldr w20, [fp, #56]
	ldr w21, [fp, #48]
	ldr x0, =g1
	add x0, x0, w20, SXTW
	ldrb w0, [x0, #-1]
	ldr x1, =_buf
	add x1, x1, w21, SXTW
	strb w0, [x1, #-1]
//       choose(k-1, n-1);
	sub w1, w20, #1
	sub w0, w21, #1
	bl _choose
.L4:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   choose(3, 6)
	mov w1, #6
	mov w0, #3
	bl _choose
	ldp fp, lr, [sp], #16
	ret
	.pool

	.comm _buf, 10, 4
	.section .rodata
g1:
	.byte 97, 98, 99, 100, 101, 102
	.byte 0
// End
