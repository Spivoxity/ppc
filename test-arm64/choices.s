// picoPascal compiler output
	.global pmain

// proc choose(k, n: integer; proc suffix());
	.section .text
_choose:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if k <= n then
	ldr w20, [fp, #32]
	ldr w0, [fp, #40]
	cmp w20, w0
	bgt .L1
//     if k = 0 then
	cbnz w20, .L6
//       suffix(); newline()
	ldr x19, [fp, #56]
	ldr x0, [fp, #48]
	blr x0
	bl newline
	b .L1
.L6:
//       choose(k, n-1, suffix);
	ldr x3, [fp, #56]
	ldr x2, [fp, #48]
	ldr w0, [fp, #40]
	sub w1, w0, #1
	ldr w0, [fp, #32]
	bl _choose
//       choose(k-1, n-1, suffix1)
	add x3, fp, #16
	ldr x2, =_suffix1
	ldr w0, [fp, #40]
	sub w1, w0, #1
	ldr w0, [fp, #32]
	sub w0, w0, #1
	bl _choose
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #32
	ret
	.pool

//   proc suffix1();
_suffix1:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     print_char(letters[n-1]); suffix()
	add x20, fp, #16
	ldr x0, =g1
	ldr x1, [x20]
	ldr w1, [x1, #24]
	add x0, x0, w1, SXTW
	ldrb w0, [x0, #-1]
	bl print_char
	ldr x0, [x20]
	ldr w1, =32
	add x20, x0, w1, SXTW
	ldr x19, [x20, #8]
	ldr x0, [x20]
	blr x0
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

// proc null(); begin end;
_null:
	stp fp, lr, [sp, -16]!
	mov fp, sp
	ldp fp, lr, [sp], #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   choose(3, 6, null)
	mov x3, xzr
	ldr x2, =_null
	mov w1, #6
	mov w0, #3
	bl _choose
	ldp fp, lr, [sp], #16
	ret
	.pool

	.section .rodata
g1:
	.byte 97, 98, 99, 100, 101, 102
	.byte 0
// End
