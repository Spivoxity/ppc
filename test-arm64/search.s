// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0; found := false;
	ldr x0, =_i
	str wzr, [x0]
	ldr x0, =_found
	strb wzr, [x0]
.L2:
//   while not found do
	ldr x20, =_found
	ldrb w0, [x20]
	cbnz w0, .L4
//     found := target[i] = 'd';
	ldr x21, =_i
	ldr w22, [x21]
	ldr x0, =g1
	ldrb w0, [x0, w22, SXTW]
	cmp w0, #100
	cset w0, eq
	strb w0, [x20]
//     i := i + 1
	add w0, w22, #1
	str w0, [x21]
	b .L2
.L4:
//   print_num(i);
	ldr x0, =_i
	ldr w0, [x0]
	bl print_num
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

	.comm _i, 4, 4
	.comm _found, 1, 4
	.section .rodata
g1:
	.byte 97, 98, 114, 97, 99, 97, 100, 97, 98, 114
	.byte 97
	.byte 0
// End
