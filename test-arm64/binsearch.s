// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   y := 200000000;
	ldr w0, =200000000
	ldr x1, =_y
	str w0, [x1]
//   a := 0;
	ldr x0, =_a
	str wzr, [x0]
//   b := 20000;
	ldr w0, =20000
	ldr x1, =_b
	str w0, [x1]
.L2:
//   while a+1 < b do
	ldr x20, =_a
	ldr w21, [x20]
	ldr x0, =_b
	ldr w22, [x0]
	add w0, w21, #1
	cmp w0, w22
	bge .L4
//     m := (a+b) div 2;
	add w0, w21, w22
	asr w21, w0, #1
	ldr x0, =_m
	str w21, [x0]
//     if m*m <= y then
	mul w0, w21, w21
	ldr x1, =_y
	ldr w1, [x1]
	cmp w0, w1
	bgt .L6
//       a := m
	str w21, [x20]
	b .L2
.L6:
//       b := m
	ldr x0, =_m
	ldr w0, [x0]
	ldr x1, =_b
	str w0, [x1]
	b .L2
.L4:
//   print_num(a); newline()
	ldr x0, =_a
	ldr w0, [x0]
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

	.comm _y, 4, 4
	.comm _a, 4, 4
	.comm _b, 4, 4
	.comm _m, 4, 4
// End
