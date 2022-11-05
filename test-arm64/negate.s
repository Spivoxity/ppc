// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := true;
	mov w20, #1
	ldr x0, =_x
	strb w20, [x0]
//   y := not x;
	eor w21, w20, #1
	ldr x22, =_y
	strb w21, [x22]
//   y := not y;
	eor w21, w21, #1
	strb w21, [x22]
//   if x = y then
	cmp w20, w21
	bne .L4
//     print_string("OK"); newline()
	mov w1, #3
	ldr x0, =g1
	bl print_string
	bl newline
.L4:
//   u := 37;
	mov w20, #37
	ldr x0, =_u
	str w20, [x0]
//   v := -u;
	neg w21, w20
	ldr x22, =_v
	str w21, [x22]
//   v := -v;
	neg w21, w21
	str w21, [x22]
//   if u = v then
	cmp w20, w21
	bne .L1
//     print_string("OK2"); newline()
	mov w1, #4
	ldr x0, =g2
	bl print_string
	bl newline
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

	.comm _x, 1, 4
	.comm _y, 1, 4
	.comm _u, 4, 4
	.comm _v, 4, 4
	.section .rodata
g1:
	.byte 79, 75
	.byte 0
g2:
	.byte 79, 75, 50
	.byte 0
// End
