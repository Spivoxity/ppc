// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := 3 * 37; y := 5 * 37;
	mov w0, #111
	ldr x1, =_x
	str w0, [x1]
	mov w0, #185
	ldr x1, =_y
	str w0, [x1]
.L2:
//   while x <> y do
	ldr x20, =_x
	ldr w21, [x20]
	ldr x0, =_y
	ldr w22, [x0]
	cmp w21, w22
	beq .L4
//     if x > y then
	cmp w21, w22
	ble .L6
//       x := x - y
	sub w0, w21, w22
	str w0, [x20]
	b .L2
.L6:
//       y := y - x
	ldr x20, =_y
	ldr w0, [x20]
	ldr x1, =_x
	ldr w1, [x1]
	sub w0, w0, w1
	str w0, [x20]
	b .L2
.L4:
//   print_num(x); newline()
	ldr x0, =_x
	ldr w0, [x0]
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

	.comm _x, 4, 4
	.comm _y, 4, 4
// End
