// picoPascal compiler output
	.global pmain

// proc gcd(u, v: integer): integer;
	.section .text
_gcd:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := u; y := v;
	ldr w20, [fp, #48]
	ldr w21, [fp, #56]
.L2:
//   while x <> y do
	cmp w20, w21
	beq .L4
//     if x < y then
	cmp w20, w21
	bge .L6
//       y := y - x
	sub w21, w21, w20
	b .L2
.L6:
//       x := x - y
	sub w20, w20, w21
	b .L2
.L4:
//   return x
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   z := gcd(3*37, 5*37);
	mov w1, #185
	mov w0, #111
	bl _gcd
	ldr x20, =_z
	str w0, [x20]
//   print_string("The final answer is calculated as ");
	mov w1, #35
	ldr x0, =g1
	bl print_string
//   print_num(z); newline()
	ldr w0, [x20]
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

	.comm _z, 4, 4
	.section .rodata
g1:
	.byte 84, 104, 101, 32, 102, 105, 110, 97, 108, 32
	.byte 97, 110, 115, 119, 101, 114, 32, 105, 115, 32
	.byte 99, 97, 108, 99, 117, 108, 97, 116, 101, 100
	.byte 32, 97, 115, 32
	.byte 0
// End
