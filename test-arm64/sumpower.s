// picoPascal compiler output
	.global pmain

// proc sum(a, b: integer; proc f(x: integer): integer): integer;
	.section .text
_sum:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := a; s := 0;
	ldr w20, [fp, #48]
	mov w21, wzr
.L2:
//   while i <= b do
	ldr w0, [fp, #56]
	cmp w20, w0
	bgt .L4
//     s := s + f(i);
	mov w0, w20
	ldr x19, [fp, #72]
	ldr x1, [fp, #64]
	blr x1
	add w21, w21, w0
//     i := i + 1
	add w20, w20, #1
	b .L2
.L4:
//   return s
	mov w0, w21
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #32
	ret
	.pool

// proc sum_powers(a, b, n: integer): integer;
_sum_powers:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   return sum(a, b, pow)
	add x3, fp, #16
	ldr x2, =_pow
	ldr w1, [fp, #24]
	ldr w0, [fp, #16]
	bl _sum
	ldp fp, lr, [sp], #16
	add sp, sp, #32
	ret
	.pool

//   proc pow(x: integer): integer;
_pow:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     j := 0; p := 1;
	mov w20, wzr
	mov w21, #1
.L7:
//     while j < n do
	ldr x0, [fp, #16]
	ldr w0, [x0, #16]
	cmp w20, w0
	bge .L9
//       p := p * x;
	ldr w0, [fp, #48]
	mul w21, w21, w0
//       j := j + 1
	add w20, w20, #1
	b .L7
.L9:
//     return p
	mov w0, w21
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(sum_powers(1, 10, 3));
	mov w2, #3
	mov w1, #10
	mov w0, #1
	bl _sum_powers
	bl print_num
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
