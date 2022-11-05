// picoPascal compiler output
	.global pmain

// proc foo(a, b, c, d: integer): integer;
	.section .text
_foo:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x25, x26, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   return
	ldr w20, [fp, #80]
	ldr w21, [fp, #88]
	sub w22, w20, w21
	ldr w23, [fp, #96]
	sub w24, w20, w23
	ldr w25, [fp, #104]
	sub w20, w20, w25
	sub w26, w21, w23
	sub w21, w21, w25
	sub w23, w23, w25
	mul w0, w22, w24
	mul w0, w0, w20
	mul w0, w0, w26
	mul w0, w0, w21
	mul w0, w0, w23
	mul w1, w24, w20
	mul w1, w1, w26
	mul w1, w1, w21
	mul w1, w1, w23
	mul w1, w1, w22
	add w0, w0, w1
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ldp x25, x26, [sp], #16
	add sp, sp, #32
	ret
	.pool

// proc baz(n: integer): integer;
_baz:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   return (n-1)*(n-1)
	ldr w0, [fp, #32]
	sub w20, w0, #1
	mul w0, w20, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(foo(1,2,3,4)); newline();
	mov w3, #4
	mov w2, #3
	mov w1, #2
	mov w0, #1
	bl _foo
	bl print_num
	bl newline
//   print_num(baz(10)); newline()
	mov w0, #10
	bl _baz
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
