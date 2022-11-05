// picoPascal compiler output
	.global pmain

// proc fac(n: integer; proc k(r: integer): integer): integer;
	.section .text
_fac:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if n = 0 then return k(1) else return fac(n-1, k1) end
	ldr w0, [fp, #32]
	cbnz w0, .L3
	mov w0, #1
	ldr x19, [fp, #48]
	ldr x1, [fp, #40]
	blr x1
	b .L1
.L3:
	add x2, fp, #16
	ldr x1, =_k1
	ldr w0, [fp, #32]
	sub w0, w0, #1
	bl _fac
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #32
	ret
	.pool

//   proc k1(r: integer): integer;
_k1:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     r1 := n * r;
	add x21, fp, #16
	ldr x0, [x21]
	ldr w22, [x0, #16]
	ldr w0, [fp, #48]
	mul w20, w22, w0
//     print_num(n); print_string(" * "); print_num(r);
	mov x0, x22
	bl print_num
	mov w1, #4
	ldr x0, =g1
	bl print_string
	ldr w0, [fp, #48]
	bl print_num
//     print_string(" = "); print_num(r1); newline();
	mov w1, #4
	ldr x0, =g2
	bl print_string
	mov w0, w20
	bl print_num
	bl newline
//     return k(r1) 
	ldr x0, [x21]
	ldr w1, =24
	add x21, x0, w1, SXTW
	mov w0, w20
	ldr x19, [x21, #8]
	ldr x1, [x21]
	blr x1
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc id(r: integer): integer;
_id:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   return r 
	ldr w0, [fp, #16]
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(fac(10, id));
	mov x2, xzr
	ldr x1, =_id
	mov w0, #10
	bl _fac
	bl print_num
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

	.section .rodata
g1:
	.byte 32, 42, 32
	.byte 0
g2:
	.byte 32, 61, 32
	.byte 0
// End
