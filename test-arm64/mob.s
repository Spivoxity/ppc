// picoPascal compiler output
	.global pmain

// proc A(k: integer; proc x1(): integer; proc x2(): integer; 
	.section .text
_A:
	stp x6, x7, [sp, -16]!
	stp x4, x5, [sp, -16]!
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if k <= 0 then return x4() + x5() else return B() end
	ldr w0, [fp, #32]
	cmp w0, #0
	bgt .L3
	ldr x19, [fp, #96]
	ldr x0, [fp, #88]
	blr x0
	ldr x19, [fp, #112]
	mov x20, x0
	ldr x0, [fp, #104]
	blr x0
	add w0, w20, w0
	b .L1
.L3:
	add x19, fp, #16
	bl _B
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #64
	ret
	.pool

//   proc B(): integer;
_B:
	stp x25, x26, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
	sub sp, sp, #32
//     k := k-1;
	add x20, fp, #16
	ldr x21, [x20]
	ldr w22, =16
	ldr w0, [x21, w22, SXTW]
	sub w0, w0, #1
	str w0, [x21, w22, SXTW]
//     return A(k, B, x1, x2, x3, x4)
	ldr x20, [x20]
	ldr w0, =24
	add x21, x20, w0, SXTW
	ldr w0, =40
	add x23, x20, w0, SXTW
	ldr w0, =56
	add x24, x20, w0, SXTW
	ldr w0, =72
	add x25, x20, w0, SXTW
	ldr x0, [x25, #8]
	str x0, [sp, #16]
	ldr x0, [x25]
	str x0, [sp, #8]
	ldr x0, [x24, #8]
	str x0, [sp]
	ldr x7, [x24]
	ldr x6, [x23, #8]
	ldr x5, [x23]
	ldr x4, [x21, #8]
	ldr x3, [x21]
	mov x2, x20
	ldr x1, =_B
	ldr w0, [x20, w22, SXTW]
	bl _A
	mov sp, fp
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ldp x25, x26, [sp], #16
	ret
	.pool

// proc One(): integer; begin return 1 end;
_One:
	stp fp, lr, [sp, -16]!
	mov fp, sp
// proc One(): integer; begin return 1 end;
	mov w0, #1
	ldp fp, lr, [sp], #16
	ret
	.pool

// proc MOne(): integer; begin return -1 end;
_MOne:
	stp fp, lr, [sp, -16]!
	mov fp, sp
// proc MOne(): integer; begin return -1 end;
	mov w0, #-1
	ldp fp, lr, [sp], #16
	ret
	.pool

// proc Zero(): integer; begin return 0 end;
_Zero:
	stp fp, lr, [sp, -16]!
	mov fp, sp
// proc Zero(): integer; begin return 0 end;
	mov w0, wzr
	ldp fp, lr, [sp], #16
	ret
	.pool

pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
	sub sp, sp, #32
//   print_num(A(10, One, MOne, MOne, One, Zero)); newline()
	ldr x20, =_One
	ldr x21, =_MOne
	str xzr, [sp, #16]
	ldr x0, =_Zero
	str x0, [sp, #8]
	str xzr, [sp]
	mov x7, x20
	mov x6, xzr
	mov x5, x21
	mov x4, xzr
	mov x3, x21
	mov x2, xzr
	mov x1, x20
	mov w0, #10
	bl _A
	bl print_num
	bl newline
	mov sp, fp
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// End
