// picoPascal compiler output
	.global pmain

// proc fib(n: integer): integer;
	.section .text
_fib:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   return fib1(n, id)
	add x20, fp, #16
	mov x2, x20
	ldr x1, =_id
	ldr w0, [fp, #32]
	mov x19, x20
	bl _fib1
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

//   proc fib1(n: integer; proc k(r: integer): integer) : integer;
_fib1:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     if n <= 1 then 
	ldr w0, [fp, #32]
	cmp w0, #1
	bgt .L4
//       return k(1) 
	mov w0, #1
	ldr x19, [fp, #48]
	ldr x1, [fp, #40]
	blr x1
	b .L2
.L4:
//       return fib1(n-1, k1)
	add x20, fp, #16
	mov x2, x20
	ldr x1, =_k1
	ldr w0, [fp, #32]
	sub w0, w0, #1
	ldr x19, [x20]
	bl _fib1
.L2:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #32
	ret
	.pool

//     proc k1(r1: integer): integer;
_k1:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     begin return fib1(n-2, k2) end;
	add x20, fp, #16
	ldr x21, [x20]
	mov x2, x20
	ldr x1, =_k2
	ldr w0, [x21, #16]
	sub w0, w0, #2
	ldr x19, [x21]
	bl _fib1
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

//       proc k2(r2: integer): integer; begin return k(r1 + r2) end;
_k2:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//       proc k2(r2: integer): integer; begin return k(r1 + r2) end;
	ldr x20, [fp, #16]
	ldr x0, [x20]
	ldr w1, =24
	add x21, x0, w1, SXTW
	ldr w0, [x20, #32]
	ldr w1, [fp, #48]
	add w0, w0, w1
	ldr x19, [x21, #8]
	ldr x1, [x21]
	blr x1
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

//   proc id(r: integer): integer; begin return r end;
_id:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   proc id(r: integer): integer; begin return r end;
	ldr w0, [fp, #32]
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(fib(6)); newline()
	mov w0, #6
	bl _fib
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
