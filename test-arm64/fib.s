// picoPascal compiler output
	.global pmain

// proc fib(n: integer): integer;
	.section .text
_fib:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if n <= 1 then 
	ldr w0, [fp, #32]
	cmp w0, #1
	bgt .L3
//     return 1 
	mov w0, #1
	b .L1
.L3:
//     return fib(n-1) + fib(n-2)
	ldr w0, [fp, #32]
	sub w0, w0, #1
	bl _fib
	mov x20, x0
	ldr w0, [fp, #32]
	sub w0, w0, #2
	bl _fib
	add w0, w20, w0
.L1:
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
