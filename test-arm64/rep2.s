// picoPascal compiler output
	.global pmain

// proc inc(var x: integer): integer;
	.section .text
_inc:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := x+1;
	add x20, fp, #48
	ldr x21, [x20]
	ldr w0, [x21]
	add w0, w0, #1
	str w0, [x21]
//   return x
	ldr x0, [x20]
	ldr w0, [x0]
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
//   i := 0;
	ldr x0, =_i
	str wzr, [x0]
.L3:
//   repeat until inc(i) > 10;
	ldr x20, =_i
	mov x0, x20
	bl _inc
	cmp w0, #10
	ble .L3
//   print_num(i); newline()
	ldr w0, [x20]
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

	.comm _i, 4, 4
// End
