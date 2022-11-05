// picoPascal compiler output
	.global pmain

// proc one(var x: integer);
	.section .text
_one:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   two()
	add x19, fp, #16
	bl _two
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

//   proc two(); begin x := x+1 end;
_two:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   proc two(); begin x := x+1 end;
	ldr x0, [fp, #16]
	ldr x20, [x0, #16]
	ldr w0, [x20]
	add w0, w0, #1
	str w0, [x20]
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

// proc three();
_three:
	sub sp, sp, #16
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   y := 36;
	mov w0, #36
	str w0, [fp, #28]
//   one(y);
	add x0, fp, #28
	bl _one
//   print_num(y);
	ldr w0, [fp, #28]
	bl print_num
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   three()
	bl _three
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
