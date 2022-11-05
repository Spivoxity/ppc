// picoPascal compiler output
	.global pmain

// proc f(x: integer): integer;
	.section .text
_f:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
// begin return x end;
	ldr w0, [fp, #16]
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc mmm(x: integer): integer;
_mmm:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   y := f(x); 
	ldr w0, [fp, #16]
	bl _f
	ldr x1, =_y
	str w0, [x1]
//   return y*y 
	mul w0, w0, w0
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := 3;
	mov w20, #3
	ldr x0, =_x
	str w20, [x0]
//   y := 5;
	mov w21, #5
	ldr x0, =_y
	str w21, [x0]
//   z := x * y;
	mul w20, w20, w21
	ldr x0, =_z
	str w20, [x0]
//   print_num(z);
	mov x0, x20
	bl print_num
//   newline();
	bl newline
//   print_num(mmm(12));
	mov w0, #12
	bl _mmm
	bl print_num
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

	.comm _x, 4, 4
	.comm _y, 4, 4
	.comm _z, 4, 4
// End
