// picoPascal compiler output
	.global pmain

// proc p1(var a: integer; b: integer; var d: integer): integer;
	.section .text
_p1:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   c :=b+a;
	ldr w21, [fp, #72]
	add x22, fp, #64
	ldr x0, [x22]
	ldr w0, [x0]
	add w20, w21, w0
//   d :=b+1;
	add x23, fp, #80
	add w0, w21, #1
	ldr x1, [x23]
	str w0, [x1]
//   a :=a-b;
	ldr x21, [x22]
	ldr w0, [x21]
	ldr w1, [fp, #72]
	sub w0, w0, w1
	str w0, [x21]
//   return (a+d)*b
	ldr x0, [x22]
	ldr w0, [x0]
	ldr x1, [x23]
	ldr w1, [x1]
	add w0, w0, w1
	ldr w1, [fp, #72]
	mul w0, w0, w1
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #32
	ret
	.pool

pmain:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   a:=5; b:=2; c:=3; d:=1;
	ldr x20, =_a
	mov w0, #5
	str w0, [x20]
	ldr x21, =_b
	mov w0, #2
	str w0, [x21]
	ldr x22, =_c
	mov w0, #3
	str w0, [x22]
	mov w23, #1
	ldr x24, =_d
	str w23, [x24]
//   b := p1(b,d,a) + 1;
	mov x2, x20
	mov x1, x23
	mov x0, x21
	bl _p1
	add w0, w0, #1
	str w0, [x21]
//   print_string("A="); print_num(a);
	mov w1, #3
	ldr x0, =g1
	bl print_string
	ldr w0, [x20]
	bl print_num
//   print_string(" B="); print_num(b);
	mov w1, #4
	ldr x0, =g2
	bl print_string
	ldr w0, [x21]
	bl print_num
//   print_string(" C="); print_num(c);
	mov w1, #4
	ldr x0, =g3
	bl print_string
	ldr w0, [x22]
	bl print_num
//   print_string(" D="); print_num(d);
	mov w1, #4
	ldr x0, =g4
	bl print_string
	ldr w0, [x24]
	bl print_num
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

	.comm _a, 4, 4
	.comm _b, 4, 4
	.comm _c, 4, 4
	.comm _d, 4, 4
	.section .rodata
g1:
	.byte 65, 61
	.byte 0
g2:
	.byte 32, 66, 61
	.byte 0
g3:
	.byte 32, 67, 61
	.byte 0
g4:
	.byte 32, 68, 61
	.byte 0
// End
