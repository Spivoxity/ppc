// picoPascal compiler output
	.global pmain

// proc foo();
	.section .text
_foo:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #48
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_string("foo"); newline();
	mov w1, #4
	ldr x0, =g1
	bl print_string
	bl newline
//   j := 2; b[0] := 1; b[1] := 1;
	mov w20, #2
	mov w0, #1
	str w0, [fp, #24]
	mov w0, #1
	str w0, [fp, #28]
.L2:
//   while 10 > j do
	cmp w20, #10
	bge .L4
//     b[j] := b[j-2] + b[j-1];
	add x21, fp, #24
	add x22, x21, w20, SXTW 2
	ldr w0, [x22, #-8]
	ldr w1, [x22, #-4]
	add w0, w0, w1
	str w0, [x21, w20, SXTW #2]
//     print_char(' '); print_num(b[j]);
	mov w0, #32
	bl print_char
	ldr w0, [x21, w20, SXTW #2]
	bl print_num
//     j := 1+j
	add w20, w20, #1
	b .L2
.L4:
//   newline();
	bl newline
	ldp fp, lr, [sp], #16
	add sp, sp, #48
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

pmain:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_string("baz"); newline();
	mov w1, #4
	ldr x0, =g2
	bl print_string
	bl newline
//   i := 2; a[0] := 1; a[1] := 1;
	mov w0, #2
	ldr x1, =_i
	str w0, [x1]
	ldr x20, =_a
	mov w0, #1
	str w0, [x20]
	mov w0, #1
	str w0, [x20, #4]
.L6:
//   while i < 10 do
	ldr x20, =_i
	ldr w21, [x20]
	cmp w21, #10
	bge .L8
//     a[i] := a[i-2] + a[i-1];
	ldr x22, =_a
	add x23, x22, w21, SXTW 2
	ldr w0, [x23, #-8]
	ldr w1, [x23, #-4]
	add w0, w0, w1
	str w0, [x22, w21, SXTW #2]
//     print_char(' '); print_num(a[i]);
	mov w0, #32
	bl print_char
	ldr w0, [x20]
	ldr w0, [x22, w0, SXTW #2]
	bl print_num
//     i := i+1
	ldr w0, [x20]
	add w0, w0, #1
	str w0, [x20]
	b .L6
.L8:
//   newline();
	bl newline
//   foo()
	bl _foo
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

	.comm _i, 4, 4
	.comm _a, 40, 4
	.section .rodata
g1:
	.byte 102, 111, 111
	.byte 0
g2:
	.byte 98, 97, 122
	.byte 0
// End
