// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0;
	ldr x0, =_i
	str wzr, [x0]
.L2:
//   while i < 10 do
	ldr x0, =_i
	ldr w20, [x0]
	cmp w20, #10
	bge .L1
//     case i of
	sub w0, w20, #1
	cmp w0, #8
	bhs .L5
	adr ip0, .L13
	ldr ip0, [ip0, w0, SXTW #3]
	br ip0
	.p2align 3
.L13:
	.quad .L7
	.quad .L10
	.quad .L8
	.quad .L5
	.quad .L9
	.quad .L11
	.quad .L5
	.quad .L12
.L7:
//         i := i + 1;
	ldr x20, =_i
	ldr w0, [x20]
	add w21, w0, #1
	str w21, [x20]
// 	i := i + 2
	add w0, w21, #2
	str w0, [x20]
	b .L6
.L8:
//         i := i + 1;
	ldr x20, =_i
	ldr w0, [x20]
	add w21, w0, #1
	str w21, [x20]
// 	i := i + 2
	add w0, w21, #2
	str w0, [x20]
	b .L6
.L9:
//         i := i + 1;
	ldr x20, =_i
	ldr w0, [x20]
	add w21, w0, #1
	str w21, [x20]
// 	i := i + 2
	add w0, w21, #2
	str w0, [x20]
	b .L6
.L10:
//         i := i - 1;
	ldr x20, =_i
	ldr w0, [x20]
	sub w0, w0, #1
	str w0, [x20]
	b .L6
.L11:
//         i := i - 1;
	ldr x20, =_i
	ldr w0, [x20]
	sub w0, w0, #1
	str w0, [x20]
	b .L6
.L12:
//         i := i + 2;
	ldr x20, =_i
	ldr w0, [x20]
	add w0, w0, #2
	str w0, [x20]
	b .L6
.L5:
//       i := i + 1
	ldr x20, =_i
	ldr w0, [x20]
	add w0, w0, #1
	str w0, [x20]
.L6:
//     print_num(i); newline()
	ldr x0, =_i
	ldr w0, [x0]
	bl print_num
	bl newline
	b .L2
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

	.comm _i, 4, 4
// End
