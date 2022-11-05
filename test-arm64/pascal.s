// picoPascal compiler output
	.global pmain

// proc pascal2();
	.section .text
_pascal2:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #448
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0;
	mov w20, wzr
.L2:
//   while i < n do
	cmp w20, #10
	bge .L1
//     a[i][0] := 1; j := 1;
	mov w22, #1
	add x0, fp, #24
	mov w1, #44
	mul w1, w20, w1
	str w22, [x0, w1, SXTW]
	mov w21, #1
//     print_num(a[i][0]);
	mov x0, x22
	bl print_num
.L5:
//     while j <= i do
	cmp w21, w20
	bgt .L7
//       a[i][j] := a[i-1][j-1] + a[i-1][j];
	add x22, fp, #24
	mov w0, #44
	mul w0, w20, w0
	add x23, x22, w0, SXTW
	add x24, x23, #-44
	add x0, x24, w21, SXTW 2
	ldr w0, [x0, #-4]
	ldr w1, [x24, w21, SXTW #2]
	add w0, w0, w1
	str w0, [x23, w21, SXTW #2]
//       print_char(' '); print_num(a[i][j]);
	mov w0, #32
	bl print_char
	mov w0, #44
	mul w0, w20, w0
	add x0, x22, w0, SXTW
	ldr w0, [x0, w21, SXTW #2]
	bl print_num
//       j := j+1
	add w21, w21, #1
	b .L5
.L7:
//     a[i][i+1] := 0;
	add x0, fp, #24
	mov w1, #44
	mul w1, w20, w1
	add x0, x0, w1, SXTW
	add x0, x0, w20, SXTW 2
	str wzr, [x0, #4]
//     newline();
	bl newline
//     i := i+1
	add w20, w20, #1
	b .L2
.L1:
	ldp fp, lr, [sp], #16
	add sp, sp, #448
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   pascal2()
	bl _pascal2
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
