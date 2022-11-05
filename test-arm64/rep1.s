// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0;
	ldr x0, =_i
	str wzr, [x0]
.L2:
//     j := 1;
	mov w0, #1
	ldr x1, =_j
	str w0, [x1]
.L4:
//       j := j+1; k := k+1; 
	ldr x20, =_j
	ldr w0, [x20]
	add w21, w0, #1
	str w21, [x20]
	ldr x20, =_k
	ldr w0, [x20]
	add w22, w0, #1
	str w22, [x20]
//     until j > i;
	ldr x20, =_i
	ldr w23, [x20]
	cmp w21, w23
	ble .L4
//     i := i+1
	add w21, w23, #1
	str w21, [x20]
	cmp w21, #10
	ble .L2
//   print_num(k); newline()
	mov x0, x22
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

	.comm _i, 4, 4
	.comm _j, 4, 4
	.comm _k, 4, 4
// End
