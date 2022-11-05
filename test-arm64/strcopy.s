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
//   while in[i] <> '*' do
	ldr x20, =_i
	ldr w21, [x20]
	ldr x0, =g1
	ldrb w22, [x0, w21, SXTW]
	cmp w22, #42
	beq .L4
//     out[i] := in[i];
	ldr x0, =_out
	strb w22, [x0, w21, SXTW]
//     i := i + 1
	ldr w0, [x20]
	add w0, w0, #1
	str w0, [x20]
	b .L2
.L4:
//   out[i] := chr(0);
	ldr x20, =_out
	ldr x0, =_i
	ldr w0, [x0]
	strb wzr, [x20, w0, SXTW]
//   print_string(out); newline()
	mov w1, #128
	mov x0, x20
	bl print_string
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

	.comm _out, 128, 4
	.comm _i, 4, 4
	.section .rodata
g1:
	.byte 72, 101, 108, 108, 111, 44, 32, 119, 111, 114
	.byte 108, 100, 33, 42
	.byte 0
// End
