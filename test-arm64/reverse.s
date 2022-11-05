// picoPascal compiler output
	.global pmain

// proc reverse(a: list): list;
	.section .text
_reverse:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   p := a; q := nil;
	ldr x20, [fp, #64]
	mov x21, xzr
.L2:
//   while p <> nil do
	cbz x20, .L4
//     r := p^.tail; p^.tail := q;
	ldr w0, =8
	add x23, x20, w0, SXTW
	ldr x22, [x23]
	str x21, [x23]
//     q := p; p := r
	mov x21, x20
	mov x20, x22
	b .L2
.L4:
//   return q
	mov x0, x21
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc test();
_test:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   p := nil; i := 0; 
	mov x21, xzr
	mov w20, wzr
.L6:
//   while mike[i] <> chr(0) do
	ldr x23, =g1
	ldrb w0, [x23, w20, SXTW]
	cbz w0, .L8
//     new(q);
	mov w0, #16
	bl new
	mov x22, x0
//     q^.head := mike[i];
	ldrb w0, [x23, w20, SXTW]
	strb w0, [x22]
//     q^.tail := p;
	str x21, [x22, #8]
//     p := q; i := i+1
	mov x21, x22
	add w20, w20, #1
	b .L6
.L8:
//   p := reverse(p);
	mov x0, x21
	bl _reverse
	mov x21, x0
//   q := p;
	mov x22, x21
.L9:
//   while q <> nil do
	cbz x22, .L11
//     print_char(q^.head);
	ldrb w0, [x22]
	bl print_char
//     q := q^.tail
	ldr x22, [x22, #8]
	b .L9
.L11:
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
// begin test() end.
	bl _test
	ldp fp, lr, [sp], #16
	ret
	.pool

	.section .rodata
g1:
	.byte 109, 105, 107, 101
	.byte 0
// End
