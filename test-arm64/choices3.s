// picoPascal compiler output
	.global pmain

// proc cons(head: char; tail: list): list;
	.section .text
_cons:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   new(p);
	mov w0, #16
	bl new
	mov x20, x0
//   p^.head := head; p^.tail := tail;
	ldrb w0, [fp, #32]
	strb w0, [x20]
	ldr x0, [fp, #40]
	str x0, [x20, #8]
//   return p
	mov x0, x20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc print(p: list);
_print:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   q := p;
	ldr x20, [fp, #32]
.L3:
//   while q <> nil do
	cbz x20, .L2
//     print_char(q^.head);
	ldrb w0, [x20]
	bl print_char
//     q := q^.tail
	ldr x20, [x20, #8]
	b .L3
.L2:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc choose(k, n: integer; suffix: list);
_choose:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if k <= n then
	ldr w20, [fp, #48]
	ldr w0, [fp, #56]
	cmp w20, w0
	bgt .L6
//     if k = 0 then
	cbnz w20, .L11
//       print(suffix); newline()
	ldr x0, [fp, #64]
	bl _print
	bl newline
	b .L6
.L11:
//       choose(k, n-1, suffix);
	add x20, fp, #64
	ldr x2, [x20]
	ldr w0, [fp, #56]
	sub w1, w0, #1
	ldr w0, [fp, #48]
	bl _choose
//       choose(k-1, n-1, cons(letters[n-1], suffix))
	ldr w21, [fp, #56]
	ldr x1, [x20]
	ldr x0, =g1
	add x0, x0, w21, SXTW
	ldrb w0, [x0, #-1]
	bl _cons
	mov x2, x0
	sub w1, w21, #1
	ldr w0, [fp, #48]
	sub w0, w0, #1
	bl _choose
.L6:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #32
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   choose(3, 6, nil)
	mov x2, xzr
	mov w1, #6
	mov w0, #3
	bl _choose
	ldp fp, lr, [sp], #16
	ret
	.pool

	.section .rodata
g1:
	.byte 97, 98, 99, 100, 101, 102
	.byte 0
// End
