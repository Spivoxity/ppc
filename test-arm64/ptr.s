// picoPascal compiler output
	.global pmain

// proc build(n: integer): tree;
	.section .text
_build:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if n <= 1 then
	ldr w0, [fp, #32]
	cmp w0, #1
	bgt .L3
//     return nil
	mov x0, xzr
	b .L1
.L3:
//     new(t);
	mov w0, #16
	bl new
	mov x20, x0
//     t^.left := build(n-2);
	ldr w0, [fp, #32]
	sub w0, w0, #2
	bl _build
	str x0, [x20]
//     t^.right := build(n-1);
	ldr w0, [fp, #32]
	sub w0, w0, #1
	bl _build
	str x0, [x20, #8]
//     return t
	mov x0, x20
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc print(t: tree);
_print:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   tt := t;
	ldr x20, [fp, #32]
//   if tt = nil then
	cbnz x20, .L7
//     print_char('.')
	mov w0, #46
	bl print_char
	b .L5
.L7:
//     print_char('(');
	mov w0, #40
	bl print_char
//     print(tt^.left);
	ldr x0, [x20]
	bl _print
//     print(tt^.right);
	ldr x0, [x20, #8]
	bl _print
//     print_char(')')
	mov w0, #41
	bl print_char
.L5:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc count(t: tree): integer;
_count:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   tt := t;
	ldr x20, [fp, #48]
//   if tt = nil then
	cbnz x20, .L11
//     return 1
	mov w0, #1
	b .L9
.L11:
//     return count(tt^.left) + count(tt^.right)
	ldr x0, [x20]
	bl _count
	mov x21, x0
	ldr x0, [x20, #8]
	bl _count
	add w0, w21, w0
.L9:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   for n := 0 to 7 do
	ldr x0, =_n
	str wzr, [x0]
	mov w20, #7
.L14:
	ldr x21, =_n
	ldr w22, [x21]
	cmp w22, w20
	bgt .L13
//     p := build(n);
	mov x0, x22
	bl _build
	ldr x22, =_p
	str x0, [x22]
//     print_num(count(p)); print_char(' ');
	bl _count
	bl print_num
	mov w0, #32
	bl print_char
//     print(p); newline()
	ldr x0, [x22]
	bl _print
	bl newline
	ldr w0, [x21]
	add w0, w0, #1
	str w0, [x21]
	b .L14
.L13:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

	.comm _n, 4, 4
	.comm _p, 8, 4
// End
