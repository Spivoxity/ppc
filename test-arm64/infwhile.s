// picoPascal compiler output
	.global pmain

// proc foo(): integer;
	.section .text
_foo:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 3;
	mov w20, #3
.L2:
//     i := i + 2;
	add w20, w20, #2
//     if i > 10 then return i end;
	cmp w20, #10
	ble .L2
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(foo()); newline()
	bl _foo
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
