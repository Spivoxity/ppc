// picoPascal compiler output
	.global pmain

// proc swap(i, j: integer);
	.section .text
_swap:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   t := a[i]; 
	ldr x21, =_a
	ldr w22, [fp, #64]
	ldr w20, [x21, w22, SXTW #2]
//   a[i] := a[j]; 
	ldr w23, [fp, #72]
	ldr w0, [x21, w23, SXTW #2]
	str w0, [x21, w22, SXTW #2]
//   a[j] := t
	str w20, [x21, w23, SXTW #2]
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc main();
_main:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   for i := 0 to n-1 do a[i] := i end;
	mov w20, wzr
	mov w21, #9
.L3:
	cmp w20, w21
	bgt .L4
	ldr x0, =_a
	str w20, [x0, w20, SXTW #2]
	add w20, w20, #1
	b .L3
.L4:
//   swap(3, 6);
	mov w1, #6
	mov w0, #3
	bl _swap
//   for i := 0 to n-1 do print_num(a[i]) end;
	mov w20, wzr
	mov w22, #9
.L5:
	cmp w20, w22
	bgt .L6
	ldr x0, =_a
	ldr w0, [x0, w20, SXTW #2]
	bl print_num
	add w20, w20, #1
	b .L5
.L6:
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   main()
	bl _main
	ldp fp, lr, [sp], #16
	ret
	.pool

	.comm _a, 40, 4
// End
