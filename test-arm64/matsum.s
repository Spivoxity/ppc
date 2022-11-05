// picoPascal compiler output
	.global pmain

// proc sum(proc f(t: integer): integer): integer;
	.section .text
_sum:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     return f(0) + f(1) + f(2)
	add x20, fp, #64
	add x21, fp, #72
	mov w0, wzr
	ldr x19, [x21]
	ldr x1, [x20]
	blr x1
	mov x22, x0
	mov w0, #1
	ldr x19, [x21]
	ldr x1, [x20]
	blr x1
	mov x23, x0
	mov w0, #2
	ldr x19, [x21]
	ldr x1, [x20]
	blr x1
	add w1, w22, w23
	add w0, w1, w0
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc matsum(var a: matrix): integer;
_matsum:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     return sum(rowsum)
	add x1, fp, #16
	ldr x0, =_rowsum
	bl _sum
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

//     proc rowsum(i: integer): integer;
_rowsum:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//         return sum(cell)
	add x1, fp, #16
	ldr x0, =_cell
	bl _sum
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

//         proc cell(j: integer): integer;
_cell:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//         begin return a[i][j] end;
	ldr x20, [fp, #16]
	ldr x0, [x20]
	ldr x0, [x0]
	ldr w1, [x20, #16]
	mov w2, #12
	mul w1, w1, w2
	add x0, x0, w1, SXTW
	ldr w1, [fp, #32]
	ldr w0, [x0, w1, SXTW #2]
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc test();
_test:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #48
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     for i := 0 to 2 do
	mov w20, wzr
	mov w23, #2
.L6:
	cmp w20, w23
	bgt .L7
//         for j := 0 to 2 do
	mov w21, wzr
	mov w22, #2
.L8:
	cmp w21, w22
	bgt .L9
//             a[i][j] := (i+1)*(j+1)
	add w24, w21, #1
	add w0, w20, #1
	mul w0, w0, w24
	add x1, fp, #28
	mov w2, #12
	mul w2, w20, w2
	add x1, x1, w2, SXTW
	str w0, [x1, w21, SXTW #2]
	mov w21, w24
	b .L8
.L9:
	add w20, w20, #1
	b .L6
.L7:
//     print_num(matsum(a)); newline()
	add x0, fp, #28
	bl _matsum
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	add sp, sp, #48
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

// End
