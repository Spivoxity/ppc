// picoPascal compiler output
	.global pmain

// proc queens(k: integer; var choice: array N of integer);
	.section .text
_queens:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if k = N then
	ldr w0, [fp, #64]
	cmp w0, #8
	bne .L3
//     print(choice)
	ldr x0, [fp, #72]
	bl _print
	b .L1
.L3:
//     y := 0;
	mov w20, wzr
.L5:
//     while y < N do
	cmp w20, #8
	bge .L1
//       j := 0; ok := true;
	mov w21, wzr
	mov w23, #1
.L8:
//       while ok and (j < k) do
	cbz w23, .L10
	ldr w24, [fp, #64]
	cmp w21, w24
	bge .L10
// 	q := choice[j];
	ldr x0, [fp, #72]
	ldr w22, [x0, w21, SXTW #2]
// 	ok := (q <> y) and (q+j <> y+k) and (q-j <> y-k);
	cmp w22, w20
	beq .L12
	add w0, w22, w21
	add w1, w20, w24
	cmp w0, w1
	beq .L12
	sub w0, w22, w21
	sub w1, w20, w24
	cmp w0, w1
	cset w24, ne
	b .L13
.L12:
	mov w24, wzr
.L13:
	mov w23, w24
//         j := j+1
	add w21, w21, #1
	b .L8
.L10:
//       if ok then 
	cbz w23, .L18
// 	choice[k] := y;
	add x24, fp, #72
	ldr x0, [x24]
	ldr w1, [fp, #64]
	str w20, [x0, w1, SXTW #2]
// 	queens(k+1, choice)
	ldr x1, [x24]
	ldr w0, [fp, #64]
	add w0, w0, #1
	bl _queens
.L18:
//       y := y+1
	add w20, w20, #1
	b .L5
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc print(var choice: array N of integer);
_print:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := 0;
	mov w20, wzr
.L20:
//   while x < N do
	cmp w20, #8
	bge .L22
//     print_num(choice[x]+1);
	ldr x0, [fp, #32]
	ldr w0, [x0, w20, SXTW #2]
	add w0, w0, #1
	bl print_num
//     x := x+1
	add w20, w20, #1
	b .L20
.L22:
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   queens(0, choice)
	ldr x1, =_choice
	mov w0, wzr
	bl _queens
	ldp fp, lr, [sp], #16
	ret
	.pool

	.comm _choice, 32, 4
// End
