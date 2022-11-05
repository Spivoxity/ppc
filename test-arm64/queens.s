// picoPascal compiler output
	.global pmain

// proc queens(k: integer; proc choice(x: integer): integer);
	.section .text
_queens:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #16
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if k > N then
	ldr w0, [fp, #80]
	cmp w0, #8
	ble .L3
//     print(choice)
	ldr x1, [fp, #96]
	ldr x0, [fp, #88]
	bl _print
	b .L1
.L3:
//     y := 1;
	mov w0, #1
	str w0, [fp, #28]
.L5:
//     while y <= N do
	ldr w0, [fp, #28]
	cmp w0, #8
	bgt .L1
//       j := 1; ok := true;
	mov w20, #1
	mov w22, #1
.L8:
//       while ok and (j < k) do
	cbz w22, .L10
	ldr w0, [fp, #80]
	cmp w20, w0
	bge .L10
// 	q := choice(j);
	mov w0, w20
	ldr x19, [fp, #96]
	ldr x1, [fp, #88]
	blr x1
	mov w21, w0
// 	ok := (q <> y) and (q+j <> y+k) and (q-j <> y-k);
	ldr w23, [fp, #28]
	cmp w21, w23
	beq .L12
	ldr w24, [fp, #80]
	add w0, w21, w20
	add w1, w23, w24
	cmp w0, w1
	beq .L12
	sub w0, w21, w20
	sub w1, w23, w24
	cmp w0, w1
	cset w23, ne
	b .L13
.L12:
	mov w23, wzr
.L13:
	mov w22, w23
//         j := j+1
	add w20, w20, #1
	b .L8
.L10:
//       if ok then queens(k+1, choice1) end;
	cbz w22, .L18
	add x2, fp, #32
	ldr x1, =_choice1
	ldr w0, [fp, #80]
	add w0, w0, #1
	bl _queens
.L18:
//       y := y+1
	ldr w0, [fp, #28]
	add w0, w0, #1
	str w0, [fp, #28]
	b .L5
.L1:
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #32
	ret
	.pool

//   proc choice1(x: integer): integer;
_choice1:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     if x = k then
	ldr x20, [fp, #16]
	ldr w0, [fp, #32]
	ldr w1, [x20, #48]
	cmp w0, w1
	bne .L21
//       return y
	ldr w0, [x20, #-4]
	b .L19
.L21:
//       return choice(x)
	ldr x0, [fp, #16]
	ldr w1, =56
	add x20, x0, w1, SXTW
	ldr w0, [fp, #32]
	ldr x19, [x20, #8]
	ldr x1, [x20]
	blr x1
.L19:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc print(proc choice(x: integer): integer);
_print:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := 1;
	mov w20, #1
.L24:
//   while x <= N do
	cmp w20, #8
	bgt .L26
//     print_num(choice(x));
	mov w0, w20
	ldr x19, [fp, #40]
	ldr x1, [fp, #32]
	blr x1
	bl print_num
//     x := x+1
	add w20, w20, #1
	b .L24
.L26:
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc choice0(x: integer): integer;
_choice0:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   return 0
	mov w0, wzr
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   queens(1, choice0)
	mov x2, xzr
	ldr x1, =_choice0
	mov w0, #1
	bl _queens
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
