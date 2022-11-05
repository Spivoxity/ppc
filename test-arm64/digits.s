// picoPascal compiler output
	.global pmain

// proc search(k, n: integer; proc avail(x: integer): boolean);
	.section .text
_search:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #16
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if k = 9 then
	ldr w0, [fp, #64]
	cmp w0, #9
	bne .L3
//     print_num(n); newline()
	ldr w0, [fp, #72]
	bl print_num
	bl newline
	b .L1
.L3:
//     d := 1;
	mov w0, #1
	str w0, [fp, #28]
.L5:
//     while d < 10 do
	ldr w21, [fp, #28]
	cmp w21, #10
	bge .L1
//       n1 := 10 * n + d;
	ldr w0, [fp, #72]
	mov w1, #10
	mul w0, w0, w1
	add w20, w0, w21
//       if (n1 mod (k+1) = 0) and avail(d) then
	ldr w0, [fp, #64]
	add w1, w0, #1
	mov w0, w20
	bl int_mod
	cbnz w0, .L10
	ldr w0, [fp, #28]
	ldr x19, [fp, #88]
	ldr x1, [fp, #80]
	blr x1
	cbz w0, .L10
//         search(k+1, n1, avail1)
	add x3, fp, #32
	ldr x2, =_avail1
	mov w1, w20
	ldr w0, [fp, #64]
	add w0, w0, #1
	bl _search
.L10:
//       d := d+1
	ldr w0, [fp, #28]
	add w0, w0, #1
	str w0, [fp, #28]
	b .L5
.L1:
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #32
	ret
	.pool

//   proc avail1(x: integer): boolean;
_avail1:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     if x <> d then 
	ldr w20, [fp, #48]
	ldr x21, [fp, #16]
	ldr w0, [x21, #-4]
	cmp w20, w0
	beq .L14
//       return avail(x)
	ldr w0, =48
	add x21, x21, w0, SXTW
	mov x0, x20
	ldr x19, [x21, #8]
	ldr x1, [x21]
	blr x1
	b .L12
.L14:
//       return false
	mov w0, wzr
.L12:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc avail0(x: integer): boolean;
_avail0:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   return true
	mov w0, #1
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   search(0, 0, avail0)
	mov x3, xzr
	ldr x2, =_avail0
	mov w1, wzr
	mov w0, wzr
	bl _search
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
