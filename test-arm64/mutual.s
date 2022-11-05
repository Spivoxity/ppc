// picoPascal compiler output
	.global pmain

// proc flip(i: integer): integer;
	.section .text
_flip:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if i = 0 then 
	ldr w0, [fp, #32]
	cbnz w0, .L3
//     r := 1
	mov w20, #1
	b .L4
.L3:
//     r := 2 * flop(i-1)
	ldr w0, [fp, #32]
	sub w0, w0, #1
	bl _flop
	lsl w20, w0, #1
.L4:
//   print_string("flip("); print_num(i); 
	mov w1, #6
	ldr x0, =g1
	bl print_string
	ldr w0, [fp, #32]
	bl print_num
//   print_string(") = "); print_num(r);
	mov w1, #5
	ldr x0, =g2
	bl print_string
	mov w0, w20
	bl print_num
//   newline();
	bl newline
//   return r
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc flop(i: integer): integer;
_flop:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if i = 0 then 
	ldr w0, [fp, #32]
	cbnz w0, .L7
//     r := 1
	mov w20, #1
	b .L8
.L7:
//     r := flip(i-1) + k
	ldr w0, [fp, #32]
	sub w0, w0, #1
	bl _flip
	add w20, w0, #5
.L8:
//   print_string("flop("); print_num(i); 
	mov w1, #6
	ldr x0, =g3
	bl print_string
	ldr w0, [fp, #32]
	bl print_num
//   print_string(") = "); print_num(r);
	mov w1, #5
	ldr x0, =g4
	bl print_string
	mov w0, w20
	bl print_num
//   newline();
	bl newline
//   return r
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(flip(5));
	mov w0, #5
	bl _flip
	bl print_num
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

	.section .rodata
g1:
	.byte 102, 108, 105, 112, 40
	.byte 0
g2:
	.byte 41, 32, 61, 32
	.byte 0
g3:
	.byte 102, 108, 111, 112, 40
	.byte 0
g4:
	.byte 41, 32, 61, 32
	.byte 0
// End
