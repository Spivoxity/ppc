// picoPascal compiler output
	.global pmain

// proc sum(p: ptr): integer;
	.section .text
_sum:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   q := p; s := 0;
	ldr x20, [fp, #48]
	mov w21, wzr
.L2:
//   while q <> nil do
	cbz x20, .L4
//     s := s + q^.data;
	ldr w0, [x20]
	add w21, w21, w0
//     q := q^.next
	ldr x20, [x20, #8]
	b .L2
.L4:
//   return s
	mov w0, w21
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc main();
_main:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0;
	mov w20, wzr
.L6:
//   while input[i] <> '0' do i := i+1 end;
	ldr x0, =g1
	ldrb w0, [x0, w20, SXTW]
	cmp w0, #48
	beq .L8
	add w20, w20, #1
	b .L6
.L8:
//   p := nil;
	mov x21, xzr
.L9:
//   while i > 0 do
	cmp w20, #0
	ble .L11
//     i := i-1;
	sub w20, w20, #1
//     q := p;
	mov x22, x21
//     new(p);
	mov w0, #16
	bl new
	mov x21, x0
//     p^.data := ord(input[i]) - ord('0');
	ldr x0, =g1
	ldrb w0, [x0, w20, SXTW]
	sub w0, w0, #48
	str w0, [x21]
//     p^.next := q
	str x22, [x21, #8]
	b .L9
.L11:
//   print_num(sum(p)); newline()
	mov x0, x21
	bl _sum
	bl print_num
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

	.section .rodata
g1:
	.byte 51, 49, 52, 49, 53, 57, 50, 54, 53, 48
	.byte 0
// End
