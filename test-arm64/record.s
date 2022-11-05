// picoPascal compiler output
	.global pmain

// proc equal(x, y: string): boolean;
	.section .text
_equal:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0;
	mov w20, wzr
.L2:
//   while i < 10 do
	cmp w20, #10
	bge .L4
//     if x[i] <> y[i] then
	ldr x0, [fp, #32]
	ldrb w0, [x0, w20, SXTW]
	ldr x1, [fp, #40]
	ldrb w1, [x1, w20, SXTW]
	cmp w0, w1
	beq .L7
//       return false
	mov w0, wzr
	b .L1
.L7:
//     i := i+1
	add w20, w20, #1
	b .L2
.L4:
//   return true
	mov w0, #1
.L1:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc store(n: string; a: integer);
_store:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   db[N].name := n;
	ldr x20, =_db
	ldr x21, =_N
	mov w2, #10
	ldr x1, [fp, #48]
	ldr w0, [x21]
	add x0, x20, w0, SXTW 4
	ldr w3, =0
	add x0, x0, w3, SXTW
	bl memcpy
//   db[N].age := a;
	ldr w0, [fp, #56]
	ldr w1, [x21]
	add x1, x20, w1, SXTW 4
	str w0, [x1, #12]
//   N := N+1
	ldr w0, [x21]
	add w0, w0, #1
	str w0, [x21]
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc recall(n: string): integer;
_recall:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 0;
	mov w20, wzr
.L10:
//   while i < N do
	ldr x0, =_N
	ldr w0, [x0]
	cmp w20, w0
	bge .L12
//     if equal(db[i].name, n) then
	ldr x21, =_db
	ldr x1, [fp, #48]
	add x0, x21, w20, SXTW 4
	ldr w2, =0
	add x0, x0, w2, SXTW
	bl _equal
	cbz w0, .L15
//       return db[i].age
	add x0, x21, w20, SXTW 4
	ldr w0, [x0, #12]
	b .L9
.L15:
//     i := i+1
	add w20, w20, #1
	b .L10
.L12:
//   return 999
	mov w0, #999
.L9:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   N := 0;
	ldr x0, =_N
	str wzr, [x0]
//   store("bill     ", 23);
	mov w1, #23
	ldr x0, =g1
	bl _store
//   store("george   ", 34);
	mov w1, #34
	ldr x0, =g2
	bl _store
//   print_num(recall("george   ")); newline();
	ldr x0, =g3
	bl _recall
	bl print_num
	bl newline
//   print_num(recall("fred     ")); newline()
	ldr x0, =g4
	bl _recall
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

	.comm _db, 320, 4
	.comm _N, 4, 4
	.section .rodata
g1:
	.byte 98, 105, 108, 108, 32, 32, 32, 32, 32
	.byte 0
g2:
	.byte 103, 101, 111, 114, 103, 101, 32, 32, 32
	.byte 0
g3:
	.byte 103, 101, 111, 114, 103, 101, 32, 32, 32
	.byte 0
g4:
	.byte 102, 114, 101, 100, 32, 32, 32, 32, 32
	.byte 0
// End
