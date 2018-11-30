var x, y, z, p, q: integer;

begin
  x := 2;
  print_num(lsl(lsl(3, x), lsl(x, x))+(y-z)+(p-q)); newline();
  print_num(lsl(lsl(3, 2), lsl(2, 2))); newline()
end.

(*<<
3072
3072
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.section .text
pmain:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   x := 2;
	mov r4, #2
	ldr r0, =_x
	str r4, [r0]
@   print_num(lsl(lsl(3, x), lsl(x, x))+(y-z)+(p-q)); newline();
	mov r0, #3
	lsl r0, r0, r4
	lsl r1, r4, r4
	lsl r0, r0, r1
	ldr r1, =_y
	ldr r1, [r1]
	ldr r2, =_z
	ldr r2, [r2]
	sub r1, r1, r2
	add r0, r0, r1
	ldr r1, =_p
	ldr r1, [r1]
	ldr r2, =_q
	ldr r2, [r2]
	sub r1, r1, r2
	add r0, r0, r1
	bl print_num
	bl newline
@   print_num(lsl(lsl(3, 2), lsl(2, 2))); newline()
	mov r0, #3072
	bl print_num
	bl newline
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

	.comm _x, 4, 4
	.comm _y, 4, 4
	.comm _z, 4, 4
	.comm _p, 4, 4
	.comm _q, 4, 4
@ End
]]*)
