var i: integer;

begin
  for i := 1 to 5 do
    print_num(i);
    newline()
  end
end.

(*<<
1
2
3
4
5
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.section .text
pmain:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   for i := 1 to 5 do
	mov r0, #1
	ldr r1, =_i
	str r0, [r1]
	mov r5, #5
.L2:
	ldr r6, =_i
	ldr r7, [r6]
	cmp r7, r5
	bgt .L1
@     print_num(i);
	mov r0, r7
	bl print_num
@     newline()
	bl newline
	ldr r0, [r6]
	add r0, r0, #1
	str r0, [r6]
	b .L2
.L1:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

	.comm _i, 4, 4
@ End
]]*)
