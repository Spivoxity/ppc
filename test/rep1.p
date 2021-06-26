var i, j, k: integer;
begin
  i := 0;
  repeat
    j := 1;
    repeat
      j := j+1; k := k+1; 
    until j > i;
    i := i+1
  until i > 10;
  print_num(k); newline()
end.

(*<<
56
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.section .text
pmain:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   i := 0;
	mov r0, #0
	ldr r1, =_i
	str r0, [r1]
.L2:
@     j := 1;
	mov r0, #1
	ldr r1, =_j
	str r0, [r1]
.L4:
@       j := j+1; k := k+1; 
	ldr r5, =_j
	ldr r0, [r5]
	add r6, r0, #1
	str r6, [r5]
	ldr r5, =_k
	ldr r0, [r5]
	add r7, r0, #1
	str r7, [r5]
@     until j > i;
	ldr r5, =_i
	ldr r8, [r5]
	cmp r6, r8
	ble .L4
@     i := i+1
	add r6, r8, #1
	str r6, [r5]
	cmp r6, #10
	ble .L2
@   print_num(k); newline()
	mov r0, r7
	bl print_num
	bl newline
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

	.comm _i, 4, 4
	.comm _j, 4, 4
	.comm _k, 4, 4
@ End
]]*)
