var i: integer;

proc inc(var x: integer): integer;
begin
  x := x+1;
  return x
end;

begin
  i := 0;
  repeat until inc(i) > 10;
  print_num(i); newline()
end.

(*<<
11
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc inc(var x: integer): integer;
	.section .text
_inc:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   x := x+1;
	ldr r5, [fp, #24]
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
@   return x
	ldr r0, [fp, #24]
	ldr r0, [r0]
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   i := 0;
	mov r0, #0
	ldr r1, =_i
	str r0, [r1]
.L3:
@   repeat until inc(i) > 10;
	ldr r5, =_i
	mov r0, r5
	bl _inc
	cmp r0, #10
	ble .L3
@   print_num(i); newline()
	ldr r0, [r5]
	bl print_num
	bl newline
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

	.comm _i, 4, 4
@ End
]]*)
