proc f(var x: integer);
begin
  x := 37
end;

var i: integer; a: array 10 of integer;

begin
  i := 3;
  f(a[i]);
  print_num(a[3]); newline()
end.

(*<<
37
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc f(var x: integer);
	.section .text
_f:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   x := 37
	mov r0, #37
	ldr r1, [fp, #16]
	str r0, [r1]
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   i := 3;
	mov r5, #3
	ldr r0, =_i
	str r5, [r0]
@   f(a[i]);
	ldr r6, =_a
	add r0, r6, r5, LSL #2
	bl _f
@   print_num(a[3]); newline()
	ldr r0, [r6, #12]
	bl print_num
	bl newline
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

	.comm _i, 4, 4
	.comm _a, 40, 4
@ End
]]*)
