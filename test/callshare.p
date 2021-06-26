(* Identical function calls are not merged by CSE *)

var x, y: integer;

proc f(n: integer): integer;
begin x := x + n; return x end;

begin
  x := 2;
  y := f(3) + 1;
  y := f(3) + 1;
  print_num(x); newline()
end.

(*<<
8
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc f(n: integer): integer;
	.section .text
_f:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@ begin x := x + n; return x end;
	ldr r5, =_x
	ldr r0, [r5]
	ldr r1, [fp, #24]
	add r6, r0, r1
	str r6, [r5]
	mov r0, r6
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   x := 2;
	ldr r5, =_x
	mov r0, #2
	str r0, [r5]
@   y := f(3) + 1;
	mov r0, #3
	bl _f
	ldr r6, =_y
	add r0, r0, #1
	str r0, [r6]
@   y := f(3) + 1;
	mov r0, #3
	bl _f
	add r0, r0, #1
	str r0, [r6]
@   print_num(x); newline()
	ldr r0, [r5]
	bl print_num
	bl newline
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

	.comm _x, 4, 4
	.comm _y, 4, 4
@ End
]]*)
