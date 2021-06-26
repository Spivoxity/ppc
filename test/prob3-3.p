(* From a problem sheet *)

proc double(x: integer): integer;
begin
  return x + x
end;

proc apply3(proc f(x:integer): integer): integer;
begin
  return f(3)
end;

begin
  print_num(apply3(double));
  newline()
end.

(*<<
6
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc double(x: integer): integer;
	.section .text
_double:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   return x + x
	ldr r5, [fp, #24]
	add r0, r5, r5
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc apply3(proc f(x:integer): integer): integer;
_apply3:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   return f(3)
	mov r0, #3
	ldr r4, [fp, #20]
	ldr r1, [fp, #16]
	blx r1
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   print_num(apply3(double));
	mov r1, #0
	ldr r0, =_double
	bl _apply3
	bl print_num
@   newline()
	bl newline
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

@ End
]]*)
