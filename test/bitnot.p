(* Bitwise NOT function *)

var x: integer;
begin
  x := 314159265;
  x := bitnot(x);
  print_num(x); newline()
end.

(*<<
-314159266
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.section .text
pmain:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   x := 314159265;
	ldr r5, =314159265
	ldr r6, =_x
	str r5, [r6]
@   x := bitnot(x);
	mvn r5, r5
	str r5, [r6]
@   print_num(x); newline()
	mov r0, r5
	bl print_num
	bl newline
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

	.comm _x, 4, 4
@ End
]]*)
