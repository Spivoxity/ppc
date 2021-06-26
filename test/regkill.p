(* Aliasing in CSE *)

var x: integer;

proc p();
var y, z: integer;
begin
  y := 1;
  z := y + 1;
  z := 3;
  z := y + 1;
  x := z
end;

begin
  p();
  print_num(x);
  newline()
end.

(*<<
2
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc p();
	.section .text
_p:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   y := 1;
	mov r5, #1
@   z := y + 1;
	add r6, r5, #1
@   z := 3;
	mov r6, #3
@   z := y + 1;
	add r6, r5, #1
@   x := z
	ldr r0, =_x
	str r6, [r0]
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   p();
	bl _p
@   print_num(x);
	ldr r0, =_x
	ldr r0, [r0]
	bl print_num
@   newline()
	bl newline
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

	.comm _x, 4, 4
@ End
]]*)
