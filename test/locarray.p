(* Local array *)

proc P();
  var i: integer; x: integer; a: array 10 of integer;
begin
  i := 0;
  x := a[i]
end;

begin
  P()
end.

(*[[
@ picoPascal compiler output
	.global pmain

@ proc P();
	.section .text
_P:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #40
@   i := 0;
	mov r5, #0
@   x := a[i]
	add r0, fp, #-40
	ldr r6, [r0, r5, LSL #2]
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   P()
	bl _P
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

@ End
]]*)
