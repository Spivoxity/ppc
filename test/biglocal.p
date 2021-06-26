(* Large local array and large array parameter *)

proc foo(var a: array 10000 of integer);
  var c: array 10000 of integer; x: integer;
begin 
  x := 5000;
  c[5000] := 4;
  a[5000] := c[x]+3 
end;

var b: array 10000 of integer;
begin foo(b) end.

(*[[
@ picoPascal compiler output
	.global pmain

@ proc foo(var a: array 10000 of integer);
	.section .text
_foo:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
	ldr ip, =40000
	sub sp, sp, ip
@   x := 5000;
	ldr r5, =5000
@   c[5000] := 4;
	mov r0, #4
	ldr r1, =-20000
	str r0, [fp, r1]
@   a[5000] := c[x]+3 
	ldr ip, =-40000
	add r0, fp, ip
	ldr r0, [r0, r5, LSL #2]
	add r0, r0, #3
	ldr r1, [fp, #24]
	ldr r2, =20000
	str r0, [r1, r2]
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@ begin foo(b) end.
	ldr r0, =_b
	bl _foo
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

	.comm _b, 40000, 4
@ End
]]*)
