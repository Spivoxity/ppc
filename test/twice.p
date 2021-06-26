(* Another test of higher-order functions *)

type int = integer;

proc square(x: int): int; begin return x * x end;

proc twice(proc f(y: int): int; x: int): int;
begin return f(f(x)) end;

proc ap_to_sq(proc ff(proc f(x: int): int; x: int): int; x: int): int;
begin return ff(square, x) end;

begin
  print_num(ap_to_sq(twice, 3));
  newline()
end.

(*<<
81
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc square(x: int): int; begin return x * x end;
	.section .text
_square:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@ proc square(x: int): int; begin return x * x end;
	ldr r5, [fp, #24]
	mul r0, r5, r5
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc twice(proc f(y: int): int; x: int): int;
_twice:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@ begin return f(f(x)) end;
	ldr r5, [fp, #24]
	ldr r6, [fp, #28]
	ldr r0, [fp, #32]
	mov r4, r6
	blx r5
	mov r4, r6
	blx r5
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc ap_to_sq(proc ff(proc f(x: int): int; x: int): int; x: int): int;
_ap_to_sq:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@ begin return ff(square, x) end;
	ldr r2, [fp, #24]
	mov r1, #0
	ldr r0, =_square
	ldr r4, [fp, #20]
	ldr r3, [fp, #16]
	blx r3
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   print_num(ap_to_sq(twice, 3));
	mov r2, #3
	mov r1, #0
	ldr r0, =_twice
	bl _ap_to_sq
	bl print_num
@   newline()
	bl newline
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

@ End
]]*)
