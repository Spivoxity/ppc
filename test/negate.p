(* Negation of booleans and integers *)

var x, y: boolean;
var u, v: integer;

begin
  x := true;
  y := not x;
  y := not y;
  if x = y then
    print_string("OK"); newline()
  end;

  u := 37;
  v := -u;
  v := -v;
  if u = v then
    print_string("OK2"); newline()
  end
end.

(*<<
OK
OK2
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.section .text
pmain:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   x := true;
	mov r5, #1
	ldr r0, =_x
	strb r5, [r0]
@   y := not x;
	eor r6, r5, #1
	ldr r7, =_y
	strb r6, [r7]
@   y := not y;
	eor r6, r6, #1
	strb r6, [r7]
@   if x = y then
	cmp r5, r6
	bne .L4
@     print_string("OK"); newline()
	mov r1, #3
	ldr r0, =g1
	bl print_string
	bl newline
.L4:
@   u := 37;
	mov r5, #37
	ldr r0, =_u
	str r5, [r0]
@   v := -u;
	neg r6, r5
	ldr r7, =_v
	str r6, [r7]
@   v := -v;
	neg r6, r6
	str r6, [r7]
@   if u = v then
	cmp r5, r6
	bne .L1
@     print_string("OK2"); newline()
	mov r1, #4
	ldr r0, =g2
	bl print_string
	bl newline
.L1:
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

	.comm _x, 1, 4
	.comm _y, 1, 4
	.comm _u, 4, 4
	.comm _v, 4, 4
	.section .rodata
g1:
	.byte 79, 75
	.byte 0
g2:
	.byte 79, 75, 50
	.byte 0
@ End
]]*)
