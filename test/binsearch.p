(* Binary search for sqrt(200000000) *)

var y, a, b, m: integer;

begin
  y := 200000000;
  a := 0;
  b := 20000;
  (* Inv: a^2 <= y < b^2 *)
  while a+1 < b do
    m := (a+b) div 2;
    if m*m <= y then
      a := m
    else
      b := m
    end
  end;
  print_num(a); newline()
end. 

(*<<
14142
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.section .text
pmain:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   y := 200000000;
	ldr r0, =200000000
	ldr r1, =_y
	str r0, [r1]
@   a := 0;
	mov r0, #0
	ldr r1, =_a
	str r0, [r1]
@   b := 20000;
	ldr r0, =20000
	ldr r1, =_b
	str r0, [r1]
.L2:
@   while a+1 < b do
	ldr r5, =_a
	ldr r6, [r5]
	ldr r0, =_b
	ldr r7, [r0]
	add r0, r6, #1
	cmp r0, r7
	bge .L4
@     m := (a+b) div 2;
	add r0, r6, r7
	asr r6, r0, #1
	ldr r0, =_m
	str r6, [r0]
@     if m*m <= y then
	mul r0, r6, r6
	ldr r1, =_y
	ldr r1, [r1]
	cmp r0, r1
	bgt .L6
@       a := m
	str r6, [r5]
	b .L2
.L6:
@       b := m
	ldr r0, =_m
	ldr r0, [r0]
	ldr r1, =_b
	str r0, [r1]
	b .L2
.L4:
@   print_num(a); newline()
	ldr r0, =_a
	ldr r0, [r0]
	bl print_num
	bl newline
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

	.comm _y, 4, 4
	.comm _a, 4, 4
	.comm _b, 4, 4
	.comm _m, 4, 4
@ End
]]*)
