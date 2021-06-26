(* Alias analysis and VAR parameters *)

var a,b,c,d: integer;

proc p1(var a: integer; b: integer; var d: integer): integer;
  var c: integer;
begin
  c :=b+a;
  d :=b+1;
  a :=a-b;
  return (a+d)*b
end;

begin
  a:=5; b:=2; c:=3; d:=1;
  b := p1(b,d,a) + 1;
  print_string("A="); print_num(a);
  print_string(" B="); print_num(b);
  print_string(" C="); print_num(c);
  print_string(" D="); print_num(d);
  newline()
end.

(*<<
A=2 B=4 C=3 D=1
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc p1(var a: integer; b: integer; var d: integer): integer;
	.section .text
_p1:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   c :=b+a;
	ldr r6, [fp, #28]
	ldr r0, [fp, #24]
	ldr r0, [r0]
	add r5, r6, r0
@   d :=b+1;
	add r0, r6, #1
	ldr r1, [fp, #32]
	str r0, [r1]
@   a :=a-b;
	ldr r6, [fp, #24]
	ldr r0, [r6]
	ldr r1, [fp, #28]
	sub r0, r0, r1
	str r0, [r6]
@   return (a+d)*b
	ldr r0, [fp, #24]
	ldr r0, [r0]
	ldr r1, [fp, #32]
	ldr r1, [r1]
	add r0, r0, r1
	ldr r1, [fp, #28]
	mul r0, r0, r1
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   a:=5; b:=2; c:=3; d:=1;
	ldr r5, =_a
	mov r0, #5
	str r0, [r5]
	ldr r6, =_b
	mov r0, #2
	str r0, [r6]
	ldr r7, =_c
	mov r0, #3
	str r0, [r7]
	mov r8, #1
	ldr r9, =_d
	str r8, [r9]
@   b := p1(b,d,a) + 1;
	mov r2, r5
	mov r1, r8
	mov r0, r6
	bl _p1
	add r0, r0, #1
	str r0, [r6]
@   print_string("A="); print_num(a);
	mov r1, #3
	ldr r0, =g1
	bl print_string
	ldr r0, [r5]
	bl print_num
@   print_string(" B="); print_num(b);
	mov r1, #4
	ldr r0, =g2
	bl print_string
	ldr r0, [r6]
	bl print_num
@   print_string(" C="); print_num(c);
	mov r1, #4
	ldr r0, =g3
	bl print_string
	ldr r0, [r7]
	bl print_num
@   print_string(" D="); print_num(d);
	mov r1, #4
	ldr r0, =g4
	bl print_string
	ldr r0, [r9]
	bl print_num
@   newline()
	bl newline
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

	.comm _a, 4, 4
	.comm _b, 4, 4
	.comm _c, 4, 4
	.comm _d, 4, 4
	.section .rodata
g1:
	.byte 65, 61
	.byte 0
g2:
	.byte 32, 66, 61
	.byte 0
g3:
	.byte 32, 67, 61
	.byte 0
g4:
	.byte 32, 68, 61
	.byte 0
@ End
]]*)
