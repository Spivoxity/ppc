proc g(var c: char); begin
  c := chr(ord(c)+1)
end;

proc f(x: char): char;
  var c: char; n: integer;
begin
  c := x;
  g(c);
  return c
end;

begin
  print_char(f('A')); newline()
end.

(*<<
B
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc g(var c: char); begin
	.section .text
_g:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   c := chr(ord(c)+1)
	ldr r5, [fp, #24]
	ldrb r0, [r5]
	add r0, r0, #1
	strb r0, [r5]
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc f(x: char): char;
_f:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #8
@   c := x;
	ldrb r0, [fp, #16]
	strb r0, [fp, #-1]
@   g(c);
	add r0, fp, #-1
	bl _g
@   return c
	ldrb r0, [fp, #-1]
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   print_char(f('A')); newline()
	mov r0, #65
	bl _f
	bl print_char
	bl newline
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

@ End
]]*)
