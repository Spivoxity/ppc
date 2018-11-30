(* Representation of immediate constants *)

var k: integer;
begin
  k := 100;
  print_num(516); newline();
  print_num(517); newline();
  print_num(k + -50); newline();
  print_num(k + -1023); newline();
  print_num(k + -1024); newline()
end.

(*<<
516
517
50
-923
-924
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.section .text
pmain:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   k := 100;
	ldr r4, =_k
	mov r0, #100
	str r0, [r4]
@   print_num(516); newline();
	mov r0, #516
	bl print_num
	bl newline
@   print_num(517); newline();
	ldr r0, =517
	bl print_num
	bl newline
@   print_num(k + -50); newline();
	ldr r0, [r4]
	sub r0, r0, #50
	bl print_num
	bl newline
@   print_num(k + -1023); newline();
	ldr r0, [r4]
	ldr r1, =1023
	sub r0, r0, r1
	bl print_num
	bl newline
@   print_num(k + -1024); newline()
	ldr r0, [r4]
	sub r0, r0, #1024
	bl print_num
	bl newline
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

	.comm _k, 4, 4
@ End
]]*)
