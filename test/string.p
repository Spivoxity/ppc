(* Print a string *)

begin
  print_string("Hello world!");
  newline()
end.

(*<<
Hello world!
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.section .text
pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   print_string("Hello world!");
	mov r1, #13
	ldr r0, =g1
	bl print_string
@   newline()
	bl newline
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

	.section .rodata
g1:
	.byte 72, 101, 108, 108, 111, 32, 119, 111, 114, 108
	.byte 100, 33
	.byte 0
@ End
]]*)
