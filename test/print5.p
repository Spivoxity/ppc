(* Printing strings *)

begin
  print_string("five"); newline()
end.

(*<<
five
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.section .text
pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   print_string("five"); newline()
	mov r1, #5
	ldr r0, =g1
	bl print_string
	bl newline
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

	.section .rodata
g1:
	.byte 102, 105, 118, 101
	.byte 0
@ End
]]*)
