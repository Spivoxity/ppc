(* Printing numbers *)

begin
  print_num(2); newline()
end.

(*<<
2
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.section .text
pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   print_num(2); newline()
	mov r0, #2
	bl print_num
	bl newline
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

@ End
]]*)
