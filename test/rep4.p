begin
  repeat
    print_string("Hello"); newline()
  until true
end.

(*<<
Hello
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.section .text
pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@     print_string("Hello"); newline()
	mov r1, #6
	ldr r0, =g1
	bl print_string
	bl newline
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

	.section .rodata
g1:
	.byte 72, 101, 108, 108, 111
	.byte 0
@ End
]]*)
