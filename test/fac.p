(* The usual recursive factorial *)

proc fac(n: integer): integer;
begin
  if n = 0 then
    return 1
  else
    return n * fac(n-1)
  end
end;

var f: integer;

begin
  f := fac(10);
  print_num(f);
  newline()
end.
        
(*<<
3628800
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc fac(n: integer): integer;
	.section .text
_fac:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   if n = 0 then
	ldr r0, [fp, #24]
	cmp r0, #0
	bne .L3
@     return 1
	mov r0, #1
	b .L1
.L3:
@     return n * fac(n-1)
	ldr r5, [fp, #24]
	sub r0, r5, #1
	bl _fac
	mul r0, r5, r0
.L1:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   f := fac(10);
	mov r0, #10
	bl _fac
	ldr r1, =_f
	str r0, [r1]
@   print_num(f);
	bl print_num
@   newline()
	bl newline
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

	.comm _f, 4, 4
@ End
]]*)
