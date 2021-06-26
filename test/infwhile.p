proc foo(): integer;
  var i: integer;
begin
  i := 3;
  while true do
    i := i + 2;
    if i > 10 then return i end;
  end
end;

begin
  print_num(foo()); newline()
end.

(*<<
11
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc foo(): integer;
	.section .text
_foo:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   i := 3;
	mov r5, #3
.L2:
@     i := i + 2;
	add r5, r5, #2
@     if i > 10 then return i end;
	cmp r5, #10
	ble .L2
	mov r0, r5
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   print_num(foo()); newline()
	bl _foo
	bl print_num
	bl newline
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

@ End
]]*)
