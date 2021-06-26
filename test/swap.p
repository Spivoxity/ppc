(* Procedure to swap elements of array *)

const n = 10;

var a: array n of integer; 

proc swap(i, j: integer);
  var t: integer;
begin
  t := a[i]; 
  a[i] := a[j]; 
  a[j] := t
end;

proc main();
  var i: integer;
begin
  for i := 0 to n-1 do a[i] := i end;
  swap(3, 6);
  for i := 0 to n-1 do print_num(a[i]) end;
  newline()
end;

begin 
  main()
end.

(*<<
0126453789
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc swap(i, j: integer);
	.section .text
_swap:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   t := a[i]; 
	ldr r6, =_a
	ldr r0, [fp, #32]
	add r7, r6, r0, LSL #2
	ldr r5, [r7]
@   a[i] := a[j]; 
	ldr r0, [fp, #36]
	add r6, r6, r0, LSL #2
	ldr r0, [r6]
	str r0, [r7]
@   a[j] := t
	str r5, [r6]
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc main();
_main:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   for i := 0 to n-1 do a[i] := i end;
	mov r5, #0
	mov r6, #9
.L3:
	cmp r5, r6
	bgt .L4
	ldr r0, =_a
	str r5, [r0, r5, LSL #2]
	add r5, r5, #1
	b .L3
.L4:
@   swap(3, 6);
	mov r1, #6
	mov r0, #3
	bl _swap
@   for i := 0 to n-1 do print_num(a[i]) end;
	mov r5, #0
	mov r7, #9
.L5:
	cmp r5, r7
	bgt .L6
	ldr r0, =_a
	ldr r0, [r0, r5, LSL #2]
	bl print_num
	add r5, r5, #1
	b .L5
.L6:
@   newline()
	bl newline
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   main()
	bl _main
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

	.comm _a, 40, 4
@ End
]]*)
