(* Linked lists *)

type ptr = pointer to rec;
  rec = record 
      data: integer; 
      next: ptr; 
    end;

proc sum(p: ptr): integer;
  var q: ptr; s: integer;
begin
  q := p; s := 0;
  while q <> nil do
    s := s + q^.data;
    q := q^.next
  end;
  return s
end;

proc main();
  const input = "3141592650";
  var i: integer; p, q: ptr;
begin
  i := 0;
  while input[i] <> '0' do i := i+1 end;

  p := nil;
  while i > 0 do
    i := i-1;
    q := p;
    new(p);
    p^.data := ord(input[i]) - ord('0');
    p^.next := q
  end;

  print_num(sum(p)); newline()
end;

begin
  main()
end.

(*<<
36
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc sum(p: ptr): integer;
	.section .text
_sum:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   q := p; s := 0;
	ldr r5, [fp, #24]
	mov r6, #0
.L2:
@   while q <> nil do
	cmp r5, #0
	beq .L4
@     s := s + q^.data;
	ldr r0, [r5]
	add r6, r6, r0
@     q := q^.next
	ldr r5, [r5, #4]
	b .L2
.L4:
@   return s
	mov r0, r6
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc main();
_main:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   i := 0;
	mov r5, #0
.L6:
@   while input[i] <> '0' do i := i+1 end;
	ldr r0, =g1
	ldrb r0, [r0, r5]
	cmp r0, #48
	beq .L8
	add r5, r5, #1
	b .L6
.L8:
@   p := nil;
	mov r6, #0
.L9:
@   while i > 0 do
	cmp r5, #0
	ble .L11
@     i := i-1;
	sub r5, r5, #1
@     q := p;
	mov r7, r6
@     new(p);
	mov r0, #8
	bl new
	mov r6, r0
@     p^.data := ord(input[i]) - ord('0');
	ldr r0, =g1
	ldrb r0, [r0, r5]
	sub r0, r0, #48
	str r0, [r6]
@     p^.next := q
	str r7, [r6, #4]
	b .L9
.L11:
@   print_num(sum(p)); newline()
	mov r0, r6
	bl _sum
	bl print_num
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

	.section .rodata
g1:
	.byte 51, 49, 52, 49, 53, 57, 50, 54, 53, 48
	.byte 0
@ End
]]*)
