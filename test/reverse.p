type list = pointer to cell;
  cell = record head: char; tail: list end;

proc reverse(a: list): list;
  var p, q, r: list;
begin
  p := a; q := nil;
  while p <> nil do
    r := p^.tail; p^.tail := q;
    q := p; p := r
  end;
  return q
end;

proc test();
  const mike = "mike";

  var i: integer; p, q: list;
begin
  p := nil; i := 0; 
  while mike[i] <> chr(0) do
    new(q);
    q^.head := mike[i];
    q^.tail := p;
    p := q; i := i+1
  end;

  p := reverse(p);

  q := p;
  while q <> nil do
    print_char(q^.head);
    q := q^.tail
  end;
  newline()
end;

begin test() end.

(*<<
mike
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc reverse(a: list): list;
	.section .text
_reverse:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   p := a; q := nil;
	ldr r5, [fp, #32]
	mov r6, #0
.L2:
@   while p <> nil do
	cmp r5, #0
	beq .L4
@     r := p^.tail; p^.tail := q;
	ldr r8, =4
	ldr r7, [r5, r8]
	str r6, [r5, r8]
@     q := p; p := r
	mov r6, r5
	mov r5, r7
	b .L2
.L4:
@   return q
	mov r0, r6
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

@ proc test();
_test:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   p := nil; i := 0; 
	mov r6, #0
	mov r5, #0
.L6:
@   while mike[i] <> chr(0) do
	ldr r8, =g1
	ldrb r0, [r8, r5]
	cmp r0, #0
	beq .L8
@     new(q);
	mov r0, #8
	bl new
	mov r7, r0
@     q^.head := mike[i];
	ldrb r0, [r8, r5]
	strb r0, [r7]
@     q^.tail := p;
	str r6, [r7, #4]
@     p := q; i := i+1
	mov r6, r7
	add r5, r5, #1
	b .L6
.L8:
@   p := reverse(p);
	mov r0, r6
	bl _reverse
	mov r6, r0
@   q := p;
	mov r7, r6
.L9:
@   while q <> nil do
	cmp r7, #0
	beq .L11
@     print_char(q^.head);
	ldrb r0, [r7]
	bl print_char
@     q := q^.tail
	ldr r7, [r7, #4]
	b .L9
.L11:
@   newline()
	bl newline
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@ begin test() end.
	bl _test
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

	.section .rodata
g1:
	.byte 109, 105, 107, 101
	.byte 0
@ End
]]*)
