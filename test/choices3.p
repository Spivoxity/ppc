(* Enumerate (n choose k) choices with linked list *)

const letters = "abcdef";

type list = pointer to cell; cell = record head: char; tail: list end;

proc cons(head: char; tail: list): list;
  var p: list;
begin
  new(p);
  p^.head := head; p^.tail := tail;
  return p
end;

proc print(p: list);
  var q: list;
begin
  q := p;
  while q <> nil do
    print_char(q^.head);
    q := q^.tail
  end
end;

(* Complete choice with k items chosen from [0..n) *)
proc choose(k, n: integer; suffix: list);
begin
  if k <= n then
    if k = 0 then
      print(suffix); newline()
    else
      choose(k, n-1, suffix);
      choose(k-1, n-1, cons(letters[n-1], suffix))
    end
  end
end;

begin
  choose(3, 6, nil)
end.    

(*<<
abc
abd
acd
bcd
abe
ace
bce
ade
bde
cde
abf
acf
bcf
adf
bdf
cdf
aef
bef
cef
def
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc cons(head: char; tail: list): list;
	.section .text
_cons:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   new(p);
	mov r0, #8
	bl new
	mov r5, r0
@   p^.head := head; p^.tail := tail;
	ldrb r0, [fp, #24]
	strb r0, [r5]
	ldr r0, [fp, #28]
	str r0, [r5, #4]
@   return p
	mov r0, r5
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc print(p: list);
_print:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   q := p;
	ldr r5, [fp, #24]
.L3:
@   while q <> nil do
	cmp r5, #0
	beq .L2
@     print_char(q^.head);
	ldrb r0, [r5]
	bl print_char
@     q := q^.tail
	ldr r5, [r5, #4]
	b .L3
.L2:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc choose(k, n: integer; suffix: list);
_choose:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   if k <= n then
	ldr r5, [fp, #24]
	ldr r0, [fp, #28]
	cmp r5, r0
	bgt .L6
@     if k = 0 then
	cmp r5, #0
	bne .L11
@       print(suffix); newline()
	ldr r0, [fp, #32]
	bl _print
	bl newline
	b .L6
.L11:
@       choose(k, n-1, suffix);
	ldr r2, [fp, #32]
	ldr r0, [fp, #28]
	sub r1, r0, #1
	ldr r0, [fp, #24]
	bl _choose
@       choose(k-1, n-1, cons(letters[n-1], suffix))
	ldr r5, [fp, #28]
	ldr r1, [fp, #32]
	ldr r0, =g1
	add r0, r0, r5
	ldrb r0, [r0, #-1]
	bl _cons
	mov r2, r0
	sub r1, r5, #1
	ldr r0, [fp, #24]
	sub r0, r0, #1
	bl _choose
.L6:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
	mov fp, sp
@   choose(3, 6, nil)
	mov r2, #0
	mov r1, #6
	mov r0, #3
	bl _choose
	ldmfd fp, {r4, fp, sp, pc}
	.ltorg

	.section .rodata
g1:
	.byte 97, 98, 99, 100, 101, 102
	.byte 0
@ End
]]*)
