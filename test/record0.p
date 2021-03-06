(* Phone book using record type *)

const wordlen = 10;

type string = array wordlen of char;

type rec = record name: string; age: integer end;

var 
  db: array 20 of rec;
  N: integer;

proc equal(x, y: string): boolean;
  var i: integer;
begin
  i := 0;
  while i < wordlen do
    if x[i] <> y[i] then
      return false
    end;
    i := i+1
  end;
  return true
end;

proc copy(var dst: string; src: string);
  var i: integer;
begin
  i := 0;
  while i < wordlen do
    dst[i] := src[i]; i := i+1
  end
end;

proc store(n: string; a: integer);
begin
  copy(db[N].name, n);
  db[N].age := a;
  N := N+1
end;

proc recall(n: string): integer;
  var i: integer;
begin
  i := 0;
  while i < N do
    if equal(db[i].name, n) then
      return db[i].age
    end;
    i := i+1
  end;
  return 999
end;

begin
  N := 0;

  store("bill     ", 23);
  store("george   ", 34);

  print_num(recall("george   ")); newline();
  print_num(recall("fred     ")); newline()
end.

(*<<
34
999
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc equal(x, y: string): boolean;
	.section .text
_equal:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   i := 0;
	mov r4, #0
.L6:
@   while i < wordlen do
	cmp r4, #10
	bge .L8
@     if x[i] <> y[i] then
	ldr r0, [fp, #40]
	ldrb r0, [r0, r4]
	ldr r1, [fp, #44]
	ldrb r1, [r1, r4]
	cmp r0, r1
	beq .L11
@       return false
	mov r0, #0
	b .L5
.L11:
@     i := i+1
	add r4, r4, #1
	b .L6
.L8:
@   return true
	mov r0, #1
.L5:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc copy(var dst: string; src: string);
_copy:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   i := 0;
	mov r4, #0
.L13:
@   while i < wordlen do
	cmp r4, #10
	bge .L12
@     dst[i] := src[i]; i := i+1
	ldr r0, [fp, #44]
	ldrb r0, [r0, r4]
	ldr r1, [fp, #40]
	strb r0, [r1, r4]
	add r4, r4, #1
	b .L13
.L12:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc store(n: string; a: integer);
_store:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   copy(db[N].name, n);
	ldr r4, =_db
	ldr r5, =_N
	ldr r1, [fp, #40]
	ldr r0, [r5]
	add r0, r4, r0, LSL #4
	bl _copy
@   db[N].age := a;
	ldr r0, [fp, #44]
	ldr r1, [r5]
	add r1, r4, r1, LSL #4
	str r0, [r1, #12]
@   N := N+1
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc recall(n: string): integer;
_recall:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   i := 0;
	mov r4, #0
.L18:
@   while i < N do
	ldr r0, =_N
	ldr r0, [r0]
	cmp r4, r0
	bge .L20
@     if equal(db[i].name, n) then
	ldr r5, =_db
	ldr r1, [fp, #40]
	add r0, r5, r4, LSL #4
	bl _equal
	cmp r0, #0
	beq .L23
@       return db[i].age
	add r0, r5, r4, LSL #4
	ldr r0, [r0, #12]
	b .L17
.L23:
@     i := i+1
	add r4, r4, #1
	b .L18
.L20:
@   return 999
	ldr r0, =999
.L17:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   N := 0;
	mov r0, #0
	ldr r1, =_N
	str r0, [r1]
@   store("bill     ", 23);
	mov r1, #23
	ldr r0, =g1
	bl _store
@   store("george   ", 34);
	mov r1, #34
	ldr r0, =g2
	bl _store
@   print_num(recall("george   ")); newline();
	ldr r0, =g3
	bl _recall
	bl print_num
	bl newline
@   print_num(recall("fred     ")); newline()
	ldr r0, =g4
	bl _recall
	bl print_num
	bl newline
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

	.comm _db, 320, 4
	.comm _N, 4, 4
	.section .rodata
g1:
	.byte 98, 105, 108, 108, 32, 32, 32, 32, 32
	.byte 0
g2:
	.byte 103, 101, 111, 114, 103, 101, 32, 32, 32
	.byte 0
g3:
	.byte 103, 101, 111, 114, 103, 101, 32, 32, 32
	.byte 0
g4:
	.byte 102, 114, 101, 100, 32, 32, 32, 32, 32
	.byte 0
@ End
]]*)
