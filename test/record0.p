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
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   i := 0;
	mov r5, #0
.L2:
@   while i < wordlen do
	cmp r5, #10
	bge .L4
@     if x[i] <> y[i] then
	ldr r0, [fp, #24]
	ldrb r0, [r0, r5]
	ldr r1, [fp, #28]
	ldrb r1, [r1, r5]
	cmp r0, r1
	beq .L7
@       return false
	mov r0, #0
	b .L1
.L7:
@     i := i+1
	add r5, r5, #1
	b .L2
.L4:
@   return true
	mov r0, #1
.L1:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc copy(var dst: string; src: string);
_copy:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   i := 0;
	mov r5, #0
.L9:
@   while i < wordlen do
	cmp r5, #10
	bge .L8
@     dst[i] := src[i]; i := i+1
	ldr r0, [fp, #28]
	ldrb r0, [r0, r5]
	ldr r1, [fp, #24]
	strb r0, [r1, r5]
	add r5, r5, #1
	b .L9
.L8:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc store(n: string; a: integer);
_store:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   copy(db[N].name, n);
	ldr r5, =_db
	ldr r6, =_N
	ldr r1, [fp, #24]
	ldr r0, [r6]
	add r0, r5, r0, LSL #4
	ldr r2, =0
	add r0, r0, r2
	bl _copy
@   db[N].age := a;
	ldr r0, [fp, #28]
	ldr r1, [r6]
	add r1, r5, r1, LSL #4
	str r0, [r1, #12]
@   N := N+1
	ldr r0, [r6]
	add r0, r0, #1
	str r0, [r6]
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

@ proc recall(n: string): integer;
_recall:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   i := 0;
	mov r5, #0
.L14:
@   while i < N do
	ldr r0, =_N
	ldr r0, [r0]
	cmp r5, r0
	bge .L16
@     if equal(db[i].name, n) then
	ldr r6, =_db
	ldr r1, [fp, #24]
	add r0, r6, r5, LSL #4
	ldr r2, =0
	add r0, r0, r2
	bl _equal
	cmp r0, #0
	beq .L19
@       return db[i].age
	add r0, r6, r5, LSL #4
	ldr r0, [r0, #12]
	b .L13
.L19:
@     i := i+1
	add r5, r5, #1
	b .L14
.L16:
@   return 999
	ldr r0, =999
.L13:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4, fp, ip, lr}
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
	ldmfd fp, {r4, fp, sp, pc}
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
