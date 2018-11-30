type matrix = array 3 of array 3 of integer;

proc sum(proc f(t: integer): integer): integer;
begin
    return f(0) + f(1) + f(2)
end;

proc matsum(var a: matrix): integer;
    proc rowsum(i: integer): integer;
        proc cell(j: integer): integer;
        begin return a[i][j] end;
    begin
        return sum(cell)
    end;
begin
    return sum(rowsum)
end;

proc test();
    var a: matrix; i, j: integer;
begin
    for i := 0 to 2 do
        for j := 0 to 2 do
            a[i][j] := (i+1)*(j+1)
        end
    end;

    print_num(matsum(a)); newline()
end;

begin test() end.

(*<<
36
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc sum(proc f(t: integer): integer): integer;
	.section .text
_sum:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@     return f(0) + f(1) + f(2)
	mov r0, #0
	ldr r10, [fp, #44]
	ldr r1, [fp, #40]
	blx r1
	mov r4, r0
	mov r0, #1
	ldr r10, [fp, #44]
	ldr r1, [fp, #40]
	blx r1
	mov r5, r0
	mov r0, #2
	ldr r10, [fp, #44]
	ldr r1, [fp, #40]
	blx r1
	add r1, r4, r5
	add r0, r1, r0
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc matsum(var a: matrix): integer;
_matsum:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@     return sum(rowsum)
	mov r1, fp
	ldr r0, =_rowsum
	bl _sum
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@     proc rowsum(i: integer): integer;
_rowsum:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@         return sum(cell)
	mov r1, fp
	ldr r0, =_cell
	bl _sum
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@         proc cell(j: integer): integer;
_cell:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@         begin return a[i][j] end;
	ldr r4, [fp, #24]
	ldr r0, [r4, #24]
	ldr r0, [r0, #40]
	ldr r1, [r4, #40]
	mov r2, #12
	mul r1, r1, r2
	add r0, r0, r1
	ldr r1, [fp, #40]
	ldr r0, [r0, r1, LSL #2]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc test();
_test:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #40
@     for i := 0 to 2 do
	mov r4, #0
	mov r0, #2
	str r0, [fp, #-40]
.L6:
	ldr r0, [fp, #-40]
	cmp r4, r0
	bgt .L7
@         for j := 0 to 2 do
	mov r5, #0
	mov r6, #2
.L8:
	cmp r5, r6
	bgt .L9
@             a[i][j] := (i+1)*(j+1)
	add r7, r5, #1
	add r0, r4, #1
	mul r0, r0, r7
	add r1, fp, #-36
	mov r2, #12
	mul r2, r4, r2
	add r1, r1, r2
	str r0, [r1, r5, LSL #2]
	mov r5, r7
	b .L8
.L9:
	add r4, r4, #1
	b .L6
.L7:
@     print_num(matsum(a)); newline()
	add r0, fp, #-36
	bl _matsum
	bl print_num
	bl newline
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ begin test() end.
	bl _test
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ End
]]*)
