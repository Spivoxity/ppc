var i: integer;

begin
  i := 0;
  while i < 10 do
    case i of
      1:
        i := i + 1;
	i := i + 2
    | 3:
        i := i + 1;
	i := i + 2
    | 5:
        i := i + 1;
	i := i + 2
    | 2: 
        i := i - 1;
    | 6: 
        i := i - 1;
    | 8:
        i := i + 2;
    else
      i := i + 1
    end;
    print_num(i); newline()
  end
end.

(*<<
1
4
5
8
10
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.section .text
pmain:
	mov ip, sp
	stmfd sp!, {r4-r6, fp, ip, lr}
	mov fp, sp
@   i := 0;
	mov r0, #0
	ldr r1, =_i
	str r0, [r1]
.L2:
@   while i < 10 do
	ldr r0, =_i
	ldr r5, [r0]
	cmp r5, #10
	bge .L1
@     case i of
	sub r0, r5, #1
	cmp r0, #8
	ldrlo pc, [pc, r0, LSL #2]
	b .L5
	.word .L7
	.word .L10
	.word .L8
	.word .L5
	.word .L9
	.word .L11
	.word .L5
	.word .L12
.L7:
@         i := i + 1;
	ldr r5, =_i
	ldr r0, [r5]
	add r6, r0, #1
	str r6, [r5]
@ 	i := i + 2
	add r0, r6, #2
	str r0, [r5]
	b .L6
.L8:
@         i := i + 1;
	ldr r5, =_i
	ldr r0, [r5]
	add r6, r0, #1
	str r6, [r5]
@ 	i := i + 2
	add r0, r6, #2
	str r0, [r5]
	b .L6
.L9:
@         i := i + 1;
	ldr r5, =_i
	ldr r0, [r5]
	add r6, r0, #1
	str r6, [r5]
@ 	i := i + 2
	add r0, r6, #2
	str r0, [r5]
	b .L6
.L10:
@         i := i - 1;
	ldr r5, =_i
	ldr r0, [r5]
	sub r0, r0, #1
	str r0, [r5]
	b .L6
.L11:
@         i := i - 1;
	ldr r5, =_i
	ldr r0, [r5]
	sub r0, r0, #1
	str r0, [r5]
	b .L6
.L12:
@         i := i + 2;
	ldr r5, =_i
	ldr r0, [r5]
	add r0, r0, #2
	str r0, [r5]
	b .L6
.L5:
@       i := i + 1
	ldr r5, =_i
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
.L6:
@     print_num(i); newline()
	ldr r0, =_i
	ldr r0, [r0]
	bl print_num
	bl newline
	b .L2
.L1:
	ldmfd fp, {r4-r6, fp, sp, pc}
	.ltorg

	.comm _i, 4, 4
@ End
]]*)
