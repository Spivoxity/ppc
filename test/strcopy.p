(* String copying by loop *)

const in = "Hello, world!*";

var out: array 128 of char; i: integer;

begin
  i := 0;
  while in[i] <> '*' do
    out[i] := in[i];
    i := i + 1
  end;
  out[i] := chr(0);
  print_string(out); newline()
end.

(*<<
Hello, world!
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.section .text
pmain:
	mov ip, sp
	stmfd sp!, {r4-r8, fp, ip, lr}
	mov fp, sp
@   i := 0;
	mov r0, #0
	ldr r1, =_i
	str r0, [r1]
.L2:
@   while in[i] <> '*' do
	ldr r5, =_i
	ldr r6, [r5]
	ldr r0, =g1
	ldrb r7, [r0, r6]
	cmp r7, #42
	beq .L4
@     out[i] := in[i];
	ldr r0, =_out
	strb r7, [r0, r6]
@     i := i + 1
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
	b .L2
.L4:
@   out[i] := chr(0);
	ldr r5, =_out
	mov r0, #0
	ldr r1, =_i
	ldr r1, [r1]
	strb r0, [r5, r1]
@   print_string(out); newline()
	mov r1, #128
	mov r0, r5
	bl print_string
	bl newline
	ldmfd fp, {r4-r8, fp, sp, pc}
	.ltorg

	.comm _out, 128, 4
	.comm _i, 4, 4
	.section .rodata
g1:
	.byte 72, 101, 108, 108, 111, 44, 32, 119, 111, 114
	.byte 108, 100, 33, 42
	.byte 0
@ End
]]*)
