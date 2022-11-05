// picoPascal compiler output
	.global pmain

// proc PrintCol(c: Column);
	.section .text
_PrintCol:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_char(c^.name); print_num(c^.x); print_num(c^.y)
	add x20, fp, #32
	ldr x0, [x20]
	ldrb w0, [x0]
	bl print_char
	ldr x0, [x20]
	ldr w0, [x0, #4]
	bl print_num
	ldr x0, [x20]
	ldr w0, [x0, #8]
	bl print_num
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc PrintRow(p: Cell);
_PrintRow:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   q := p;
	ldr x20, [fp, #64]
.L3:
//     print_string(" "); PrintCol(q^.column); q := q^.right
	mov w1, #2
	ldr x0, =g1
	bl print_string
	ldr w22, =32
	ldr x0, [x20, w22, SXTW]
	bl _PrintCol
	ldr x20, [x20, #24]
	ldr x23, [fp, #64]
	cmp x20, x23
	bne .L3
//   n := 0; q := p^.column^.head;
	mov w21, wzr
	ldr x0, [x23, w22, SXTW]
	ldr x20, [x0, #40]
.L5:
//   while q <> p do n := n+1; q := q^.down end;
	ldr x0, [fp, #64]
	cmp x20, x0
	beq .L7
	add w21, w21, #1
	ldr x20, [x20, #8]
	b .L5
.L7:
//   print_string("; # "); print_num(n); print_string(" of ");
	mov w1, #5
	ldr x0, =g2
	bl print_string
	mov w0, w21
	bl print_num
	mov w1, #5
	ldr x0, =g3
	bl print_string
//   print_num(p^.column^.size); print_string(" choices for ");
	add x22, fp, #64
	ldr w23, =32
	ldr x0, [x22]
	ldr x0, [x0, w23, SXTW]
	ldr w0, [x0, #12]
	bl print_num
	mov w1, #14
	ldr x0, =g4
	bl print_string
//   PrintCol(p^.column); newline()
	ldr x0, [x22]
	ldr x0, [x0, w23, SXTW]
	bl _PrintCol
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc ColumnLink(r: Column; var p: Cell);
_ColumnLink:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   new(q);
	mov w0, #40
	bl new
	mov x20, x0
//   if p = nil then
	add x21, fp, #72
	ldr x0, [x21]
	ldr x0, [x0]
	cbnz x0, .L10
//     q^.right := q; q^.left := q; p := q
	str x20, [x20, #24]
	str x20, [x20, #16]
	ldr x0, [x21]
	str x20, [x0]
	b .L11
.L10:
//     q^.left := p^.left; q^.right := p;
	add x21, fp, #72
	ldr w22, =16
	ldr x0, [x21]
	ldr x0, [x0]
	ldr x0, [x0, w22, SXTW]
	str x0, [x20, w22, SXTW]
	ldr w23, =24
	ldr x0, [x21]
	ldr x0, [x0]
	str x0, [x20, w23, SXTW]
//     p^.left^.right := q; p^.left := q
	ldr x0, [x21]
	ldr x0, [x0]
	ldr x0, [x0, w22, SXTW]
	str x20, [x0, w23, SXTW]
	ldr x0, [x21]
	ldr x0, [x0]
	str x20, [x0, w22, SXTW]
.L11:
//   q^.up := r^.head^.up; q^.down := r^.head;
	add x21, fp, #64
	ldr w22, =40
	ldr w23, =0
	ldr x0, [x21]
	ldr x0, [x0, w22, SXTW]
	ldr x0, [x0, w23, SXTW]
	str x0, [x20, w23, SXTW]
	ldr w24, =8
	ldr x0, [x21]
	ldr x0, [x0, w22, SXTW]
	str x0, [x20, w24, SXTW]
//   r^.head^.up^.down := q; r^.head^.up := q;
	ldr x0, [x21]
	ldr x0, [x0, w22, SXTW]
	ldr x0, [x0, w23, SXTW]
	str x20, [x0, w24, SXTW]
	ldr x0, [x21]
	ldr x0, [x0, w22, SXTW]
	str x20, [x0, w23, SXTW]
//   q^.column := r; r^.size := r^.size+1
	ldr x0, [x21]
	str x0, [x20, #32]
	ldr x21, [x21]
	ldr w22, =12
	ldr w0, [x21, w22, SXTW]
	add w0, w0, #1
	str w0, [x21, w22, SXTW]
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc MakeArray(var a: array N of array N of Column; 
_MakeArray:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x25, x26, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #16
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   for i := 0 to m-1 do
	mov w20, wzr
	ldr w0, [fp, #112]
	sub w0, w0, #1
	str w0, [fp, #28]
.L13:
	ldr w0, [fp, #28]
	cmp w20, w0
	bgt .L12
//     for j := 0 to n-1 do
	mov w21, wzr
	ldr w0, [fp, #120]
	sub w23, w0, #1
.L15:
	cmp w21, w23
	bgt .L16
//       new(p); p^.name := name; p^.x := i+1; p^.y := j+1; 
	mov w0, #48
	bl new
	mov x22, x0
	ldrb w0, [fp, #104]
	strb w0, [x22]
	add w0, w20, #1
	str w0, [x22, #4]
	add w0, w21, #1
	str w0, [x22, #8]
//       p^.size := 0; p^.covered := false;
	str wzr, [x22, #12]
	strb wzr, [x22, #16]
//       new(p^.head); p^.head^.down := p^.head; p^.head^.up := p^.head;
	mov w0, #40
	bl new
	ldr w1, =40
	add x24, x22, w1, SXTW
	str x0, [x24]
	str x0, [x0, #8]
	ldr x24, [x24]
	str x24, [x24]
//       p^.prev := root^.prev; p^.next := root;
	ldr x24, =_root
	ldr w25, =24
	ldr x0, [x24]
	ldr x0, [x0, w25, SXTW]
	str x0, [x22, w25, SXTW]
	ldr w26, =32
	ldr x0, [x24]
	str x0, [x22, w26, SXTW]
//       root^.prev^.next := p; root^.prev := p;
	ldr x0, [x24]
	ldr x0, [x0, w25, SXTW]
	str x22, [x0, w26, SXTW]
	ldr x0, [x24]
	str x22, [x0, w25, SXTW]
//       a[i][j] := p
	ldr x0, [fp, #96]
	mov w1, #72
	mul w1, w20, w1
	add x0, x0, w1, SXTW
	str x22, [x0, w21, SXTW #3]
	add w21, w21, #1
	b .L15
.L16:
	add w20, w20, #1
	b .L13
.L12:
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ldp x25, x26, [sp], #16
	add sp, sp, #32
	ret
	.pool

// proc MakeMove(i, j, k: integer);
_MakeMove:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #16
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   p := nil;
	add x20, fp, #24
	str xzr, [x20]
//   ColumnLink(boardCell[i][j], p);
	mov x1, x20
	ldr x0, =_boardCell
	ldr w2, [fp, #64]
	mov w3, #72
	mul w2, w2, w3
	add x0, x0, w2, SXTW
	ldr w2, [fp, #72]
	ldr x0, [x0, w2, SXTW #3]
	bl _ColumnLink
//   ColumnLink(boardColumn[j][k], p);
	mov x1, x20
	ldr x0, =_boardColumn
	ldr w2, [fp, #72]
	mov w3, #72
	mul w2, w2, w3
	add x0, x0, w2, SXTW
	ldr w2, [fp, #80]
	ldr x0, [x0, w2, SXTW #3]
	bl _ColumnLink
//   ColumnLink(boardRow[i][k], p);
	mov x1, x20
	ldr x0, =_boardRow
	ldr w2, [fp, #64]
	mov w3, #72
	mul w2, w2, w3
	add x0, x0, w2, SXTW
	ldr w2, [fp, #80]
	ldr x0, [x0, w2, SXTW #3]
	bl _ColumnLink
//   ColumnLink(boardBlock[sqrtN * (i div sqrtN) + j div sqrtN][k], p);
	mov w1, #3
	ldr w0, [fp, #64]
	bl int_div
	mov w1, #3
	mov x21, x0
	ldr w0, [fp, #72]
	bl int_div
	mov x1, x20
	mov x22, x0
	ldr x0, =_boardBlock
	mov w2, #3
	mul w2, w21, w2
	add w2, w2, w22
	mov w3, #72
	mul w2, w2, w3
	add x0, x0, w2, SXTW
	ldr w2, [fp, #80]
	ldr x0, [x0, w2, SXTW #3]
	bl _ColumnLink
//   boardMove[i][j][k] := p
	ldr x0, [x20]
	ldr x1, =_boardMove
	ldr w2, [fp, #64]
	mov w3, #648
	mul w2, w2, w3
	add x1, x1, w2, SXTW
	ldr w2, [fp, #72]
	mov w3, #72
	mul w2, w2, w3
	add x1, x1, w2, SXTW
	ldr w2, [fp, #80]
	str x0, [x1, w2, SXTW #3]
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #32
	ret
	.pool

// proc MakePuzzle();
_MakePuzzle:
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #16
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   new(root);
	mov w0, #48
	bl new
	ldr x24, =_root
	str x0, [x24]
//   root^.prev := root; root^.next := root;
	str x0, [x0, #24]
	ldr x24, [x24]
	str x24, [x24, #32]
//   MakeArray(boardCell, 'Q', N, N);
	mov w3, #9
	mov w2, #9
	mov w1, #81
	ldr x0, =_boardCell
	bl _MakeArray
//   MakeArray(boardColumn, 'C', N, N);
	mov w3, #9
	mov w2, #9
	mov w1, #67
	ldr x0, =_boardColumn
	bl _MakeArray
//   MakeArray(boardRow, 'R', N, N);
	mov w3, #9
	mov w2, #9
	mov w1, #82
	ldr x0, =_boardRow
	bl _MakeArray
//   MakeArray(boardBlock, 'B', N, N);
	mov w3, #9
	mov w2, #9
	mov w1, #66
	ldr x0, =_boardBlock
	bl _MakeArray
//   for i := 0 to N-1 do
	mov w20, wzr
	mov w0, #8
	str w0, [fp, #24]
.L19:
	ldr w0, [fp, #24]
	cmp w20, w0
	bgt .L18
//     for j := 0 to N-1 do
	mov w21, wzr
	mov w0, #8
	str w0, [fp, #28]
.L21:
	ldr w0, [fp, #28]
	cmp w21, w0
	bgt .L22
//       for k := 0 to N-1 do
	mov w22, wzr
	mov w23, #8
.L23:
	cmp w22, w23
	bgt .L24
//         MakeMove(i, j, k);
	mov w2, w22
	mov w1, w21
	mov w0, w20
	bl _MakeMove
//       end
	add w22, w22, #1
	b .L23
.L24:
	add w21, w21, #1
	b .L21
.L22:
	add w20, w20, #1
	b .L19
.L18:
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ret
	.pool

// proc Cover(p: Column);
_Cover:
	stp x0, x1, [sp, -16]!
	stp x25, x26, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   p^.covered := true;
	add x22, fp, #80
	mov w0, #1
	ldr x1, [x22]
	strb w0, [x1, #16]
//   p^.prev^.next := p^.next; p^.next^.prev := p^.prev;
	ldr x23, [x22]
	ldr w24, =32
	ldr w25, =24
	ldr x0, [x23, w24, SXTW]
	ldr x1, [x23, w25, SXTW]
	str x0, [x1, w24, SXTW]
	ldr x23, [x22]
	ldr x0, [x23, w25, SXTW]
	ldr x1, [x23, w24, SXTW]
	str x0, [x1, w25, SXTW]
//   q := p^.head^.down;
	ldr x0, [x22]
	ldr x0, [x0, #40]
	ldr x20, [x0, #8]
.L26:
//   while q <> p^.head do
	ldr x0, [fp, #80]
	ldr x0, [x0, #40]
	cmp x20, x0
	beq .L25
//     r := q^.right;
	ldr x21, [x20, #24]
.L29:
//     while r <> q do
	cmp x21, x20
	beq .L31
//       r^.up^.down := r^.down; r^.down^.up := r^.up;
	ldr w22, =8
	add x23, x21, w22, SXTW
	ldr w24, =0
	add x25, x21, w24, SXTW
	ldr x0, [x23]
	ldr x1, [x25]
	str x0, [x1, w22, SXTW]
	ldr x0, [x25]
	ldr x1, [x23]
	str x0, [x1, w24, SXTW]
//       r^.column^.size := r^.column^.size-1; r := r^.right
	ldr x22, [x21, #32]
	ldr w23, =12
	ldr w0, [x22, w23, SXTW]
	sub w0, w0, #1
	str w0, [x22, w23, SXTW]
	ldr x21, [x21, #24]
	b .L29
.L31:
//     q := q^.down
	ldr x20, [x20, #8]
	b .L26
.L25:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ldp x25, x26, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc Uncover(p: Column);
_Uncover:
	stp x0, x1, [sp, -16]!
	stp x25, x26, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   p^.prev^.next := p; p^.next^.prev := p;
	add x22, fp, #80
	ldr x23, [x22]
	ldr w24, =24
	ldr w25, =32
	ldr x0, [x23, w24, SXTW]
	str x23, [x0, w25, SXTW]
	ldr x23, [x22]
	ldr x0, [x23, w25, SXTW]
	str x23, [x0, w24, SXTW]
//   q := p^.head^.up;
	ldr x0, [x22]
	ldr x0, [x0, #40]
	ldr x20, [x0]
.L33:
//   while q <> p^.head do
	ldr x0, [fp, #80]
	ldr x0, [x0, #40]
	cmp x20, x0
	beq .L35
//     r := q^.left;
	ldr x21, [x20, #16]
.L36:
//     while r <> q do
	cmp x21, x20
	beq .L38
//       r^.up^.down := r; r^.down^.up := r;
	ldr w22, =0
	ldr w23, =8
	ldr x0, [x21, w22, SXTW]
	str x21, [x0, w23, SXTW]
	ldr x0, [x21, w23, SXTW]
	str x21, [x0, w22, SXTW]
//       r^.column^.size := r^.column^.size+1; r := r^.left
	ldr x22, [x21, #32]
	ldr w23, =12
	ldr w0, [x22, w23, SXTW]
	add w0, w0, #1
	str w0, [x22, w23, SXTW]
	ldr x21, [x21, #16]
	b .L36
.L38:
//     q := q^.up
	ldr x20, [x20]
	b .L33
.L35:
//   p^.covered := false
	ldr x0, [fp, #80]
	strb wzr, [x0, #16]
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ldp x25, x26, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc ChooseColumn(): Column;
_ChooseColumn:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   col := root^.next;
	ldr w22, =32
	ldr x0, =_root
	ldr x0, [x0]
	ldr x21, [x0, w22, SXTW]
//   c := col^.next;
	ldr x20, [x21, w22, SXTW]
.L40:
//   while c <> root do
	ldr x0, =_root
	ldr x0, [x0]
	cmp x20, x0
	beq .L42
//     if c^.size < col^.size then col := c end;
	ldr w22, =12
	ldr w0, [x20, w22, SXTW]
	ldr w1, [x21, w22, SXTW]
	cmp w0, w1
	bge .L45
	mov x21, x20
.L45:
//     c := c^.next
	ldr x20, [x20, #32]
	b .L40
.L42:
//   return col
	mov x0, x21
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// proc PrintState(level: integer);
_PrintState:
	stp x0, x1, [sp, -16]!
	stp x25, x26, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #112
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   for i := 0 to N-1 do
	mov w20, wzr
	mov w0, #8
	str w0, [fp, #36]
.L47:
	ldr w0, [fp, #36]
	cmp w20, w0
	bgt .L48
//     for j := 0 to N-1 do
	mov w21, wzr
	mov w0, #8
	str w0, [fp, #40]
.L49:
	ldr w0, [fp, #40]
	cmp w21, w0
	bgt .L50
//       board[i][j] := '.'
	mov w0, #46
	add x1, fp, #47
	mov w2, #9
	mul w2, w20, w2
	add x1, x1, w2, SXTW
	strb w0, [x1, w21, SXTW]
	add w21, w21, #1
	b .L49
.L50:
	add w20, w20, #1
	b .L47
.L48:
//   for k := 0 to level-1 do
	mov w22, wzr
	ldr w0, [fp, #192]
	sub w0, w0, #1
	str w0, [fp, #32]
.L51:
	ldr w0, [fp, #32]
	cmp w22, w0
	bgt .L52
//     p := choice[k];
	ldr x0, =_choice
	ldr x23, [x0, w22, SXTW #3]
.L53:
//     while p^.column^.name <> 'Q' do p := p^.right end;
	ldr x0, [x23, #32]
	ldrb w0, [x0]
	cmp w0, #81
	beq .L55
	ldr x23, [x23, #24]
	b .L53
.L55:
//     i := p^.column^.x - 1; j := p^.column^.y - 1;
	ldr w24, =32
	ldr x25, [x23, w24, SXTW]
	ldr w0, [x25, #4]
	sub w20, w0, #1
	ldr w26, =8
	ldr w0, [x25, w26, SXTW]
	sub w21, w0, #1
//     board[i][j] := chr(p^.right^.column^.y + ord('0'))
	ldr x0, [x23, #24]
	ldr x0, [x0, w24, SXTW]
	ldr w0, [x0, w26, SXTW]
	add w0, w0, #48
	add x1, fp, #47
	mov w2, #9
	mul w2, w20, w2
	add x1, x1, w2, SXTW
	strb w0, [x1, w21, SXTW]
	add w22, w22, #1
	b .L51
.L52:
//   for i := 0 to N-1 do
	mov w20, wzr
	mov w0, #8
	str w0, [fp, #28]
.L56:
	ldr w0, [fp, #28]
	cmp w20, w0
	bgt .L46
//     print_string(board[i]); newline()
	mov w1, #9
	add x0, fp, #47
	mov w2, #9
	mul w2, w20, w2
	add x0, x0, w2, SXTW
	bl print_string
	bl newline
	add w20, w20, #1
	b .L56
.L46:
	ldp fp, lr, [sp], #16
	add sp, sp, #112
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	ldp x25, x26, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc Solve(level: integer);
_Solve:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   if root^.next = root then
	ldr x0, =_root
	ldr x23, [x0]
	ldr x0, [x23, #32]
	cmp x0, x23
	bne .L61
//     print_string("Solution:"); newline();
	mov w1, #10
	ldr x0, =g5
	bl print_string
	bl newline
//     PrintState(level); return
	ldr w0, [fp, #64]
	bl _PrintState
	b .L58
.L61:
//   col := ChooseColumn();
	bl _ChooseColumn
	mov x20, x0
//   if col^.size = 0 then return end;
	ldr w0, [x20, #12]
	cbz w0, .L58
//   Cover(col);
	mov x0, x20
	bl _Cover
//   p := col^.head^.down;
	ldr x0, [x20, #40]
	ldr x21, [x0, #8]
.L65:
//   while p <> col^.head do
	ldr x0, [x20, #40]
	cmp x21, x0
	beq .L67
//     choice[level] := p;
	ldr w23, [fp, #64]
	ldr x0, =_choice
	str x21, [x0, w23, SXTW #3]
//     print_num(level); print_string(":"); PrintRow(p);
	mov x0, x23
	bl print_num
	mov w1, #2
	ldr x0, =g6
	bl print_string
	mov x0, x21
	bl _PrintRow
//     q := p^.right;
	ldr x22, [x21, #24]
.L68:
//     while q <> p do Cover(q^.column); q := q^.right end;
	cmp x22, x21
	beq .L70
	ldr x0, [x22, #32]
	bl _Cover
	ldr x22, [x22, #24]
	b .L68
.L70:
//     Solve(level+1);
	ldr w0, [fp, #64]
	add w0, w0, #1
	bl _Solve
//     q := p^.left;
	ldr x22, [x21, #16]
.L71:
//     while q <> p do Uncover(q^.column); q := q^.left end;
	cmp x22, x21
	beq .L73
	ldr x0, [x22, #32]
	bl _Uncover
	ldr x22, [x22, #16]
	b .L71
.L73:
//     p := p^.down
	ldr x21, [x21, #8]
	b .L65
.L67:
//   Uncover(col)
	mov x0, x20
	bl _Uncover
.L58:
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc ChooseRow(var level: integer; p: Cell);
_ChooseRow:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   choice[level] := p; level := level+1;
	add x21, fp, #56
	ldr x22, [fp, #48]
	ldr x0, [x21]
	ldr x1, =_choice
	ldr w2, [x22]
	str x0, [x1, w2, SXTW #3]
	ldr w0, [x22]
	add w0, w0, #1
	str w0, [x22]
//   q := p;
	ldr x20, [x21]
.L75:
//     if q^.column^.covered then
	ldr w21, =32
	ldr x0, [x20, w21, SXTW]
	ldrb w0, [x0, #16]
	cbz w0, .L79
//       print_string("Conflict for "); PrintCol(q^.column); newline()
	mov w1, #14
	ldr x0, =g7
	bl print_string
	ldr x0, [x20, w21, SXTW]
	bl _PrintCol
	bl newline
.L79:
//     Cover(q^.column); q := q^.right
	ldr x0, [x20, #32]
	bl _Cover
	ldr x20, [x20, #24]
	ldr x0, [fp, #56]
	cmp x20, x0
	bne .L75
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc Input(var level: integer);
_Input:
	stp x0, x1, [sp, -16]!
	stp x23, x24, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	sub sp, sp, #16
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   for i := 0 to N-1 do
	mov w20, wzr
	mov w0, #8
	str w0, [fp, #24]
.L81:
	ldr w0, [fp, #24]
	cmp w20, w0
	bgt .L80
//     for j := 0 to N-1 do
	mov w21, wzr
	mov w0, #8
	str w0, [fp, #28]
.L83:
	ldr w0, [fp, #28]
	cmp w21, w0
	bgt .L84
//       ch := input[10*i+j];
	ldr x0, =g8
	mov w1, #10
	mul w1, w20, w1
	add x0, x0, w1, SXTW
	ldrb w23, [x0, w21, SXTW]
//       print_char(ch);
	mov w0, w23
	bl print_char
//       if ch <> '.' then
	cmp w23, #46
	beq .L87
//         k := ord(ch) - ord('1');
	sub w22, w23, #49
// 	ChooseRow(level, boardMove[i][j][k])
	ldr x0, =_boardMove
	mov w1, #648
	mul w1, w20, w1
	add x0, x0, w1, SXTW
	mov w1, #72
	mul w1, w21, w1
	add x0, x0, w1, SXTW
	ldr x1, [x0, w22, SXTW #3]
	ldr x0, [fp, #80]
	bl _ChooseRow
.L87:
	add w21, w21, #1
	b .L83
.L84:
//     newline()
	bl newline
	add w20, w20, #1
	b .L81
.L80:
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x23, x24, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   MakePuzzle();
	bl _MakePuzzle
//   level := 0;
	ldr x20, =_level
	str wzr, [x20]
//   Input(level);
	mov x0, x20
	bl _Input
//   Solve(level)
	ldr w0, [x20]
	bl _Solve
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

	.comm _root, 8, 4
	.comm _boardCell, 648, 4
	.comm _boardColumn, 648, 4
	.comm _boardRow, 648, 4
	.comm _boardBlock, 648, 4
	.comm _boardMove, 5832, 4
	.comm _choice, 648, 4
	.comm _level, 4, 4
	.section .rodata
g1:
	.byte 32
	.byte 0
g2:
	.byte 59, 32, 35, 32
	.byte 0
g3:
	.byte 32, 111, 102, 32
	.byte 0
g4:
	.byte 32, 99, 104, 111, 105, 99, 101, 115, 32, 102
	.byte 111, 114, 32
	.byte 0
g5:
	.byte 83, 111, 108, 117, 116, 105, 111, 110, 58
	.byte 0
g6:
	.byte 58
	.byte 0
g7:
	.byte 67, 111, 110, 102, 108, 105, 99, 116, 32, 102
	.byte 111, 114, 32
	.byte 0
g8:
	.byte 46, 46, 51, 46, 46, 46, 46, 53, 49, 47
	.byte 53, 46, 50, 46, 46, 54, 52, 46, 46, 47
	.byte 46, 46, 55, 46, 53, 46, 46, 46, 46, 47
	.byte 46, 46, 46, 54, 51, 46, 55, 46, 46, 47
	.byte 50, 46, 46, 55, 46, 56, 46, 46, 54, 47
	.byte 46, 46, 52, 46, 50, 49, 46, 46, 46, 47
	.byte 46, 46, 46, 46, 55, 46, 56, 46, 46, 47
	.byte 46, 46, 56, 49, 46, 46, 54, 46, 57, 47
	.byte 49, 55, 46, 46, 46, 46, 53, 46, 46
	.byte 0
// End
