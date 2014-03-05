# r15 : code
# r14 : env
# r13 : stack
# r12 : stack bottom
# r11 : freeEnv
# r0-r3 : tmps

.macro APP ind code
\ind:
  push r14
  push $\code
.endm

.macro LAM ind 
\ind:
  cmp r13, r12
  je exit_failure
  cmp $0, (r13)
  jne noupdate_\ind
  mov r3, 8(r13)
  mov (r3), $\ind
  mov 8(r3), r14
  add $16, r13
  jmp \ind # TODO: remove if collapsed markers
noupdate_\ind:
  mov r3, (r11)
  pop (r11)
  pop 8(r11)
  mov 16(r11), r14
  mov r14, r11
  mov r11, r3
  cmp $0, r11
  jne no_gc_\ind
  call gc
no_gc_\ind:
.endm

.macro VAR ind var
\ind:
  mov r3, $\var
  jmp enter
.endm

.macro CONST ind i
\ind:
  mov r14, $\i
  jmp lit
.endm

.macro OP ind type
\ind:
  jmp op_\type
.endm

.text
  .global _start

enter: 
  cmp $0, r3
  je enter_end
  mov r14, 16(r14) 
  dec r3
  jmp enter
enter_end:
  push r14
  push $0
  mov r3, (r14)
  mov r14, 8(r14)
  jmp *r3

exit_failure:
  mov r0, $-1
  mov r7, $60
  swi $0

exit:
  mov r0, r14
  mov r14, $60
  swi $0

lit:
  cmp r13, r12
  je exit
  cmp $0, (r13)
  jne noupdate_lit
  mov r3, 8(r13)
  mov (r3), $lit
  mov 8(r3), r14
  add $16, r13
  jmp lit
noupdate_lit:
  mov r3, (r13)
  mov r2, 8(r13)
  mov (r13), $lit
  mov 8(r13), r14 
  mov r14, r2
  jmp *r3

op_add:
  mov r3, 8(r13)
  mov r14, 24(r13)
  add r3, r14
  add $32, r13
  jmp lit

op_sub:
  mov r3, 8(r13)
  mov r14, 24(r13)
  sub r3, r14
  add $32, r13
  jmp lit

op_mul:
  mov r2, 8(r13)
  mov r14, 24(r13)
  mul r2
  add $32, r13
  jmp lit

op_div:
  mov r2, $0
  mov r14, 24(r13)
  divq 8(r13)
  add $32, r13
  jmp lit

op_mod:
  mov r2, $0
  mov r14, 24(r13)
  divq 8(r13)
  mov r14, r2
  add $32, r13
  jmp lit

op_eq:
  mov r3, 8(r13)
  mov r14, 24(r13)
  add $32, r13
  cmp r3, r14
  je true
  jmp false

op_lt:
  mov r3, 8(r13)
  mov r14, 24(r13)
  add $32, r13
  cmp r3, r14
  jl true
  jmp false

op_lte:
  mov r3, 8(r13)
  mov r14, 24(r13)
  add $32, r13
  cmp r3, r14
  jle true
  jmp false

op_gt:
  mov r3, 8(r13)
  mov r14, 24(r13)
  add $32, r13
  cmp r3, r14
  jg true 
  jmp false 

op_gte:
  mov r3, 8(r13)
  mov r14, 24(r13)
  add $32, r13
  cmp r3, r14
  jge true 
  jmp false 

op_write:
  leaq 8(r13), r1 
  mov r0, $1
  mov r2, $1
  mov r14, $1
  swi $0 
  add $16, r13
  jmp lit 

op_read:
  pushq $0
  push $lit
  leaq 8(r13), r1
  mov r0, $0
  mov r14, $0
  mov r2, $1
  swi $0 
  mov r3, 16(r13)
  mov r2, 24(r13)
  mov 16(r13), $lit
  mov 24(r13), r14
  mov r14, r2
  jmp *r3
  
true:
  LAM lamtrue1
  LAM lamtrue2
  VAR vartrue 1

false:
  LAM lamfalse1
  LAM lamfalse2
  VAR varfalse 0

alloc_heap:
  mov r0, $0
  mov r1, $0x100000
  mov r2, $255
  mov r3, $0x22
  mov r4, $-1
  mov r5, $0
  mov r7, $9
  swi $0
  mov r11, r0
  mov r2, r0
  sub r0, 24, r0
  add r0, r1
  mov r1, r0
freeHeap_loop:
  add r2, r2, 24
  cmp r2, r0
  bhi alloc_heap_ret
  mov -24(r2), r2
  jmp freeHeap_loop
alloc_heap_ret:
  ret

_start:
  # mmap call
  call alloc_heap 
  mov r12, r13

/*
  APP L0 L5
  APP L1 L4
  LAM L2
  VAR L3 0
  CONST L4 5
  APP L5 L7 
  CONST L6 5
  OP L7 add
*/
