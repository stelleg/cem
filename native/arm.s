# rip : code
# rax : env
# rbx : freeEnv
# rcx ; stack bottom
# rsi : envStart
# rdi : envEnd
# rbp : [rdi, rsi, r8 ..]
# rdx,r8-r16 : tmps

.macro APP ind code
\ind:
  push %rax
  push $\code
.endm

.macro LAM ind 
\ind:
  cmp %rsp, %rbp
  je exit_failure
  cmp $0, (%rsp)
  jne noupdate_\ind
  movq 8(%rsp), %rcx
  movq $\ind, (%rcx)
  movq %rax, 8(%rcx)
  add $16, %rsp
  jmp \ind # TODO: remove if collapsed markers
noupdate_\ind:
  movq (%rbx), %rcx
  pop (%rbx)
  pop 8(%rbx)
  movq %rax, 16(%rbx)
  movq %rbx, %rax
  mov %rcx, %rbx
  cmp $0, %rbx
  jne no_gc_\ind
  call gc
no_gc_\ind:
.endm

.macro VAR ind var
\ind:
  movq $\var, %rcx
  jmp enter
.endm

.macro CONST ind i
\ind:
  movq $\i, %rax
  jmp lit
.endm

.macro OP ind type
\ind:
  jmp op_\type
.endm

.text
  .global _start

enter: 
  cmp $0, %rcx
  je enter_end
  movq 16(%rax), %rax 
  dec %rcx
  jmp enter
enter_end:
  push %rax
  push $0
  movq (%rax), %rcx
  movq 8(%rax), %rax
  jmp *%rcx

exit_failure:
  movq $-1, %rdi
  movq $60, %rax
  syscall

exit:
  movq %rax, %rdi
  movq $60, %rax
  syscall

lit:
  cmp %rsp, %rbp
  je exit
  cmp $0, (%rsp)
  jne noupdate_lit
  movq 8(%rsp), %rcx
  movq $lit, (%rcx)
  movq %rax, 8(%rcx)
  add $16, %rsp
  jmp lit
noupdate_lit:
  movq (%rsp), %rcx
  movq 8(%rsp), %rdx
  movq $lit, (%rsp)
  movq %rax, 8(%rsp) 
  movq %rdx, %rax
  jmp *%rcx

op_add:
  movq 8(%rsp), %rcx
  movq 24(%rsp), %rax
  add %rcx, %rax
  add $32, %rsp
  jmp lit

op_sub:
  movq 8(%rsp), %rcx
  movq 24(%rsp), %rax
  sub %rcx, %rax
  add $32, %rsp
  jmp lit

op_mul:
  movq 8(%rsp), %rdx
  movq 24(%rsp), %rax
  mul %rdx
  add $32, %rsp
  jmp lit

op_div:
  movq $0,%rdx
  movq 24(%rsp), %rax
  divq 8(%rsp)
  add $32, %rsp
  jmp lit

op_mod:
  movq $0,%rdx
  movq 24(%rsp), %rax
  divq 8(%rsp)
  mov %rdx, %rax
  add $32, %rsp
  jmp lit

op_eq:
  movq 8(%rsp), %rcx
  movq 24(%rsp), %rax
  add $32, %rsp
  cmp %rcx, %rax
  je true
  jmp false

op_lt:
  movq 8(%rsp), %rcx
  movq 24(%rsp), %rax
  add $32, %rsp
  cmp %rcx, %rax
  jl true
  jmp false

op_lte:
  movq 8(%rsp), %rcx
  movq 24(%rsp), %rax
  add $32, %rsp
  cmp %rcx, %rax
  jle true
  jmp false

op_gt:
  movq 8(%rsp), %rcx
  movq 24(%rsp), %rax
  add $32, %rsp
  cmp %rcx, %rax
  jg true 
  jmp false 

op_gte:
  movq 8(%rsp), %rcx
  movq 24(%rsp), %rax
  add $32, %rsp
  cmp %rcx, %rax
  jge true 
  jmp false 

op_write:
  leaq 8(%rsp), %rsi 
  movq $1, %rdi
  movq $1, %rdx
  movq $1, %rax
  syscall 
  add $16, %rsp
  jmp lit 

op_read:
  pushq $0
  push $lit
  leaq 8(%rsp), %rsi
  movq $0, %rdi
  movq $0, %rax
  movq $1, %rdx
  syscall 
  movq 16(%rsp), %rcx
  movq 24(%rsp), %rdx
  movq $lit, 16(%rsp)
  movq %rax, 24(%rsp)
  movq %rdx, %rax
  jmp *%rcx
  
true:
  LAM lamtrue1
  LAM lamtrue2
  VAR vartrue 1

false:
  LAM lamfalse1
  LAM lamfalse2
  VAR varfalse 0

op_alloc:
  movq $0, %rdi
  pop %rsi
  mov $255, %rdx
  movq $0x22, %r10
  movq $-1, %r8
  movq $0, %r9
  movq $9, %rax
  syscall
  jmp lit

alloc_heap:
  movq $0, %rdi
  mov $255, %rdx
  movq $0x22, %r10
  movq $-1, %r8
  movq $0, %r9
  movq $9, %rax
  syscall
  movq %rax, %rbx
  movq %rax, %rdx
  lea -24(%rax), %rdi
  add %rsi, %rdi
  mov %rax, %rsi
freeHeap_loop:
  add $24, %rdx
  cmp %rdx, %rdi
  jb  alloc_heap_ret
  mov %rdx, -24(%rdx)
  jmp freeHeap_loop
alloc_heap_ret:
  ret

_start:
  # mmap call
  movq $0x100000, %rsi
  call alloc_heap 
  movq %rsp, %rbp

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
