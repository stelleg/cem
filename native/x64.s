# Register assignments
# rax = environment/literal
# rbp = the bottom of the stack (shouldn't be needed)
# rbx = free env 
# r14 = env start (don't need these pinned)
# r15 = env end
# r12, r13 = temporaries
# We keep the argument registers open to avoid needing to save them for calls

.macro APP ind code
\ind:
  push %rax
  push $\code
  jmp next_\ind
entered_\ind:
  cmp $0, %r12
  je \ind
  push %r12
  push $0
  jmp \ind
next_\ind:
.endm

.macro UNUSED_LAM ind 
entered_\ind:
\ind:
VALUE val_\ind $255
noupdate_\ind:
  addq $16, %rsp
next_\ind:
.endm

.macro LAM ind 
entered_\ind:
\ind:
VALUE val_\ind $255
noupdate_\ind:
  cmp $0, %rbx
  jne apply_\ind
  call gc
apply_\ind:
  movq (%rbx), %r12
  pop (%rbx)
  pop 8(%rbx)
  movq %rax, 16(%rbx)
  movq %rbx, %rax
  movq %r12, %rbx
.endm

.macro CONST ind val
entered_\ind:
\ind:
  movq $\val, %rax
  LIT prim_result_\ind
.endm

# Variable sets %r12 to entered location, and thunks push an update to said
# location
.macro VAR ind var
\ind:
  mov $\var, %r12
enter_\ind:
  cmp $0, %r12
  je enter_end_\ind
  movq 16(%rax), %rax 
  dec %r12
  jmp enter_\ind
enter_end_\ind:
  movq %rax, %r12
  movq 8(%rax), %rax
  jmp *(%r12)
entered_\ind:
  cmp $0, %r12
  je \ind
  push %r12
  push $0
  jmp \ind
.endm

.macro TAIL_VAR ind var
\ind:
  mov $\var, %r12
enter_\ind:
  cmp $0, %r12
  je enter_end_\ind
  movq 16(%rax), %rax 
  dec %r12
  jmp enter_\ind
enter_end_\ind:
  movq (%rax), %r13
  movq %rbx, (%rax) # frees the heap
  movq %rax, %rbx
  movq 8(%rax), %rax
  movq $0, %r12
  jmp *%r13
.endm

.macro VALUE ind val
\ind:
  cmp %rsp, %rbp
  jne update_\ind
  movq \val, %rdi
  movq $60, %rax
  syscall
update_\ind:
  cmpq $0, (%rsp)
  jne apply_\ind
  movq 8(%rsp), %r12
  movq $\ind, (%r12)
  movq %rax, 8(%r12)
  add $16, %rsp
  jmp \ind # TODO: remove if collapsed markers
apply_\ind:
.endm

.macro LIT ind
entered_\ind:
\ind:
  VALUE val_\ind %rax
  pop %r11
  pop %r13
  push %rax
  push $entered_\ind 
  mov $0, %r12
  mov %r13, %rax
  jmp *%r11
.endm

# Loads variables into registers for syscalls, calls, and primops. Note that by
# making syscalls and calls have varargs, we have implicitly required that the
# number of args is checked at runtime. An eventual optimization (and the
# general use case) will be solving for this before compilation to native code. 
# We also free the heap space on loadvar.
.macro LOADVAR reg
  movq %rbx, (%rax)
  movq %rax, %rbx
  movq 8(%rax), \reg
  movq 16(%rax), %rax 
.endm 

.macro OP_ADD ind
\ind:
  BINOP \ind
  add %rsi, %rdi
  mov %rdi, %rax
  LIT prim_result_\ind
.endm

.macro OP_SUB ind
\ind:
  BINOP \ind
  sub %rsi, %rdi
  mov %rdi, %rax
  LIT prim_result_\ind
.endm

.macro OP_MUL ind
\ind:
  BINOP \ind
  mov %rsi, %rax
  mul %rdi
  LIT prim_result_\ind
.endm

.macro OP_DIV ind
\ind:
  BINOP \ind
  mov $0, %rdx
  mov %rdi, %rax
  divq %rsi 
  LIT prim_result_\ind
.endm

.macro OP_MOD ind
\ind:
  BINOP \ind
  mov $0, %rdx
  mov %rdi, %rax
  divq %rsi 
  mov %rdx, %rax
  LIT prim_result_\ind
.endm

.macro BINOP ind
entered_\ind:
  mov 8(%rsp), %rsi
  mov 24(%rsp), %rdi
  add $32, %rsp
.endm

.macro BINBOOL ind cmp
\ind:
  BINOP \ind
  cmp %rsi, %rdi
  \cmp true_\ind
  FALSE \ind
true_\ind:
  TRUE \ind
.endm

.macro OP_EQ ind
  BINBOOL \ind je
.endm

.macro OP_NEQ ind
  BINBOOL \ind jne
.endm

.macro OP_LT ind
  BINBOOL \ind jl
.endm

.macro OP_LTE ind 
  BINBOOL \ind jle
.endm

.macro OP_GT ind
  BINBOOL \ind jg
.endm

.macro OP_GTE ind
  BINBOOL \ind jge
.endm

.macro TRUE ind
  LAM lamtrue1_\ind
  UNUSED_LAM lamtrue2_\ind
  TAIL_VAR vartrue_\ind 0
.endm

.macro FALSE ind
  UNUSED_LAM lamfalse1_\ind
  LAM lamfalse2_\ind
  TAIL_VAR varfalse_\ind 0
.endm

.macro OP_SYSCALL ind
\ind:
  movq 8(%rax), %rax 
  syscall
  LIT syscall_lit_\ind
.endm

.macro OP_WRITEq ind
entered_\ind:
\ind:
  LAM write_val_\ind
  LAM write_loc_\ind
  movq $0, %rdi
  LOADVAR %rdi
  movq $0, %rsi
  LOADVAR %rsi
  movq %rdi, (%rsi)
  addq $16, %rsp
  movq -8(%rsp), %rax
  movq $0, %r12
  jmp *-16(%rsp)
.endm

.macro OP_WRITEl ind
entered_\ind:
\ind:
  LAM write_val_\ind
  LAM write_loc_\ind
  movq $0, %rdi
  LOADVAR %rdi
  movq $0, %rsi
  LOADVAR %rsi
  movl %edi, (%rsi)
  addq $16, %rsp
  movq -8(%rsp), %rax
  movq $0, %r12
  jmp *-16(%rsp)
.endm

.macro OP_WRITEs ind
entered_\ind:
\ind:
  LAM write_val_\ind
  LAM write_loc_\ind
  movq $0, %rdi
  LOADVAR %rdi
  movq $0, %rsi
  LOADVAR %rsi
  mov %di, (%rsi)
  addq $16, %rsp
  movq -8(%rsp), %rax
  movq $0, %r12
  jmp *-16(%rsp)
.endm

.macro OP_WRITEb ind
entered_\ind:
\ind:
  LAM write_val_\ind
  LAM write_loc_\ind
  movq $0, %rdi
  LOADVAR %rdi
  movq $0, %rsi
  LOADVAR %rsi
  movb %dil, (%rsi)
  addq $16, %rsp
  movq -8(%rsp), %rax
  movq $0, %r12
  jmp *-16(%rsp)
.endm

.macro OP_READq ind
entered_\ind:
\ind:
  LAM read_val_\ind
  LOADVAR %rdi
  movq $0, %rax
  mov (%rdi), %rax
  LIT read_lit_\ind
.endm

.macro OP_READl ind
entered_\ind:
\ind:
  LAM read_val_\ind
  LOADVAR %rdi
  movq $0, %rax
  mov (%rdi), %eax
  LIT read_lit_\ind
.endm

.macro OP_READs ind
entered_\ind:
\ind:
  LAM read_val_\ind
  LOADVAR %rdi
  movq $0, %rax
  mov (%rdi), %ax
  LIT read_lit_\ind
.endm

.macro OP_READb ind
entered_\ind:
\ind:
  LAM read_val_\ind
  LOADVAR %rdi
  movq $0, %rax
  mov (%rdi), %al
  LIT read_lit_\ind
.endm

.text
  .global _start

LIT envp
LIT argv
LIT argc

alloc_heap:
  movq $0, %rdi
  movq $40000000, %rsi
  movq $255, %rdx
  movq $0x22, %r10
  movq $-1, %r8
  movq $0, %r9
  movq $9, %rax
  syscall
  movq %rax, %rbx # Free env ptr
  movq %rax, %r13 # Tmp loop var
  movq %rax, %r14 # Env start
  movq %rax, %r15 
  add %rsi, %r15 # Env end
# this is important, we leave some free space at the end for garbage updates
  sub $64, %r15
freeHeap_loop:
  add $24, %r13
  cmp %r13, %r15
  jbe  alloc_heap_ret
  mov %r13, -24(%r13)
  jmp freeHeap_loop
alloc_heap_ret:
  movq $0, (%r13)
  ret

_start:
  # mmap call
  call alloc_heap 
  movq %rsp, %rbp
# Push the argc and argv onto the stack
  addq $24, %rbp
  pushq %rbp
  pushq $envp
  subq $16, %rbp
  pushq %rbp
  pushq $argv
  subq $8, %rbp
  pushq (%rbp)
  pushq $argc
  movq $0, %rax # set initial environment to null

