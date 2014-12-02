# Register assignments
# rax = environment/literal
# rbp = the bottom of the stack (shouldn't be needed)
# rbx = free env 
# r14 = env start (don't need these pinned)
# r15 = env end
# rsi, rdi, rcx, rdx, r8-r13 = temporaries

.macro THUNK ind
entered_\ind:
  cmp $0, %rcx
  je \ind
  push %rcx
  push $0
  jmp \ind
.endm

# Moves 
.macro APP ind code
\ind:
  push %rax
  push $\code
  jmp next_\ind
THUNK \ind
next_\ind:
.endm

# Pops a closure off the stack
.macro UNUSED_LAM ind 
entered_\ind:
\ind:
VALUE val_\ind $255
noupdate_\ind:
  addq $16, %rsp
next_\ind:
.endm

# Pops a closure from the stack into the environment
.macro LAM ind 
entered_\ind:
\ind:
VALUE val_\ind $255
noupdate_\ind:
  cmp $0, %rbx
  jne apply_\ind
  call gc
apply_\ind:
  movq (%rbx), %rcx
  pop (%rbx)
  pop 8(%rbx)
  movq %rax, 16(%rbx)
  movq %rbx, %rax
  movq %rcx, %rbx
.endm

# Moves a value into env and enters the code for lit
.macro CONST ind val
entered_\ind:
\ind:
  movq $\val, %rax
  LIT const_\ind
.endm

# Sets %rcx to entered location, and thunks push an update to said location
.macro VAR ind var
\ind:
  mov $\var, %rcx
enter_\ind:
  cmp $0, %rcx
  je enter_end_\ind
  movq 16(%rax), %rax 
  dec %rcx
  jmp enter_\ind
enter_end_\ind:
  movq %rax, %rcx
  movq 8(%rax), %rax
  jmp *(%rcx)
THUNK \ind
.endm

# Same as VAR, but also removes from heap. 
.macro TAIL_VAR ind var
entered_\ind:
\ind:
  mov $\var, %rcx
enter_\ind:
  cmp $0, %rcx
  je enter_end_\ind
  movq 16(%rax), %rax 
  dec %rcx
  jmp enter_\ind
enter_end_\ind:
  movq (%rax), %r13
  movq %rbx, (%rax) # frees the heap
  movq %rax, %rbx
  movq 8(%rax), %rax
  movq $0, %rcx
  jmp *%r13
.endm

# Values check to see if there is an update marker on top of the stack, and if
# so, update the appropriate location with themselves.
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
  movq 8(%rsp), %rcx
  movq $\ind, (%rcx)
  movq %rax, 8(%rcx)
  add $16, %rsp
  jmp \ind # TODO: remove if collapsed markers
apply_\ind:
.endm

# Enters the closure on top of the stack with itself in its place
.macro LIT ind
entered_\ind:
\ind:
  VALUE val_\ind %rax
  pop %r11
  pop %r13
  push %rax
  push $entered_\ind 
  mov $0, %rcx
  mov %r13, %rax
  jmp *%r11
.endm

# Enters the closure on top of the stack with itself in its place
.macro WORLD ind
entered_\ind:
\ind:
  VALUE val_\ind $0
  pop %r11
  pop %r13
  push %rax
  push $entered_\ind
  movq $0, %rcx
  movq %r13, %rax
  jmp *%r11
.endm

# Loads variables into registers for syscalls, calls, and primops. Note that by
# making syscalls and calls that have varargs, we have implicitly required that
# the number of args is checked at runtime. An eventual optimization (and the
# general use case) will be solving for this before compilation to native code.
# We also free the heap space on loadvar.
.macro LOADVARENV reg
  movq %rbx, (%rax)
  movq %rax, %rbx
  movq 8(%rax), \reg
  movq 16(%rax), %rax 
.endm 

.macro LOADVAR reg1 reg2
  movq (%rax), \reg1
  movq %rbx, (%rax)
  movq %rax, %rbx
  movq 8(%rax), \reg2
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
  LOADVARENV %rsi
  LOADVARENV %rdi
.endm

.macro BINBOOL ind cmp
\ind:
  BINOP \ind
  LOADVAR %r9 %r10
  LOADVAR %r11 %r13
  movq $0, %rcx
  cmp %rsi, %rdi
  \cmp true_\ind
  movq %r10, %rax
  jmp *%r9
true_\ind:
  movq %r13, %rax
  jmp *%r11
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

.macro OP_WRITE reg ind 
entered_\ind:
\ind:
  movq 16(%rax), %rax  #World
  LOADVARENV %rsi
  LOADVARENV %rdi
  mov %\reg, (%rsi)
  RETURNVAL ret_\ind
.endm

.macro OP_READ reg ind
entered_\ind:
\ind:
  movq 16(%rax), %rax #World
  LOADVARENV %rsi
  movq $0, %rdi
  mov (%rsi), %\reg
  RETURNVAL ret_\ind
.endm

.macro RETURNVAL ind
entered_\ind:
\ind:
  pop %r9
  pop %rax
  push $0
  push $world_\ind
  push %rdi
  push $lit_\ind
  movq $0, %rcx
  jmp *%r9
  WORLD world_\ind
  LIT lit_\ind
.endm

.text
  .global _start

LIT envp
LIT argv
LIT argc

alloc_heap:
  movq $0, %rdi
  movq $4000000000, %rsi
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
  movq $0, %rax # set initial environment to null
  movq $0, %rcx # set initial environment to null
  
#  addq $24, %rbp
#  pushq %rbp
#  pushq $envp
#  subq $16, %rbp
#  pushq %rbp
#  pushq $argv
#  subq $8, %rbp
#  pushq (%rbp)
#  pushq $argc

