# r14, r15 = env_start, env_end
# r9 = free_count
# r10 stack gc pointer
# r11 tmp
# r8 heap gc pointer

gc:
  call gc_cc
  call gc_stack
  call gc_free_stack
  call gc_free
  ret

gc_cc:
  # we know gc will only be called from lambda, so no need for lit check
  mov %rax, %r8
  call gc_mark
  ret

# Marks all closures on stack as live
gc_stack:
  mov %rsp, %r10
gc_stack_loop:
  add $16, %r10
  cmp %r10, %rbp
  jbe gc_stack_ret
  cmp $0, (%r10) # Check if update
  je gc_stack_loop 
# If we get here, it is a closure with an environment pointer.
  mov 8(%r10), %r8
  call gc_mark
  jmp gc_stack_loop
gc_stack_ret:
  ret
  
gc_mark: 
# HACK: Check if env is in heap. Could fail if integer in heap range.
  cmpq %r14, %r8
  jb gc_mark_ret
  cmpq %r15, %r8
  jae gc_mark_ret
# If we hit something already marked, return
  cmpb $1, 7(%r8)
  je gc_mark_ret
  movb $1, 7(%r8) 
gc_mark_test:
# Lookahead optimization to reduce stack overflows
  mov 16(%r8), %r11
  cmpq %r14, %r11
  jb gc_mark_env
  cmpq %r15, %r11
  jae gc_mark_env
  cmpb $1, 7(%r11)
  je gc_mark_env
gc_mark_both_env:
  push 16(%r8)
  mov 8(%r8), %r8 
  call gc_mark
  pop %r8
  jmp gc_mark
gc_mark_env:
  mov 8(%r8), %r8
  jmp gc_mark
gc_mark_ret:
  ret

gc_free:
  movq $0, %r9
  mov %r14, %r8 
gc_free_loop:
  cmpq %r15, %r8
  jae gc_free_ret
  cmpb $0, 7(%r8)
  je gc_free_cell
  movb $0, 7(%r8)
  add $24, %r8
  jmp gc_free_loop
gc_free_cell:
  mov %rbx, (%r8)
  mov %r8, %rbx 
  add $24, %r8
  inc %r9
  jmp gc_free_loop
gc_free_ret:
  ret

# Just removes updates to free cells.
gc_free_stack:
  mov %rsp, %r10
gc_free_stack_loop:
  add $16, %r10
  cmp %r10, %rbp
  jbe gc_free_stack_ret
  cmp $0, (%r10)
  jne gc_free_stack_loop # Only continue if update marker
gc_free_stack_update:
  mov 8(%r10), %r12 
  cmpb $1, 7(%r12)
  jne gc_update_not_live
# we should collapse the update, but instead we just update a
# meaningless area
gc_update_live:
  jmp gc_free_stack_loop 
gc_update_not_live:
  mov %r15, %r12
  add $24, %r12
  mov %r12, 8(%r10) 
  jmp gc_free_stack_loop
gc_free_stack_ret:
  ret
