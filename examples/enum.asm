  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  # Allocate enum 16 bytes
  mov.w %r0, %r12
  load.w %r1, $16
  add.w %r12, %r1
  # Store tag 2
  movi %r1, $2
  store.w %r1, 0(%r0)
  # Store variant data
  push.w %r0
  load.w %r0, $10  # span 84..86 "10"
  pop.w %r1
  store.w %r0, 8(%r1)
  mov.w %r0, %r1
  push.w %r0
  load.w %r0, 0(%sp)  # span 98..99 "p"
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_main_ret
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_main_ret
fn_main_ret:
  ret
main:
  call fn_main
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
