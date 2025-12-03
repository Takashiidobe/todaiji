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
  # Store tag 0
  xor.w %r1, %r1
  store.w %r1, 0(%r0)
  # Store variant data
  push.w %r0
  load.w %r0, $100  # span 105..108 "100"
  pop.w %r1
  store.w %r0, 8(%r1)
  mov.w %r0, %r1
  push.w %r0
