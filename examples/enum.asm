  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  mov %r1, %r12  # span 74..87 "Pagoda::Z(...)"
  mov.w %r2, %r12
  load.w %r0, $16
  add.w %r2, %r0
  movi %r0, $12
  mov.w %r1, %r2
  trap
  mov.w %r12, %r2
  # Store tag 2
  movi %r0, $2
  store.w %r0, 0(%r1)
  # Store variant data
  push.w %r1
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
