  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  load.l %r0, $0  # span 26..27 "0"
  push.l %r0
  load.l %r0, 0(%sp)  # span 38..39 "x"
  load.w %r7, $4
  add.w %sp, %r7
  jmp fn_main_ret
  load.w %r7, $4
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
