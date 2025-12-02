  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_add:
  push.w %r1
  push.w %r2
  load.w %r0, 0(%sp)  # span 30..31 "b"
  push.w %r0
  load.w %r0, 16(%sp)  # span 26..27 "a"
  pop.w %r1
  add.w %r0, %r1  # span 26..31 "a+b"
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_add_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_add_ret
fn_add_ret:
  ret
fn_main:
  load.w %r0, $10  # span 76..78 "10"
  push.w %r0
  pop.w %r1
  load.w %r0, $20  # span 80..82 "20"
  push.w %r0
  pop.w %r2
  call fn_add  # span 72..83 "add(10,20)"
  push.w %r0
  load.w %r0, 0(%sp)  # span 96..102 "result"
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
