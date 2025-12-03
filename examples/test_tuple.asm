  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  mov %r1, %r12  # span 31..43 "tuple"
  mov.w %r2, %r12
  load.w %r0, $24
  add.w %r2, %r0
  movi %r0, $12
  mov.w %r1, %r2
  trap
  mov.w %r12, %r2
  push.w %r1
  load.w %r0, $10  # span 32..34 "10"
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 0(%r1)  # element 0
  push.w %r1
  load.w %r0, $20  # span 36..38 "20"
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 8(%r1)  # element 1
  push.w %r1
  load.w %r0, $30  # span 40..42 "30"
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 16(%r1)  # element 2
  mov.w %r0, %r1
  push.w %r0
  load.w %r0, 0(%sp)  # span 57..58 "t"
  load.w %r0, 0(%r0)  # span 57..60 "t.0"
  push.w %r0
  load.w %r0, 8(%sp)  # span 74..75 "t"
  load.w %r0, 8(%r0)  # span 74..77 "t.1"
  push.w %r0
  load.w %r0, 16(%sp)  # span 91..92 "t"
  load.w %r0, 16(%r0)  # span 91..94 "t.2"
  push.w %r0
  load.w %r0, 0(%sp)  # span 115..116 "z"
  push.w %r0
  load.w %r0, 16(%sp)  # span 111..112 "y"
  push.w %r0
  load.w %r0, 32(%sp)  # span 107..108 "x"
  pop.w %r1
  add.w %r0, %r1  # span 107..112 "x+y"
  pop.w %r1
  add.w %r0, %r1  # span 107..116 "x+y+z"
  load.w %r7, $32
  add.w %sp, %r7
  jmp fn_main_ret
  load.w %r7, $32
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
