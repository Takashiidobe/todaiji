  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  mov %r1, %r12  # span 56..69 "Point(10,20)"
  mov.w %r2, %r12
  load.w %r0, $16
  add.w %r2, %r0
  movi %r0, $12
  mov.w %r1, %r2
  trap
  mov.w %r12, %r2
  push.w %r1
  load.w %r0, $10  # span 62..64 "10"
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 0(%r1)  # span 56..69 "Point(10,20)"
  push.w %r1
  load.w %r0, $20  # span 66..68 "20"
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 8(%r1)  # span 56..69 "Point(10,20)"
  mov.w %r0, %r1
  push.w %r0
  load.w %r0, 0(%sp)  # span 83..84 "p"
  load.w %r0, 0(%r0)  # span 83..86 "p.0"
  push.w %r0
  load.w %r0, 8(%sp)  # span 100..101 "p"
  load.w %r0, 8(%r0)  # span 100..103 "p.1"
  push.w %r0
  load.w %r0, 0(%sp)  # span 120..121 "y"
  push.w %r0
  load.w %r0, 16(%sp)  # span 116..117 "x"
  pop.w %r1
  add.w %r0, %r1  # span 116..121 "x+y"
  load.w %r7, $24
  add.w %sp, %r7
  jmp fn_main_ret
  load.w %r7, $24
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
