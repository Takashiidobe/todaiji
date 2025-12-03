  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  mov %r1, %r12  # span 35..42 "tuple"
  mov.w %r2, %r12
  load.w %r0, $16
  add.w %r2, %r0
  movi %r0, $12
  mov.w %r1, %r2
  trap
  mov.w %r12, %r2
  push.w %r1
  load.w %r0, $5  # span 36..37 "5"
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 0(%r1)  # element 0
  push.w %r1
  load.w %r0, $10  # span 39..41 "10"
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 8(%r1)  # element 1
  mov.w %r0, %r1
  push.w %r0
  mov %r1, %r12  # span 60..82 "tuple"
  mov.w %r2, %r12
  load.w %r0, $24
  add.w %r2, %r0
  movi %r0, $12
  mov.w %r1, %r2
  trap
  mov.w %r12, %r2
  push.w %r1
  load.w %r0, 8(%sp)  # span 61..66 "inner"
  load.w %r0, 0(%r0)  # span 61..68 "inner.0"
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 0(%r1)  # element 0
  push.w %r1
  load.w %r0, 8(%sp)  # span 70..75 "inner"
  load.w %r0, 8(%r0)  # span 70..77 "inner.1"
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 8(%r1)  # element 1
  push.w %r1
  load.w %r0, $15  # span 79..81 "15"
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 16(%r1)  # element 2
  mov.w %r0, %r1
  push.w %r0
  load.w %r0, 0(%sp)  # span 96..101 "outer"
  load.w %r0, 0(%r0)  # span 96..103 "outer.0"
  push.w %r0
  load.w %r0, 8(%sp)  # span 117..122 "outer"
  load.w %r0, 8(%r0)  # span 117..124 "outer.1"
  push.w %r0
  load.w %r0, 16(%sp)  # span 138..143 "outer"
  load.w %r0, 16(%r0)  # span 138..145 "outer.2"
  push.w %r0
  load.w %r0, 0(%sp)  # span 166..167 "c"
  push.w %r0
  load.w %r0, 16(%sp)  # span 162..163 "b"
  push.w %r0
  load.w %r0, 32(%sp)  # span 158..159 "a"
  pop.w %r1
  add.w %r0, %r1  # span 158..163 "a+b"
  pop.w %r1
  add.w %r0, %r1  # span 158..167 "a+b+c"
  load.w %r7, $40
  add.w %sp, %r7
  jmp fn_main_ret
  load.w %r7, $40
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
