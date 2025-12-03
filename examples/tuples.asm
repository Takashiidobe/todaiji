  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  mov %r1, %r12  # span 21..39 "tuple"
  mov.w %r2, %r12
  load.w %r0, $32
  add.w %r2, %r0
  movi %r0, $12
  mov.w %r1, %r2
  trap
  mov.w %r12, %r2
  push.w %r1
  load.w %r0, $1  # span 22..23 "1"
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 0(%r1)  # element 0
  push.w %r1
  load.w %r0, $2  # span 25..26 "2"
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 8(%r1)  # element 1
  push.w %r1
  mov %r1, %r12  # span 28..32 ""hi""
  mov.w %r2, %r12
  load.w %r0, $2
  add.w %r2, %r0
  movi %r0, $12
  mov.w %r1, %r2
  trap
  mov.w %r12, %r2
  load.w %r0, $104
  store.b %r0, 0(%r1)  # span 28..32 ""hi""
  load.w %r0, $105
  store.b %r0, 1(%r1)  # span 28..32 ""hi""
  mov.w %r0, %r1
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 16(%r1)  # element 2
  push.w %r1
  load.w %r0, $1  # span 34..38 "true"
  pop.w %r7
  mov.w %r1, %r7
  store.w %r0, 24(%r1)  # element 3
  mov.w %r0, %r1
  push.w %r0
  load.w %r0, 0(%sp)  # span 50..51 "p"
  load.w %r0, 24(%r0)  # span 50..53 "p.3"
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
