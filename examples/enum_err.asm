  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  mov %r1, %r12  # span 66..81 "Result::Err(...)"
  mov.w %r2, %r12
  load.w %r0, $16
  add.w %r2, %r0
  movi %r0, $12
  mov.w %r1, %r2
  trap
  mov.w %r12, %r2
  # Store tag 1
  movi %r0, $1
  store.w %r0, 0(%r1)
  # Store variant data
  push.w %r1
  load.w %r0, $99  # span 78..80 "99"
  pop.w %r1
  store.w %r0, 8(%r1)
  mov.w %r0, %r1
  push.w %r0
  load.w %r0, 0(%sp)  # span 98..99 "r"
  push.w %r0
  # Load enum tag
  load.w %r1, 0(%r0)
  # Check variant Ok
  xor.w %r2, %r2
  breq.w %r1, %r2, label_0
  # Check variant Err
  movi %r2, $1
  breq.w %r1, %r2, label_1
  trap  # Non-exhaustive match
label_0:
  # Bind variable value
  load.w %r0, 8(%sp)
  load.w %r0, 8(%r0)
  push.w %r0
  load.w %r0, 0(%sp)  # span 125..130 "value"
  pop.w %r7
  jmp label_2
label_1:
  # Bind variable code
  load.w %r0, 8(%sp)
  load.w %r0, 8(%r0)
  push.w %r0
  load.w %r0, 0(%sp)  # span 155..159 "code"
  pop.w %r7
  jmp label_2
label_2:
  pop.w %r7
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
