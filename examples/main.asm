  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_add:
  push.w %r1
  push.w %r2
  load.w %r0, 0(%sp)  # span 47..48 "b"
  push.w %r0
  load.w %r0, 16(%sp)  # span 43..44 "a"
  pop.w %r1
  add.w %r0, %r1  # span 43..48 "a+b"
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_add_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_add_ret
fn_add_ret:
  ret
fn_main:
  load.w %r0, $10  # span 88..90 "10"
  push.w %r0
  pop.w %r1
  load.w %r0, $15  # span 92..94 "15"
  push.w %r0
  pop.w %r2
  call fn_add  # span 84..95 "add(10,15)"
  push.w %r0
  load.w %r0, 0(%sp)  # span 113..114 "x"
  push.w %r0
  pop.w %r1
  load.w %r0, $17  # span 116..118 "17"
  push.w %r0
  pop.w %r2
  call fn_add  # span 109..119 "add(x,17)"
  push.w %r0
  load.w %r0, 0(%sp)  # span 132..133 "y"
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_main_ret
  load.w %r7, $16
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
