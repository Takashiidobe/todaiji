  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  load.w %r0, $5  # span 57..58 "5"
  push.w %r0
  pop.w %r1
  load.w %r0, $3  # span 60..61 "3"
  push.w %r0
  pop.w %r2
  call fn_math_add  # span 47..62 "math::add(...)"
  push.w %r0
  load.w %r0, $4  # span 97..98 "4"
  push.w %r0
  pop.w %r1
  load.w %r0, $7  # span 100..101 "7"
  push.w %r0
  pop.w %r2
  call fn_math_multiply  # span 82..102 "math::multiply(...)"
  push.w %r0
  load.w %r0, 8(%sp)  # span 115..118 "sum"
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_main_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_main_ret
fn_main_ret:
  ret
fn_math_add:
  push.w %r1
  push.w %r2
  load.w %r0, 0(%sp)  # span 51..52 "b"
  push.w %r0
  load.w %r0, 16(%sp)  # span 47..48 "a"
  pop.w %r1
  add.w %r0, %r1  # span 47..52 "a+b"
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_math_add_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_math_add_ret
fn_math_add_ret:
  ret
fn_math_multiply:
  push.w %r1
  push.w %r2
  load.w %r0, 0(%sp)  # span 113..114 "b"
  push.w %r0
  load.w %r0, 16(%sp)  # span 109..110 "a"
  pop.w %r1
  mul.w %r0, %r1  # span 109..114 "a*b"
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_math_multiply_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_math_multiply_ret
fn_math_multiply_ret:
  ret
main:
  call fn_main
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
