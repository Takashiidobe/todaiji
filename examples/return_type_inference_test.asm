  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_multiply:
  push.w %r1
  push.w %r2
  load.w %r0, 0(%sp)  # span 35..36 "b"
  push.w %r0
  load.w %r0, 16(%sp)  # span 31..32 "a"
  pop.w %r1
  mul.w %r0, %r1  # span 31..36 "a*b"
  pop.w %r7
  pop.w %r7
  jmp fn_multiply_ret
  pop.w %r7
  pop.w %r7
  jmp fn_multiply_ret
fn_multiply_ret:
  ret
fn_main:
  load.w %r0, $5  # span 79..80 "5"
  push.w %r0
  pop.w %r1
  load.w %r0, $7  # span 82..83 "7"
  push.w %r0
  pop.w %r2
  call fn_multiply  # span 70..84 "multiply(5,7)"
  push.w %r0
  load.w %r0, 0(%sp)  # span 97..103 "result"
  pop.w %r7
  jmp fn_main_ret
  pop.w %r7
  jmp fn_main_ret
fn_main_ret:
  ret
main:
  call fn_main
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
