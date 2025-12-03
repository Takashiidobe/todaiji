  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  load.w %r0, $10  # span 78..80 "10"
  push.w %r0
  pop.w %r1
  call fn_left_left_compute  # span 59..81 "left::left_compute(...)"
  push.w %r0
  load.w %r0, $5  # span 116..117 "5"
  push.w %r0
  pop.w %r1
  call fn_right_right_compute  # span 95..118 "right::right_compute(...)"
  push.w %r0
  load.w %r0, 0(%sp)  # span 135..136 "b"
  push.w %r0
  load.w %r0, 16(%sp)  # span 131..132 "a"
  pop.w %r1
  add.w %r0, %r1  # span 131..136 "a+b"
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_main_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_main_ret
fn_main_ret:
  ret
fn_left_left_compute:
  push.w %r1
  load.w %r0, 0(%sp)  # span 75..76 "x"
  push.w %r0
  pop.w %r1
  call fn_base_double  # span 62..77 "base::double(...)"
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_left_left_compute_ret
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_left_left_compute_ret
fn_left_left_compute_ret:
  ret
fn_base_identity:
  push.w %r1
  load.w %r0, 0(%sp)  # span 44..45 "x"
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_base_identity_ret
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_base_identity_ret
fn_base_identity_ret:
  ret
fn_base_double:
  push.w %r1
  load.w %r0, 0(%sp)  # span 96..97 "x"
  push.w %r0
  load.w %r0, 8(%sp)  # span 92..93 "x"
  pop.w %r1
  add.w %r0, %r1  # span 92..97 "x+x"
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_base_double_ret
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_base_double_ret
fn_base_double_ret:
  ret
fn_right_right_compute:
  push.w %r1
  load.w %r0, 0(%sp)  # span 78..79 "x"
  push.w %r0
  pop.w %r1
  call fn_base_identity  # span 63..80 "base::identity(...)"
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_right_right_compute_ret
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_right_right_compute_ret
fn_right_right_compute_ret:
  ret
main:
  call fn_main
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
