  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  load.w %r0, $5  # span 68..69 "5"
  push.w %r0
  pop.w %r1
  call fn_level1_compute  # span 52..70 "level1::compute(...)"
  push.w %r0
  load.w %r0, 0(%sp)  # span 83..89 "result"
  pop.w %r7
  jmp fn_main_ret
  pop.w %r7
  jmp fn_main_ret
fn_main_ret:
  ret
fn_level1_compute:
  push.w %r1
  load.w %r0, 0(%sp)  # span 81..82 "x"
  push.w %r0
  pop.w %r1
  call fn_level2_double_triple  # span 59..83 "level2::double_triple(...)"
  pop.w %r7
  jmp fn_level1_compute_ret
  pop.w %r7
  jmp fn_level1_compute_ret
fn_level1_compute_ret:
  ret
fn_level3_triple:
  push.w %r1
  load.w %r0, 0(%sp)  # span 50..51 "x"
  push.w %r0
  load.w %r0, 8(%sp)  # span 46..47 "x"
  push.w %r0
  load.w %r0, 16(%sp)  # span 42..43 "x"
  pop.w %r1
  add.w %r0, %r1  # span 42..47 "x+x"
  pop.w %r1
  add.w %r0, %r1  # span 42..51 "x+x+x"
  pop.w %r7
  jmp fn_level3_triple_ret
  pop.w %r7
  jmp fn_level3_triple_ret
fn_level3_triple_ret:
  ret
fn_level2_double_triple:
  push.w %r1
  load.w %r0, 0(%sp)  # span 87..88 "x"
  push.w %r0
  pop.w %r1
  call fn_level3_triple  # span 72..89 "level3::triple(...)"
  push.w %r0
  load.w %r0, 0(%sp)  # span 112..119 "tripled"
  push.w %r0
  load.w %r0, 8(%sp)  # span 102..109 "tripled"
  pop.w %r1
  add.w %r0, %r1  # span 102..119 "tripled+tripled"
  pop.w %r7
  pop.w %r7
  jmp fn_level2_double_triple_ret
  pop.w %r7
  pop.w %r7
  jmp fn_level2_double_triple_ret
fn_level2_double_triple_ret:
  ret
main:
  call fn_main
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
