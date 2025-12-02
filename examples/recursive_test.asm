  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  load.w %r0, $5  # span 46..47 "5"
  push.w %r0
  load.w %r0, 0(%sp)  # span 81..82 "x"
  push.w %r0
  pop.w %r1
  call fn_utils_double  # span 67..83 "utils::double(...)"
  push.w %r0
  load.w %r0, 8(%sp)  # span 117..118 "x"
  push.w %r0
  pop.w %r1
  call fn_utils_triple  # span 103..119 "utils::triple(...)"
  push.w %r0
  load.w %r0, 16(%sp)  # span 153..154 "x"
  push.w %r0
  pop.w %r1
  call fn_utils_square  # span 139..155 "utils::square(...)"
  push.w %r0
  load.w %r0, 0(%sp)  # span 188..195 "squared"
  push.w %r0
  load.w %r0, 16(%sp)  # span 178..185 "tripled"
  push.w %r0
  load.w %r0, 32(%sp)  # span 168..175 "doubled"
  pop.w %r1
  add.w %r0, %r1  # span 168..185 "doubled+tripled"
  pop.w %r1
  add.w %r0, %r1  # span 168..195 "doubled+tripled+squared"
  pop.w %r7
  pop.w %r7
  pop.w %r7
  pop.w %r7
  jmp fn_main_ret
  pop.w %r7
  pop.w %r7
  pop.w %r7
  pop.w %r7
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
  pop.w %r7
  pop.w %r7
  jmp fn_math_add_ret
  pop.w %r7
  pop.w %r7
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
  pop.w %r7
  pop.w %r7
  jmp fn_math_multiply_ret
  pop.w %r7
  pop.w %r7
  jmp fn_math_multiply_ret
fn_math_multiply_ret:
  ret
fn_utils_double:
  push.w %r1
  load.w %r0, 0(%sp)  # span 66..67 "x"
  push.w %r0
  pop.w %r1
  load.w %r0, 0(%sp)  # span 69..70 "x"
  push.w %r0
  pop.w %r2
  call fn_math_add  # span 56..71 "math::add(...)"
  pop.w %r7
  jmp fn_utils_double_ret
  pop.w %r7
  jmp fn_utils_double_ret
fn_utils_double_ret:
  ret
fn_utils_triple:
  push.w %r1
  load.w %r0, 0(%sp)  # span 135..136 "x"
  push.w %r0
  pop.w %r1
  load.w %r0, 0(%sp)  # span 138..139 "x"
  push.w %r0
  pop.w %r2
  call fn_math_add  # span 125..140 "math::add(...)"
  push.w %r0
  load.w %r0, 0(%sp)  # span 163..170 "doubled"
  push.w %r0
  pop.w %r1
  load.w %r0, 8(%sp)  # span 172..173 "x"
  push.w %r0
  pop.w %r2
  call fn_math_add  # span 153..174 "math::add(...)"
  pop.w %r7
  pop.w %r7
  jmp fn_utils_triple_ret
  pop.w %r7
  pop.w %r7
  jmp fn_utils_triple_ret
fn_utils_triple_ret:
  ret
fn_utils_square:
  push.w %r1
  load.w %r0, 0(%sp)  # span 236..237 "x"
  push.w %r0
  pop.w %r1
  load.w %r0, 0(%sp)  # span 239..240 "x"
  push.w %r0
  pop.w %r2
  call fn_math_multiply  # span 221..241 "math::multiply(...)"
  pop.w %r7
  jmp fn_utils_square_ret
  pop.w %r7
  jmp fn_utils_square_ret
fn_utils_square_ret:
  ret
main:
  call fn_main
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
