  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_try_divide:
  push.w %r1
  push.w %r2
  load.w %r0, $0  # span 97..98 "0"
  push.w %r0
  load.w %r0, 8(%sp)  # span 92..93 "b"
  pop.w %r1
  cmpeq.w %r0, %r1  # span 92..98 "b==0"
  brz.w %r0, label_0
  mov %r1, %r12  # span 111..125 "Result::Err(...)"
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
  load.w %r0, $1  # span 123..124 "1"
  pop.w %r1
  store.w %r0, 8(%r1)
  mov.w %r0, %r1
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_try_divide_ret
label_0:
  mov %r1, %r12  # span 139..156 "Result::Ok(...)"
  mov.w %r2, %r12
  load.w %r0, $16
  add.w %r2, %r0
  movi %r0, $12
  mov.w %r1, %r2
  trap
  mov.w %r12, %r2
  # Store tag 0
  xor.w %r0, %r0
  store.w %r0, 0(%r1)
  # Store variant data
  push.w %r1
  load.w %r0, 8(%sp)  # span 154..155 "b"
  push.w %r0
  load.w %r0, 24(%sp)  # span 150..151 "a"
  pop.w %r1
  divmod.w %r0, %r1  # span 150..155 "a/b"
  pop.w %r1
  store.w %r0, 8(%r1)
  mov.w %r0, %r1
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_try_divide_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_try_divide_ret
fn_try_divide_ret:
  ret
main:
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
