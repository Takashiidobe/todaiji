main:
  load.w %r0, $1  # span 11..12 "1"
  push.w %r0
  load.w %r0, $2  # span 24..25 "2"
  push.w %r0
label_0:
  load.w %r0, $10  # span 52..54 "10"
  push.w %r0
  load.w %r0, 16(%sp)  # span 48..49 "a"
  pop.w %r1
  add.w %r0, %r1  # span 48..54 "a+10"
  pop.w %r15
  pop.w %r15
  jmp ret_exit
  jmp label_0
label_1:
  pop.w %r15
  pop.w %r15
ret_exit:
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
