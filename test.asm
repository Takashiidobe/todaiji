main:
  load.w %r0, $1  # span 11..12 "1"
  push.w %r0
  load.w %r0, $2  # span 24..25 "2"
  push.w %r0
  load.w %r0, $0  # span 41..42 "0"
  push.w %r0
label_0:
  load.w %r0, $3  # span 48..49 "3"
  push.w %r0
  load.w %r0, 8(%sp)  # span 44..45 "c"
  pop.w %r1
  cmplt.w %r0, %r1  # span 44..49 "c<3"
  brz.w %r0, label_1
  load.w %r0, $3  # span 74..75 "3"
  push.w %r0
  load.w %r0, 24(%sp)  # span 70..71 "a"
  pop.w %r1
  mul.w %r0, %r1  # span 70..75 "a*3"
  store.w %r0, 16(%sp)  # span 66..75 "a=a*3"
  load.w %r0, $1  # span 59..60 "1"
  push.w %r0
  load.w %r0, 8(%sp)  # span 55..56 "c"
  pop.w %r1
  add.w %r0, %r1  # span 55..60 "c+1"
  store.w %r0, 0(%sp)  # span 51..60 "c=c+1"
  jmp label_0
label_1:
  pop.w %r15
  load.w %r0, 8(%sp)  # span 88..89 "a"
  pop.w %r15
  pop.w %r15
  jmp ret_exit
  pop.w %r15
  pop.w %r15
ret_exit:
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
