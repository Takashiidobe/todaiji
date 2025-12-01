main:
  load.w %r0, $1  # span 11..12 "1"
  push.w %r0
  load.w %r0, $2  # span 24..25 "2"
  push.w %r0
  load.w %r0, 0(%sp)  # span 37..38 "b"
  push.w %r0
  load.w %r0, 16(%sp)  # span 32..33 "a"
  pop.w %r1
  cmpeq.w %r0, %r1  # span 32..38 "a==b"
  brz.w %r0, label_0
  load.w %r0, $2  # span 51..52 "2"
  pop.w %r15
  pop.w %r15
  jmp ret_exit
  jmp label_1
label_0:
  load.w %r0, 8(%sp)  # span 71..72 "a"
  push.w %r0
  load.w %r0, 16(%sp)  # span 66..67 "a"
  pop.w %r1
  cmpeq.w %r0, %r1  # span 66..72 "a==a"
  brz.w %r0, label_2
  load.w %r0, $0  # span 85..86 "0"
  pop.w %r15
  pop.w %r15
  jmp ret_exit
  jmp label_3
label_2:
  load.w %r0, $3  # span 106..107 "3"
  pop.w %r15
  pop.w %r15
  jmp ret_exit
label_3:
label_1:
  pop.w %r15
  pop.w %r15
ret_exit:
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
