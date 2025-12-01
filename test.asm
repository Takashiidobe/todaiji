main:
  load.w %r0, $1  # span 6..7 "1"
  push.w %r0
  load.w %r0, $200  # span 0..3 "200"
  pop.w %r1
  cmplt.w %r1, %r0  # span 0..7 "200>1"
  push.w %r1
  pop.w %r0
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
