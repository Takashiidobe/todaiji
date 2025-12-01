main:
  load.w %r0, $1  # span 11..12 "1"
  push.w %r0
  load.w %r0, $2  # span 24..25 "2"
  push.w %r0
  load.w %r0, $3  # span 36..37 "3"
  push.w %r0
  load.w %r0, $4  # span 48..49 "4"
  push.w %r0
  load.w %r0, $5  # span 60..61 "5"
  push.w %r0
  load.w %r0, $6  # span 72..73 "6"
  push.w %r0
  load.w %r0, $7  # span 84..85 "7"
  push.w %r0
  load.w %r0, $8  # span 96..97 "8"
  push.w %r0
  load.w %r0, $9  # span 108..109 "9"
  push.w %r0
  load.w %r0, $10  # span 120..122 "10"
  push.w %r0
  load.w %r0, $11  # span 133..135 "11"
  push.w %r0
  load.w %r0, $12  # span 146..148 "12"
  push.w %r0
  load.w %r0, $13  # span 159..161 "13"
  push.w %r0
  load.w %r0, $14  # span 172..174 "14"
  push.w %r0
  load.w %r0, $15  # span 185..187 "15"
  push.w %r0
  load.w %r0, $16  # span 198..200 "16"
  push.w %r0
  load.w %r0, $33  # span 211..213 "33"
  push.w %r0
  load.w %r0, 120(%sp)  # span 225..226 "b"
  push.w %r0
  load.w %r0, 136(%sp)  # span 220..221 "a"
  pop.w %r1
  cmpeq.w %r0, %r1  # span 220..226 "a==b"
  brz.w %r0, label_0
  load.w %r0, $2  # span 239..240 "2"
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  pop.w %r15
  jmp label_1
label_0:
  load.w %r0, $3  # span 261..262 "3"
label_1:
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
