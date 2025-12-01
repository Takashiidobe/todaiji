main:
  load.w %r0, $1  # span 8..9 "1"
  push.w %r0
  load.w %r0, $2  # span 20..21 "2"
  push.w %r0
  load.w %r0, $3  # span 31..32 "3"
  push.w %r0
  load.w %r0, $4  # span 42..43 "4"
  push.w %r0
  load.w %r0, $5  # span 53..54 "5"
  push.w %r0
  load.w %r0, $6  # span 64..65 "6"
  push.w %r0
  load.w %r0, $7  # span 75..76 "7"
  push.w %r0
  load.w %r0, $8  # span 86..87 "8"
  push.w %r0
  load.w %r0, $9  # span 97..98 "9"
  push.w %r0
  load.w %r0, $10  # span 108..110 "10"
  push.w %r0
  load.w %r0, $11  # span 120..122 "11"
  push.w %r0
  load.w %r0, $12  # span 132..134 "12"
  push.w %r0
  load.w %r0, $13  # span 144..146 "13"
  push.w %r0
  load.w %r0, $14  # span 156..158 "14"
  push.w %r0
  load.w %r0, $15  # span 168..170 "15"
  push.w %r0
  load.w %r0, $16  # span 180..182 "16"
  push.w %r0
  load.w %r0, $33  # span 192..194 "33"
  push.w %r0
  load.w %r0, 120(%sp)  # span 207..208 "b"
  push.w %r0
  load.w %r0, 136(%sp)  # span 203..204 "a"
  pop.w %r1
  mul.w %r0, %r1  # span 203..208 "a*b"
  jmp ret_exit
  load.w %r0, 0(%sp)  # span 241..242 "q"
  push.w %r0
  load.w %r0, 96(%sp)  # span 237..238 "f"
  push.w %r0
  load.w %r0, 112(%sp)  # span 233..234 "e"
  push.w %r0
  load.w %r0, 128(%sp)  # span 229..230 "d"
  push.w %r0
  load.w %r0, 144(%sp)  # span 225..226 "c"
  push.w %r0
  load.w %r0, 160(%sp)  # span 221..222 "b"
  push.w %r0
  load.w %r0, 176(%sp)  # span 217..218 "a"
  pop.w %r1
  mul.w %r0, %r1  # span 217..222 "a*b"
  pop.w %r1
  mul.w %r0, %r1  # span 217..226 "a*b*c"
  pop.w %r1
  mul.w %r0, %r1  # span 217..230 "a*b*c*d"
  pop.w %r1
  mul.w %r0, %r1  # span 217..234 "a*b*c*d*e"
  pop.w %r1
  mul.w %r0, %r1  # span 217..238 "a*b*c*d*e*f"
  pop.w %r1
  mul.w %r0, %r1  # span 217..242 "a*b*c*d*e*f*q"
  jmp ret_exit
ret_exit:
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
