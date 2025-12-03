  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  load.w %r0, $3  # span 63..64 "3"
  push.w %r0
  load.w %r0, $4  # span 81..82 "4"
  push.w %r0
  load.w %r0, $5  # span 99..100 "5"
  push.w %r0
  load.w %r0, $12  # span 117..119 "12"
  push.w %r0
  load.w %r0, 24(%sp)  # span 148..152 "v1_x"
  push.w %r0
  pop.w %r1
  load.w %r0, 16(%sp)  # span 154..158 "v1_y"
  push.w %r0
  pop.w %r2
  load.w %r0, 8(%sp)  # span 160..164 "v2_x"
  push.w %r0
  pop.w %r3
  load.w %r0, 0(%sp)  # span 166..170 "v2_y"
  push.w %r0
  pop.w %r4
  call fn_vec2_add  # span 138..171 "vec2::add(...)"
  push.w %r0
  load.w %r0, 32(%sp)  # span 205..209 "v1_x"
  push.w %r0
  pop.w %r1
  load.w %r0, 24(%sp)  # span 211..215 "v1_y"
  push.w %r0
  pop.w %r2
  load.w %r0, 16(%sp)  # span 217..221 "v2_x"
  push.w %r0
  pop.w %r3
  load.w %r0, 8(%sp)  # span 223..227 "v2_y"
  push.w %r0
  pop.w %r4
  call fn_vec2_dot  # span 195..228 "vec2::dot(...)"
  push.w %r0
  load.w %r0, 40(%sp)  # span 271..275 "v1_x"
  push.w %r0
  pop.w %r1
  load.w %r0, 32(%sp)  # span 277..281 "v1_y"
  push.w %r0
  pop.w %r2
  call fn_vec2_magnitude_squared  # span 247..282 "vec2::magnitude_squared(...)"
  push.w %r0
  load.w %r0, $10  # span 306..308 "10"
  push.w %r0
  load.w %r0, $5  # span 332..333 "5"
  push.w %r0
  load.w %r0, 8(%sp)  # span 373..383 "rect_width"
  push.w %r0
  pop.w %r1
  load.w %r0, 0(%sp)  # span 385..396 "rect_height"
  push.w %r0
  pop.w %r2
  call fn_shapes_rectangle_area  # span 350..397 "shapes::rectangle_area(...)"
  push.w %r0
  load.w %r0, 16(%sp)  # span 447..457 "rect_width"
  push.w %r0
  pop.w %r1
  load.w %r0, 8(%sp)  # span 459..470 "rect_height"
  push.w %r0
  pop.w %r2
  call fn_shapes_rectangle_perimeter  # span 419..471 "shapes::rectangle_perimeter(...)"
  push.w %r0
  load.w %r0, $7  # span 491..492 "7"
  push.w %r0
  load.w %r0, 0(%sp)  # span 543..549 "radius"
  push.w %r0
  pop.w %r1
  call fn_shapes_circle_area_approx  # span 516..550 "shapes::circle_area_approx(...)"
  push.w %r0
  mov %r1, %r12  # span 571..634 "shapes::Rectangle { ... }"
  mov.w %r2, %r12
  load.w %r0, $16
  add.w %r2, %r0
  movi %r0, $12
  mov.w %r1, %r2
  trap
  mov.w %r12, %r2
  push.w %r1
  load.w %r0, $20  # span 606..608 "20"
  store.w %r0, 0(%r1)
  pop.w %r1
  push.w %r1
  load.w %r0, $15  # span 626..628 "15"
  store.w %r0, 8(%r1)
  pop.w %r1
  mov.w %r0, %r1
  push.w %r0
  mov %r1, %r12  # span 657..698 "shapes::Circle { ... }"
  mov.w %r2, %r12
  load.w %r0, $8
  add.w %r2, %r0
  movi %r0, $12
  mov.w %r1, %r2
  trap
  mov.w %r12, %r2
  push.w %r1
  load.w %r0, $10  # span 690..692 "10"
  store.w %r0, 0(%r1)
  pop.w %r1
  mov.w %r0, %r1
  push.w %r0
  mov %r1, %r12  # span 718..767 "vec2::Vec2 { ... }"
  mov.w %r2, %r12
  load.w %r0, $16
  add.w %r2, %r0
  movi %r0, $12
  mov.w %r1, %r2
  trap
  mov.w %r12, %r2
  push.w %r1
  load.w %r0, $100  # span 742..745 "100"
  store.w %r0, 0(%r1)
  pop.w %r1
  push.w %r1
  load.w %r0, $200  # span 758..761 "200"
  store.w %r0, 8(%r1)
  pop.w %r1
  mov.w %r0, %r1
  push.w %r0
  load.w %r0, 24(%sp)  # span 831..842 "circle_area"
  push.w %r0
  load.w %r0, 48(%sp)  # span 819..828 "perimeter"
  push.w %r0
  load.w %r0, 64(%sp)  # span 812..816 "area"
  push.w %r0
  load.w %r0, 96(%sp)  # span 803..809 "mag_sq"
  push.w %r0
  load.w %r0, 112(%sp)  # span 789..800 "dot_product"
  push.w %r0
  load.w %r0, 128(%sp)  # span 781..786 "sum_x"
  pop.w %r1
  add.w %r0, %r1  # span 781..800 "sum_x+dot_product"
  pop.w %r1
  add.w %r0, %r1  # span 781..809 "sum_x+dot_product+mag_sq"
  pop.w %r1
  add.w %r0, %r1  # span 781..816 "sum_x+dot_product+mag_sq+area"
  pop.w %r1
  add.w %r0, %r1  # span 781..828 "sum_x+dot_product+mag_sq+area+perimeter"
  pop.w %r1
  add.w %r0, %r1  # span 781..842 "sum_x+dot_product+mag_sq+area+perimeter+circle_area"
  load.w %r7, $128
  add.w %sp, %r7
  jmp fn_main_ret
  load.w %r7, $128
  add.w %sp, %r7
  jmp fn_main_ret
fn_main_ret:
  ret
fn_vec2_new:
  push.w %r1
  push.w %r2
  load.w %r0, $0  # span 92..93 "0"
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_vec2_new_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_vec2_new_ret
fn_vec2_new_ret:
  ret
fn_vec2_add:
  push.w %r1
  push.w %r2
  push.w %r3
  push.w %r4
  load.w %r0, 8(%sp)  # span 170..172 "x2"
  push.w %r0
  load.w %r0, 32(%sp)  # span 165..167 "x1"
  pop.w %r1
  add.w %r0, %r1  # span 165..172 "x1+x2"
  load.w %r7, $32
  add.w %sp, %r7
  jmp fn_vec2_add_ret
  load.w %r7, $32
  add.w %sp, %r7
  jmp fn_vec2_add_ret
fn_vec2_add_ret:
  ret
fn_vec2_dot:
  push.w %r1
  push.w %r2
  push.w %r3
  push.w %r4
  load.w %r0, 0(%sp)  # span 259..261 "y2"
  push.w %r0
  load.w %r0, 24(%sp)  # span 254..256 "y1"
  pop.w %r1
  mul.w %r0, %r1  # span 254..261 "y1*y2"
  push.w %r0
  load.w %r0, 16(%sp)  # span 249..251 "x2"
  push.w %r0
  load.w %r0, 40(%sp)  # span 244..246 "x1"
  pop.w %r1
  mul.w %r0, %r1  # span 244..251 "x1*x2"
  pop.w %r1
  add.w %r0, %r1  # span 244..261 "x1*x2+y1*y2"
  load.w %r7, $32
  add.w %sp, %r7
  jmp fn_vec2_dot_ret
  load.w %r7, $32
  add.w %sp, %r7
  jmp fn_vec2_dot_ret
fn_vec2_dot_ret:
  ret
fn_vec2_magnitude_squared:
  push.w %r1
  push.w %r2
  load.w %r0, 0(%sp)  # span 339..340 "y"
  push.w %r0
  load.w %r0, 8(%sp)  # span 335..336 "y"
  pop.w %r1
  mul.w %r0, %r1  # span 335..340 "y*y"
  push.w %r0
  load.w %r0, 16(%sp)  # span 331..332 "x"
  push.w %r0
  load.w %r0, 24(%sp)  # span 327..328 "x"
  pop.w %r1
  mul.w %r0, %r1  # span 327..332 "x*x"
  pop.w %r1
  add.w %r0, %r1  # span 327..340 "x*x+y*y"
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_vec2_magnitude_squared_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_vec2_magnitude_squared_ret
fn_vec2_magnitude_squared_ret:
  ret
fn_shapes_rectangle_area:
  push.w %r1
  push.w %r2
  load.w %r0, 0(%sp)  # span 174..180 "height"
  push.w %r0
  load.w %r0, 16(%sp)  # span 166..171 "width"
  pop.w %r1
  mul.w %r0, %r1  # span 166..180 "width*height"
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_shapes_rectangle_area_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_shapes_rectangle_area_ret
fn_shapes_rectangle_area_ret:
  ret
fn_shapes_rectangle_perimeter:
  push.w %r1
  push.w %r2
  load.w %r0, 0(%sp)  # span 282..288 "height"
  push.w %r0
  load.w %r0, 8(%sp)  # span 273..279 "height"
  push.w %r0
  load.w %r0, 24(%sp)  # span 265..270 "width"
  push.w %r0
  load.w %r0, 32(%sp)  # span 257..262 "width"
  pop.w %r1
  add.w %r0, %r1  # span 257..270 "width+width"
  pop.w %r1
  add.w %r0, %r1  # span 257..279 "width+width+height"
  pop.w %r1
  add.w %r0, %r1  # span 257..288 "width+width+height+height"
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_shapes_rectangle_perimeter_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_shapes_rectangle_perimeter_ret
fn_shapes_rectangle_perimeter_ret:
  ret
fn_shapes_circle_area_approx:
  push.w %r1
  load.w %r0, 0(%sp)  # span 365..371 "radius"
  push.w %r0
  load.w %r0, 8(%sp)  # span 356..362 "radius"
  push.w %r0
  load.w %r0, $3  # span 352..353 "3"
  pop.w %r1
  mul.w %r0, %r1  # span 352..362 "3*radius"
  pop.w %r1
  mul.w %r0, %r1  # span 352..371 "3*radius*radius"
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_shapes_circle_area_approx_ret
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_shapes_circle_area_approx_ret
fn_shapes_circle_area_approx_ret:
  ret
main:
  call fn_main
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
