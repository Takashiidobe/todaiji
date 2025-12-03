  movi %r0, $12
  xor.w %r1, %r1
  trap
  mov %r12, %r0
  jmp main
fn_main:
  load.w %r0, $10  # span 50..52 "10"
  push.w %r0
  load.w %r0, 0(%sp)  # span 88..96 "box_size"
  push.w %r0
  pop.w %r1
  call fn_ui_render_box  # span 73..97 "ui::render_box(...)"
  push.w %r0
  load.w %r0, $20  # span 137..139 "20"
  push.w %r0
  pop.w %r1
  load.w %r0, $30  # span 141..143 "30"
  push.w %r0
  pop.w %r2
  call fn_ui_calculate_layout  # span 116..144 "ui::calculate_layout(...)"
  push.w %r0
  load.w %r0, 0(%sp)  # span 168..174 "layout"
  push.w %r0
  load.w %r0, 16(%sp)  # span 157..165 "rendered"
  pop.w %r1
  add.w %r0, %r1  # span 157..174 "rendered+layout"
  load.w %r7, $24
  add.w %sp, %r7
  jmp fn_main_ret
  load.w %r7, $24
  add.w %sp, %r7
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
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_math_add_ret
  load.w %r7, $16
  add.w %sp, %r7
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
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_math_multiply_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_math_multiply_ret
fn_math_multiply_ret:
  ret
fn_ui_render_box:
  push.w %r1
  load.w %r0, 0(%sp)  # span 109..113 "size"
  push.w %r0
  pop.w %r1
  load.w %r0, 0(%sp)  # span 115..119 "size"
  push.w %r0
  pop.w %r2
  call fn_graphics_draw_rectangle  # span 84..120 "graphics::draw_rectangle(...)"
  push.w %r0
  load.w %r0, 8(%sp)  # span 154..158 "size"
  push.w %r0
  pop.w %r1
  load.w %r0, $4  # span 160..161 "4"
  push.w %r0
  pop.w %r2
  call fn_math_multiply  # span 139..162 "math::multiply(...)"
  push.w %r0
  load.w %r0, 8(%sp)  # span 185..189 "area"
  push.w %r0
  pop.w %r1
  load.w %r0, 0(%sp)  # span 191..197 "border"
  push.w %r0
  pop.w %r2
  call fn_math_add  # span 175..198 "math::add(...)"
  load.w %r7, $24
  add.w %sp, %r7
  jmp fn_ui_render_box_ret
  load.w %r7, $16
  add.w %sp, %r7
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_ui_render_box_ret
fn_ui_render_box_ret:
  ret
fn_ui_calculate_layout:
  push.w %r1
  push.w %r2
  load.w %r0, 8(%sp)  # span 282..287 "width"
  push.w %r0
  pop.w %r1
  load.w %r0, 0(%sp)  # span 289..295 "height"
  push.w %r0
  pop.w %r2
  call fn_math_add  # span 272..296 "math::add(...)"
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_ui_calculate_layout_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_ui_calculate_layout_ret
fn_ui_calculate_layout_ret:
  ret
fn_graphics_draw_rectangle:
  push.w %r1
  push.w %r2
  load.w %r0, 8(%sp)  # span 96..101 "width"
  push.w %r0
  pop.w %r1
  load.w %r0, 0(%sp)  # span 103..109 "height"
  push.w %r0
  pop.w %r2
  call fn_math_multiply  # span 81..110 "math::multiply(...)"
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_graphics_draw_rectangle_ret
  load.w %r7, $16
  add.w %sp, %r7
  jmp fn_graphics_draw_rectangle_ret
fn_graphics_draw_rectangle_ret:
  ret
fn_graphics_draw_line:
  push.w %r1
  load.w %r0, 0(%sp)  # span 175..181 "length"
  push.w %r0
  pop.w %r1
  load.w %r0, $0  # span 183..184 "0"
  push.w %r0
  pop.w %r2
  call fn_math_add  # span 165..185 "math::add(...)"
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_graphics_draw_line_ret
  load.w %r7, $8
  add.w %sp, %r7
  jmp fn_graphics_draw_line_ret
fn_graphics_draw_line_ret:
  ret
main:
  call fn_main
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
