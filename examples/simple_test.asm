# Simple test program for binary encoding
# Sets r0 = 42, r1 = 100, then adds them
load.l %r0, $42
load.l %r1, $100
add.l %r0, %r1
call print_u64_stack
.include "./print.asm"
