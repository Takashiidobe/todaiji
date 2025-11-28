# Simple test program for binary encoding
# Sets r0 = 42, r1 = 100, then adds them
mov.l %r0, $42
mov.l %r1, $100
add.l %r0, %r1
# Result: r0 should be 142
