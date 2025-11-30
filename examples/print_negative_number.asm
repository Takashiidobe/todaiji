main:
	movi %r0, $5 # %r0 = 5
	neg  %r0 # %r0 = -5
	muli %r0, $5 # %r0 = -25
	call print_i64_stack
.include "./print.asm"
