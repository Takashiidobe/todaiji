main:
	movi %r0, $-5
	muli %r0, $5
	call print_i64_stack
.include "./print.asm"
