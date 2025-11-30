main:
	movi %r1, $10
	movi %r2, $20
	brlt.b %r1, %r2, less_than
	jmps greater_than
	ret

less_than:
	movi %r0, $1
	movi %r1, $1
	load.l %r2, lt_str
	movi %r3, $9
	trap
	ret

greater_than:
	movi %r0, $1
	movi %r1, $1
	load.l %r2, gte_str
	movi %r3, $10
	trap
	ret

gte_str:
	.asciz "10 >= 20\n"
lt_str:
	.asciz "10 < 20\n"
