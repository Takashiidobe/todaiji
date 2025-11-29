movi.b %r0, $60  # syscall exit
movi.b %r1, $63  # exit code
trap             # perform syscall
