movi %r0, $60  # syscall exit
movi %r1, $63  # exit code
trap           # perform syscall
