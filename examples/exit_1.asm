mov.b %r0, $60 # syscall exit
mov.b %r1, $1  # exit code
trap           # perform syscall
