mov.l %r0, $60 # syscall exit
mov.l %r1, $1  # exit code
trap           # perform syscall
