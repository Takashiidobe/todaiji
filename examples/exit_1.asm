addi.b %r0, $60 # syscall exit
addi.b %r1, $63  # exit code
trap           # perform syscall
