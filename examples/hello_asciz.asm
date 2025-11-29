# Print "hello world" via trap syscall write
# r0 = syscall number (1)
# r1 = fd (1 = stdout)
# r2 = buffer pointer (absolute byte offset)
# r3 = length

        mov.l %r0, $1       # syscall write
        mov.l %r1, $1       # stdout
        mov.l %r2, msg      # r2 = &msg
        mov.l %r3, $12      # length
        trap                # perform syscall
        ret

msg:
        .asciz "hello world\n"
