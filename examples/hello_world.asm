# Print "hello world" via trap syscall write
# r0 = syscall number (1)
# r1 = fd (1 = stdout)
# r2 = buffer pointer (absolute byte offset)
# r3 = length

        movi.l %r0, $1       # syscall write
        movi.l %r1, $1       # stdout
        mov.l %r2, msg       # r2 = &msg
        movi.l %r3, $12      # length
        trap                 # perform syscall
        mov.l %r2, msg_asciz # r2 = &msg
        trap                 # perform syscall
        mov.l %r2, msg_ascii # r2 = &msg
        trap                 # perform syscall
        ret

msg:
        .byte 104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 10
msg_ascii:
        .ascii "hello ascii\n"
msg_asciz:
        .ascii "hello asciz\n"
