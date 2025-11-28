# Recursive Fibonacci up to n=10. Returns fib(10) in %r0.
# Calling convention: %r14 is PC, %r15 is SP.

# Layout:
# r0: return value
# r1: argument n
# r2: temp
# r3: temp

        mov.l %r1, $10     // n = 10
        call fib
        # result in r0
        jmp done

fib:
        # Prologue: save caller-saved we use (r2, r3)
        push.w %r2
        push.w %r3

        # if n <= 1: return n
        brz.l %r1, base_case
        # check if n == 1
        mov.l %r3, %r1
        subi.l %r3, $1
        brz.l %r3, base_case

        # fib(n-1)
        subi.l %r1, $1
        push.w %r1        # save n-1
        call fib
        mov.l %r2, %r0    # r2 = fib(n-1)
        pop.w %r1         # restore n-1

        # fib(n-2)
        subi.l %r1, $1
        push.w %r1
        call fib
        mov.l %r3, %r0    # r3 = fib(n-2)
        pop.w %r1

        mov.l %r0, %r2    # r0 = fib(n-1)
        add.l %r0, %r3    # r0 += fib(n-2)
        jmp end_fib

base_case:
        mov.l %r0, %r1

end_fib:
        # Epilogue
        pop.w %r3
        pop.w %r2
        ret

done:
        nop
