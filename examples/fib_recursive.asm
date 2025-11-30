# Recursive Fibonacci up to n=10. Returns fib(10) in %r0.
# Uses: %r1 (argument n), %r2/%r3 temps. Stack grows downward.

        movi    %r1, $10     # n = 10
        call    fib
        # result in %r0
				call print_u64
				ret

fib:
        # Prologue: save caller-saved we use (r2, r3)
        push.w  %r2
        push.w  %r3

        # if n <= 1: return n
        brz.w   %r1, base_case
        mov.w   %r3, %r1
        subi.w  %r3, $1
        brz.w   %r3, base_case

        # fib(n-1)
        subi.w  %r1, $1
        push.w  %r1        # save n-1
        call    fib
        mov.w   %r2, %r0   # r2 = fib(n-1)
        pop.w   %r1        # restore n-1

        # fib(n-2)
        subi.w  %r1, $1
        push.w  %r1
        call    fib
        mov.w   %r3, %r0   # r3 = fib(n-2)
        pop.w   %r1

        mov.w   %r0, %r2   # r0 = fib(n-1)
        add.w   %r0, %r3   # r0 += fib(n-2)
        jmp     end_fib

base_case:
        mov.w   %r0, %r1

end_fib:
        # Epilogue
        pop.w   %r3
        pop.w   %r2
        ret
.include "./print_unsigned.asm"
