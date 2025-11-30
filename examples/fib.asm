# Iterative Fibonacci for n=10. Result returned in %r0.

# Registers:
# r0: fib(n-1) / result
# r1: loop counter n
# r2: fib(n)
# r3: temp (next)

        movi   %r1, $10    # n = 10
        xor.w  %r0, %r0    # i = 0
        movi   %r2, $1     # fib(1)

        brz.w  %r1, done   # if n == 0, r0 already holds fib(0)

loop:
        mov.w  %r3, %r0    # temp = fib(n-1)
        add.w  %r3, %r2    # temp = fib(n-1) + fib(n)
        mov.w  %r0, %r2    # fib(n-1) = fib(n)
        mov.w  %r2, %r3    # fib(n) = temp
        subi   %r1, $1
        brnz.w %r1, loop

done:
				call print_u64_stack
        ret
.include "./print.asm"
