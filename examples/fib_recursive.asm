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
# utoa10: convert unsigned 64-bit integer in %r0 to decimal ASCII
# Args:
#   %r0 = value
#   %r1 = buffer pointer (at least 21 bytes)
# Returns:
#   %r0 = pointer to first digit
#   %r1 = length in bytes (no terminator)
# Clobbers:
#   %r2, %r3, %r4, %r5
utoa10:
        # Check for value == 0
        mov.w   %r2, %r0
        brz.w   %r2, utoa10_zero

        # r3 = buf_end = buf + 20 (we'll fill digits backwards)
        mov.w   %r3, %r1
        addi    %r3, $20

utoa10_loop:
        # We have n in %r0
        # We want q = n / 10 (in %r2) and rem = n - q*10 (in %r0)

        mov.w   %r2, %r0        # r2 = n
        movi   %r4, $10
        divu.w  %r2, %r4        # r2 = q = n / 10 (unsigned)

        mov.w   %r4, %r2        # r4 = q
        muli.w  %r4, $10        # r4 = q * 10
        sub.w   %r0, %r4        # r0 = rem = n - q*10 (0..9)

        # Convert remainder to ASCII digit: '0' + rem
        movi    %r5, $48        # '0'
        add.w   %r5, %r0        # r5 = '0' + rem

        # Store digit at --r3
        subi.l  %r3, $1
        store.b   %r5, (%r3)

        # Next iteration: n = q
        mov.w   %r0, %r2
        brnz.w  %r0, utoa10_loop

        # Done: r3 points to first digit, buf_end = buf + 20
        # Return:
        #   r0 = start pointer
        #   r1 = length = (buf+20) - start
        mov.w   %r0, %r3        # start pointer

        mov.w   %r2, %r1
        addi    %r2, $20        # r2 = buf_end
        mov.w   %r1, %r2        # r1 = buf_end
        sub.w   %r1, %r0        # len = buf_end - start

        ret

utoa10_zero:
        # value == 0 -> "0"
        movi   %r2, $48        # '0'
        store.b   %r2, (%r1)
        # optional NUL terminator:
        movi   %r2, $0
        store.b   %r2, 1(%r1)

        mov.w   %r0, %r1        # start = buf
        movi   %r1, $1         # len = 1
        ret
        # Enough space for 20 digits + optional '-' + '\n' + '\0'
buf:
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
# print_u64: print unsigned 64-bit in %r0 followed by '\n'
# Args:
#   %r0 = value
# Clobbers:
#   %r0-%r3, %r4
print_u64:
        # Save caller-saved regs you care about (example uses r4)
        push.w  %r4

        # Point %r1 at buffer and convert
        load.l   %r1, buf
        call    utoa10         # r0 = start, r1 = len

        # Append '\n' at end: buf[start + len] = '\n'
        mov.w   %r2, %r0       # r2 = start
        add.w   %r2, %r1       # r2 = start + len
        movi    %r4, $10       # '\n'
        store.b   %r4, (%r2)

        # len = len + 1 (include newline)
        addi    %r1, $1

        # Prepare write syscall: write(1, start, len')
        mov.w   %r2, %r0       # buf ptr
        mov.w   %r3, %r1       # len

        movi   %r0, $1        # syscall write
        movi   %r1, $1        # fd = stdout
        trap

        pop.w   %r4
        ret
