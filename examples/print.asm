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
        subi      %r3, $1
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
// # Enough space for 20 digits + optional '-' + '\n' + '\0'
// buf:
//         .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
// # print_u64: print unsigned 64-bit in %r0 followed by '\n'
// # Args:
// #   %r0 = value
// # Clobbers:
// #   %r0-%r3, %r4
// print_u64:
//         # Save caller-saved regs you care about (example uses r4)
//         push.w  %r4
// 
//         # Point %r1 at buffer and convert
//         load.l   %r1, buf
//         call    utoa10         # r0 = start, r1 = len
// 
//         # Append '\n' at end: buf[start + len] = '\n'
//         mov.w   %r2, %r0       # r2 = start
//         add.w   %r2, %r1       # r2 = start + len
//         movi    %r4, $10       # '\n'
//         store.b   %r4, (%r2)
// 
//         # len = len + 1 (include newline)
//         addi    %r1, $1
// 
//         # Prepare write syscall: write(1, start, len')
//         mov.w   %r2, %r0       # buf ptr
//         mov.w   %r3, %r1       # len
// 
//         movi   %r0, $1        # syscall write
//         movi   %r1, $1        # fd = stdout
//         trap
// 
//         pop.w   %r4
//         ret

# Stack-based helpers (no static buffer)
# This variant uses 24 bytes on the caller's stack for scratch space.
# The caller's stack pointer is restored before return.

# print_u64_stack: print unsigned 64-bit in %r0 followed by '\n'
# Args:
#   %r0 = value
# Clobbers:
#   %r1-%r4, %sp (temporary)
print_u64_stack:
        # Save caller-saved regs we want to restore (example uses r1)
        push.w  %r0

        # Reserve 24 bytes on the stack for the temporary buffer
        subi    %sp, $24

        # Use stack buffer: r1 = buf = sp
        mov.w   %r1, %sp

        call    utoa10         # r0 = start, r1 = len

        # Append '\n' at end: buf[start + len] = '\n'
        mov.w   %r2, %r0       # r2 = start
        add.w   %r2, %r1       # r2 = start + len
        movi    %r4, $10       # '\n'
        store.b %r4, (%r2)

        # len = len + 1 (include newline)
        addi    %r1, $1

        # Prepare write syscall: write(1, start, len')
        mov.w   %r2, %r0       # buf ptr
        mov.w   %r3, %r1       # len

        movi    %r0, $1        # syscall write
        movi    %r1, $1        # fd = stdout
        trap

        # Release stack scratch
        addi    %sp, $24
        pop.w   %r0
        ret
# print_i64_stack: print signed 64-bit in %r0 followed by '\n'
# Args:
#   %r0 = signed value (two's complement)
# Clobbers:
#   %r1-%r4, %sp (temporary)
# Preserves:
#   %r0 (restored to original value before return)
print_i64_stack:
        # Save original %r0 (so caller gets it back)
        push.w  %r0

        # Reserve 24 bytes on the stack for scratch buffer
        subi    %sp, $24

        # We'll treat %sp as the "base", but actually give utoa10 buffer at %sp+1
        # so we can write '-' at (%r0 - 1) later.
        mov.w   %r1, %sp
        addi    %r1, $1        # r1 = buf = sp + 1

        # Determine sign of original value in r0
        # We'll keep the working value in r2.
        mov.w   %r2, %r0       # r2 = signed input
        movi    %r3, $0        # r3 = 0
        brlts.w %r2, %r3, print_i64_neg   # if r2 < 0 (signed), go handle negative

print_i64_nonneg:
        # r0 still has the original (non-negative) value
        # r1 = buffer (sp+1)
        call    utoa10         # r0 = start, r1 = len
        jmp     print_i64_after_digits

print_i64_neg:
        # Compute magnitude as unsigned: mag = (~value) + 1
        # This is safe even for INT64_MIN.
        not.w   %r2            # r2 = ~value (bitwise NOT, 64-bit)
        addi    %r2, $1        # r2 = ~value + 1
        mov.w   %r0, %r2       # r0 = magnitude as unsigned

        # r1 is already buf = sp+1
        call    utoa10         # r0 = start, r1 = len

        # Now prepend '-' before the digits:
        subi    %r0, $1        # step back 1 byte to make room for '-'
        movi    %r4, $45       # ASCII '-' (45)
        store.b %r4, (%r0)
        addi    %r1, $1        # len += 1

print_i64_after_digits:
        # r0 = start pointer, r1 = length (digits [+ sign])
        # Append '\n' at start+len
        mov.w   %r2, %r0       # r2 = start
        add.w   %r2, %r1       # r2 = start + len
        movi    %r4, $10       # '\n'
        store.b %r4, (%r2)

        # total length = len + 1
        addi    %r1, $1

        # Prepare write syscall: write(1, start, len_total)
        mov.w   %r2, %r0       # buf ptr
        mov.w   %r3, %r1       # len
        movi    %r0, $1        # syscall write
        movi    %r1, $1        # fd = stdout
        trap

        # Release stack scratch and restore original %r0
        addi    %sp, $24
        pop.w   %r0
        ret
