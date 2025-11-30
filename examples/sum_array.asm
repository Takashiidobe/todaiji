# Build an array with values 0..32 and sum them.
# Result: %r0 holds the total (528) and we return.
# Array lives in the data segment; we use base+offset addressing.

arr:
        .byte 0                # anchor label; memory is zeroed at runtime

start:
        # r1 = base of array
        load.s  %r1, arr

        # Fill array with i = 0..32 using offset-indirect stores
        movi    %r2, $0        # i
        mov.s   %r3, %r1       # ptr = base

fill_loop:
        store.s %r2, (%r3)     # arr[i] = i
        addi    %r3, $2        # advance ptr (2 bytes per element)
        addi    %r2, $1        # i++
        brlt.s  %r2, $33, fill_loop   # loop while i < 33

        # Sum the array into r0
        movi    %r0, $0        # sum
        load.s  %r1, arr
        movi    %r2, $0        # idx
        mov.s   %r3, %r1       # ptr = base

sum_loop:
        load.s  %r4, (%r3)     # r4 = arr[idx]
        add.s   %r0, %r4       # sum += r4
        addi    %r3, $2        # ptr += 2
        addi    %r2, $1        # idx++
        brlt.s  %r2, $33, sum_loop
				call print_i64_stack

        ret
.include "./print.asm"
