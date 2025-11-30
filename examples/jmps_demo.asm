# Demonstrate jmps (short jump) instruction
# Count down from 10 using a short loop

        movi %r0, $10      # counter

loop:
        subi %r0, $1       # decrement
				call print_u64_stack
        brz.l %r0, done    # if zero, exit loop
        jmps loop          # short jump back to loop (saves space vs jmp)

done:
        nop
.include "./print.asm"
