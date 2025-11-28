# Demonstrate jmps (short jump) instruction
# Count down from 10 using a short loop

        mov.l %r0, $10     # counter

loop:
        subi.l %r0, $1     # decrement
        brz.l %r0, done    # if zero, exit loop
        jmps loop          # short jump back to loop (saves space vs jmp)

done:
        nop
