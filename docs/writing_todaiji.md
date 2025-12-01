# Writing Todaiji Assembly: A User Guide

This guide will help you write assembly code for the Todaiji ISA, a 
compact 16-bit instruction set architecture.

## Table of Contents

1. [Overview](#overview)
2. [Register Model](#register-model)
3. [Syntax Basics](#syntax-basics)
4. [Data Sizes](#data-sizes)
5. [Addressing Modes](#addressing-modes)
6. [Instruction Reference](#instruction-reference)
7. [Complete Examples](#complete-examples)

## Overview

The Todaiji ISA is a 16-bit instruction set with:
- 16 general-purpose 64-bit registers
- Fixed 16-bit instruction words (some instructions use extension words)
- Support for 8, 16, 32, and 64-bit operations
- PC-relative and absolute addressing
- Stack-based calling convention

## Register Model

### General Purpose Registers

The ISA provides 16 registers named `%r0` through `%r15`:

```asm
%r0, %r1, %r2, ... %r15
```

### Special Registers

Three registers have special purposes:

- **`%r13` (alias: `%fp`)**: Frame Pointer
- **`%r14` (alias: `%pc`)**: Program Counter
- **`%r15` (alias: `%sp`)**: Stack Pointer

You can use either the numeric form (`%r15`) or the alias (`%sp`).

### Register Usage Convention

While there's no enforced calling convention, the fibonacci example demonstrates a common pattern:
- `%r0`: Return value
- `%r1-r3`: Function arguments and temporaries
- `%sp`: Stack pointer (grows downward)

## Syntax Basics

### Comments

Use `#` or `//` for comments:

```asm
# This is a comment
movi %r0, $42    // This is also a comment
```

### Labels

Define labels with a colon suffix. Labels mark positions in code for jumps and branches:

```asm
loop_start:
    subi %r0, $1
    brnz.l %r0, loop_start
```

### Instruction Format

Most instructions follow this pattern:

```asm
opcode.size destination, source
```

Examples:
```asm
mov.l %r0, %r1          # Move r1 to r0 (long/32-bit)
add.w %r2, %r3          # Add r3 to r2 (word/64-bit)
subi  %r0, $5           # Subtract immediate 5 from r0
```

## Data Sizes

Instructions that operate on data use size suffixes:

 | Suffix | Size  | Bits | C Type  |
 |--------|-------|------|---------|
 | `.b`   | Byte  | 8    | `char`  |
 | `.s`   | Short | 16   | `short` |
 | `.l`   | Long  | 32   | `int`   |
 | `.w`   | Word  | 64   | `long`  |

Example:
```asm
mov.b %r0, $42    # Move 8-bit value 42
mov.w %r0, $1000  # Move 64-bit value 1000
```

## Addressing Modes

### Immediate (`$value`)

A constant value:
```asm
movi %r0, $100        # r0 = 300 
addi %r1, $10         # r1 += 10
```

### Register Direct (`%rX`)

A register value:
```asm
mov.l %r0, %r1        # r0 = r1
add.l %r2, %r3        # r2 += r3
```

### Register Indirect (`(%rX)`)

Value at memory address in register:
```asm
load.l %r0, (%r1)     # r0 = memory[r1]
store.l %r2, (%r3)    # memory[r3] = r2
```

### Base + Displacement (`offset(%rX)`)

Memory at register plus constant offset:
```asm
load.l %r0, 8(%sp)    # r0 = memory[sp + 8]
store.l %r1, -4(%sp)  # memory[sp - 4] = r1
```

### Labels

For jumps and branches, use label names:
```asm
jmp end_loop
brz.l %r0, done
```

## Instruction Reference

### Data Movement

#### `mov.size dest, src`
Register-to-register copy only. 
Use `movi` for immediates and `load`/`store` for memory or constants.
```asm
mov.l %r0, %r1        # r0 = r1
movi %r2, $64         # r2 = 64
```

#### `lea.size dest, address`
Load effective address (compute address without dereferencing).
```asm
lea.l %r0, 8(%sp)     # r0 = address of (sp + 8)
```

#### `load.size dest, address`
Load from memory.
```asm
load.l %r0, (%r1)     # r0 = memory[r1]
load.w %r2, 16(%sp)   # r2 = memory[sp + 16]
```

#### `store.size src, address`
Store to memory.
```asm
store.l %r0, (%r1)    # memory[r1] = r0
store.w %r2, -8(%sp)  # memory[sp - 8] = r2
```

### Arithmetic

#### `add.size dest, src` / `addi.size dest, imm`
Addition. Register-register form modifies dest. Immediate form takes 1..=64.
```asm
add.l %r0, %r1        # r0 += r1
addi %r0, $5          # r0 += 5
```

#### `sub.size dest, src` / `subi.size dest, imm`
Subtraction.
```asm
sub.l %r0, %r1        # r0 -= r1
subi %r0, $1          # r0 -= 1 (decrement)
```

#### `mul.size dest, src` / `muli.size dest, imm`
Multiplication.
```asm
mul.l %r0, %r1        # r0 *= r1
muli %r0, $10         # r0 *= 10
```

#### `divmod.size dest, src` (signed) / `divmodu.size dest, src` (unsigned)
Compute quotient and remainder together. On entry: `dest` = dividend, `src` =
divisor. On exit: `dest` = quotient, `src` = remainder.
There is no immediate form.
```asm
divmod.l %r0, %r1     # signed: r0 = q, r1 = r
divmodu.l %r0, %r1    # unsigned: r0 = q, r1 = r
```

### Logical Operations

#### `and.size dest, src`
Bitwise AND.
```asm
and.l %r0, %r1        # r0 &= r1
```

#### `or.size dest, src`
Bitwise OR.
```asm
or.l %r0, %r1         # r0 |= r1
```

#### `xor.size dest, src`
Bitwise XOR.
```asm
xor.l %r0, %r1        # r0 ^= r1
```

#### `not.size dest`
Bitwise NOT (one's complement).
```asm
not.l %r0             # r0 = ~r0
```

#### `neg.size dest`
Arithmetic negation (two's complement).
```asm
neg.l %r0             # r0 = -r0
```

### Comparisons to boolean

#### `cmpeq.size dest, src` / `cmpne.size dest, src`
Set `dest` to 1 or 0 based on equality.
```asm
cmpeq.l %r0, %r1      # r0 = (r0 == r1) ? 1 : 0
cmpne.l %r2, %r3      # r2 = (r2 != r3) ? 1 : 0
```

#### `cmpltu.size dest, src` / `cmplt.size dest, src`
Unsigned / signed less-than. Other relations derive by swapping operands or
`not $dest` to invert.
```asm
cmpltu.l %r0, %r1     # r0 = (r0 < r1) ? 1 : 0 (unsigned)
cmplt.l %r2, %r3     # r2 = (r2 < r3) ? 1 : 0 (signed)
```

### Bit Shifts

#### `shl.size dest, count`
Logical left shift. Count comes from a register.
```asm
shl.l %r0, %r1        # r0 <<= r1
```

#### `shr.size dest, count`
Logical right shift (zero-fill). Count comes from a register.
```asm
shr.l %r0, %r1        # logical right
```

#### `sar.size dest, count`
Arithmetic right shift (sign-extend). Count comes from a register.
```asm
sar.l %r0, %r1        # arithmetic right
```

### Control Flow - Unconditional

#### `fence mode`
Ordering primitive. Modes: 00=SeqCst/full, 01=Acquire, 10=Release, 11=IO/Device.

#### `jmp label`
Absolute jump to label or address.
```asm
jmp end_program
```

#### `jmpi.size address`
Indirect jump through register/memory.
```asm
jmpi.w (%r0)          # Jump to address in r0
```

#### `call label` / `calli.size address`
Call function (pushes return address, then jumps).
```asm
call fibonacci        # Call function
calli.w (%r0)         # Indirect call
```

#### `ret`
Return from function (pops return address and jumps to it).
```asm
ret
```

### Control Flow - Conditional Branches

All branches take two operands to compare and a label for the branch target.

#### Unsigned Comparisons

```asm
breq.(b/s/l/w) reg1, reg2, label    # Branch if equal
brne.(b/s/l/w) reg1, reg2, label    # Branch if not equal
brlt.(b/s/l/w) reg1, reg2, label    # Branch if less than (unsigned)
brlts.(b/s/l/w) reg1, reg2, label   # Branch if less than (signed)
```

#### Zero Tests

```asm
brz.(b/s/l/w) size reg, label            # Branch if zero
brnz.(b/s/l/w) size reg, label           # Branch if not zero
```

#### `jmps label`
**Short jump** (PC-relative, +-512 instructions). Lives in the branch group.
```asm
jmps loop_start       # Jump backward to loop
```

Example:
```asm
    subi.l %r0, $1
    brz.l %r0, done       # if r0 == 0, goto done
    jmps loop             # else continue loop
done:
```

### Stack Operations

The stack grows downward (towards lower addresses).

#### `push.size value`
Push value onto stack (decrements SP, then stores).
```asm
push.w %r0            # Push r0 onto stack
push.l $42            # Push immediate onto stack
```

#### `pop.size dest`
Pop value from stack (loads, then increments SP).
```asm
pop.w %r0             # Pop into r0
```

### Bit Extension

#### `sxt.size dest, src`
Sign-extend (preserve sign bit).
```asm
sxt.l %r0, %r1        # Sign-extend r1 to r0
```

#### `zxt.size dest, src`
Zero-extend (fill with zeros).
```asm
zxt.l %r0, %r1        # Zero-extend r1 to r0
```

### Miscellaneous

#### `nop`
No operation (does nothing).
```asm
nop
```

#### `trap`
Trap/system call (implementation defined).
```asm
trap
```

### Atomics

#### `cas.size expect_reg, new_reg, ea`
Atomic compare-and-swap. On entry: `expect_reg` has the expected value, `new_reg`
has the replacement, and `ea` points to memory. Atomically compare memory with
`expect_reg`; if equal, store `new_reg`. `expect_reg` is overwritten with the old
memory value; success if it is unchanged.

Example: spin until a lock word becomes 0, then set it to 1
```asm
lock_acquire:
        movi.l  %r0, $0        # expect = 0
        movi.l  %r1, $1        # new = 1
        movi.l  %r2, $0        # zero for branch compare
try:
        cas.l   %r0, %r1, (lockword)
        brne.l  %r0, %r2, try  # loop while old != 0
```

#### `xchg.size swap_reg, ea`
Atomic exchange. Atomically swap `swap_reg` with memory at `ea`. Returns the old
memory value in `swap_reg`.

Example: test-and-set using xchg (r0 gets previous value)
```asm
        movi.b  %r0, $1
        movi.b  %r1, $0
spin:
        xchg.b  %r0, (lockbyte)    # r0 = old lock, lockbyte = 1
        brne.b  %r0, %r1, spin     # if old != 0, someone else held it
        # lock held
```

#### `swap.size dest, src`
Exchange values between two registers.
```asm
swap.l %r0, %r1       # Swap r0 and r1
```

### Directives

### `.byte`
Provide a comma separated list of bytes to be assembled as a byte array.

```asm
msg:
        // This is assembled as the string: "Hello world\n"
        .byte 104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 10 
```

### `.ascii`
Provide a string that will be referred to by the provided label.

```asm
msg:
        .ascii "hello asciz\n"
```
### `.asciz`
Provide a string that will be referred to by the provided label. 
The string is followed by a `\0` byte.

```asm
msg:
        .asciz "hello asciz\n"
```

## Complete Examples

### Example 1: Simple Loop (Countdown)

Count down from 10 to 0:

```asm
        movi %r0, $10        # Initialize counter

loop:
        subi.l %r0, $1        # Decrement
        brnz.l %r0, loop      # Continue if not zero

        # r0 is now 0
        nop
```

### Example 2: Sum of Numbers 1 to N

Calculate sum = 1 + 2 + 3 + ... + N:

```asm
        movi %r0, $0         # sum = 0
        movi %r1, $10        # n = 10

loop:
        add.l %r0, %r1        # sum += n
        subi  %r1, $1         # n -= 1
        brnz.l %r1, loop      # continue if n != 0

        # Result in r0
        nop
```

### Example 3: Function with Stack Frame

Simple function that adds 5 to its argument:

```asm
main:
        movi %r0, $10        # Argument
        call add_five         # Call function
        jmp done              # Skip function definition

add_five:
        # Prologue: save %r0
        push.w %r1

        # Function body
        movi  %r1, %r0
        addi  %r1, $5
        mov.l %r0, %r1        # Return value in r0

        # Epilogue: restore %r0
        pop.w %r0
        ret

done:
        nop
```

### Example 4: Recursive Fibonacci

Calculate the Nth fibonacci number (from examples/fib.asm):

```asm
# Calculate fib(10)
        movi %r1, $10        # n = 10
        call fib
        jmp done

fib:
        # Prologue: save caller-saved registers
        push.w %r2
        push.w %r3

        # Base case: if n == 0, return 0
        brz.l %r1, base_case

        # Base case: if n == 1, return 1
        mov.l %r3, %r1
        subi.l %r3, $1
        brz.l %r3, base_case

        # Recursive case: fib(n-1)
        subi  %r1, $1
        push.w %r1            # Save n-1
        call fib
        mov.l %r2, %r0        # r2 = fib(n-1)
        pop.w %r1             # Restore n-1

        # fib(n-2)
        subi.l %r1, $1
        push.w %r1
        call fib
        mov.l %r3, %r0        # r3 = fib(n-2)
        pop.w %r1

        # Return fib(n-1) + fib(n-2)
        mov.l %r0, %r2
        add.l %r0, %r3
        jmp end_fib

base_case:
        mov.l %r0, %r1        # Return n

end_fib:
        # Epilogue: restore registers
        pop.w %r3
        pop.w %r2
        ret

done:
        nop
```

### Example 5: Array Access

Access array elements using base + displacement addressing:

```asm
        # Assume array starts at address in %r1
        # Access array[2] (assuming 4-byte elements)

        movi %r1,  $0x1000    # Base address
        load.l %r0, 8(%r1)    # Load array[2] (2 * 4 = 8 bytes offset)

        # Modify and store back
        addi.l %r0, $10
        store.l %r0, 8(%r1)   # Store back to array[2]
```

### Example 6: Syscalls

You can call syscalls with trap:

```asm
        movi %r0, $1         # syscall write 
        movi %r1, $1         # stdout
        load.l %r2, msg      # r2 = &msg
        movi %r3, $12        # length
        trap                 # perform write
        ret

msg:
        .asciz "hello asciz\n"
```

## Tips and Best Practices

### 1. Use Short Jumps for Loops

Prefer `jmps` over `jmp` for nearby jumps (within +-512 instructions) to save space:

```asm
loop:
        subi   %r0, $1
        brnz.l %r0, loop      # Branch to check condition
        jmps loop             # Or use jmps for unconditional backward jump
```

### 2. Preserve Caller-Saved Registers

If your function uses `%r2` and `%r3`, save them with `push`/`pop`:

```asm
my_function:
        push.w %r2
        push.w %r3

        # ... use r2 and r3 ...

        pop.w %r3
        pop.w %r2
        ret
```

### 3. Stack Alignment

Keep track of stack pushes/pops to maintain balance:

```asm
        push.w %r1            # SP -= 8
        push.w %r2            # SP -= 8
        # ... do work ...
        pop.w %r2             # SP += 8
        pop.w %r1             # SP += 8
```

### 4. Use Appropriate Sizes

Use the smallest size that works to save space and improve clarity:

```asm
        mov.b %r0, $10        # For small values (0-255)
        mov.l %r0, $1000000   # For larger values
```

### 5. Label Organization

Use descriptive labels and organize code clearly:

```asm
main:
        # Main program logic
        call initialize
        call process
        jmp exit

initialize:
        # Initialization code
        ret

process:
        # Processing logic
        ret

exit:
        nop
```

## Error Handling

### Common Errors

1. **Division by zero**: Check before dividing
   ```asm
        brz.l %r1, skip_div
        divmodu.l %r0, %r1   # r0 = quotient, r1 = remainder (both clobbered)
skip_div:
   ```

2. **Stack underflow**: Balance push/pop operations

3. **Out of range jumps**: `jmps` limited to +-512 instructions

4. **Unknown labels**: All referenced labels must be defined

## Running Your Code

Save your assembly to a `.asm` file and run:

```bash
cargo run -- my_program.asm
```

The program will execute and display final register values.

## Further Reading

- See `docs/todaiji_asm.md` for the complete ISA specification
- Check `examples/` directory for more example programs
- Study `examples/fib.asm` for a complete recursive function example
