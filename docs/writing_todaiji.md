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

Two registers have special purposes:

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
mov.l %r0, $42    // This is also a comment
```

### Labels

Define labels with a colon suffix. Labels mark positions in code for jumps and branches:

```asm
loop_start:
    subi.l %r0, $1
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
subi.l %r0, $5          # Subtract immediate 5 from r0
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
mov.l %r0, $42        # r0 = 42
addi.l %r1, $10       # r1 += 10
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
Copy data from source to destination.
```asm
mov.l %r0, %r1        # r0 = r1
mov.w %r2, $100       # r2 = 100
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
store.w %r2, 8(%sp)   # memory[sp + 8] = r2
```

### Arithmetic

#### `add.size dest, src` / `addi.size dest, imm`
Addition. Register-register form modifies dest. Immediate form adds -32..31.
```asm
add.l %r0, %r1        # r0 += r1
addi.l %r0, $5        # r0 += 5
```

#### `sub.size dest, src` / `subi.size dest, imm`
Subtraction.
```asm
sub.l %r0, %r1        # r0 -= r1
subi.l %r0, $1        # r0 -= 1 (decrement)
```

#### `mul.size dest, src` / `muli.size dest, imm`
Multiplication.
```asm
mul.l %r0, %r1        # r0 *= r1
muli.l %r0, $10       # r0 *= 10
```

#### `div.size dest, src` (signed) / `divu.size dest, src` (unsigned)
Division.
```asm
div.l %r0, %r1        # r0 /= r1 (signed)
divu.l %r0, %r1       # r0 /= r1 (unsigned)
```

#### `rem.size dest, src` / `remi.size dest, imm`
Remainder (modulo).
```asm
rem.l %r0, %r1        # r0 %= r1
remi.l %r0, $10       # r0 %= 10
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

### Bit Shifts and Rotates

#### `shl.size dest, count` / `shr.size dest, count`
Shift left/right. Count can be register or immediate.
```asm
shl.l %r0, %r1        # r0 <<= r1
shl.l %r0, $2         # r0 <<= 2
```

#### `rol.size dest, count` / `ror.size dest, count`
Rotate left/right (circular shift).
```asm
rol.l %r0, %r1        # Rotate r0 left by r1 bits
```

### Control Flow - Unconditional

#### `jmp label` / `jmp address`
Absolute jump to label or address.
```asm
jmp end_program
jmp 0(%pc)            # Jump to current PC
```

#### `jmps label`
**Short jump** (PC-relative, +-512 instructions). More compact than `jmp`.
```asm
jmps loop_start       # Jump backward to loop
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
brge.(b/s/l/w) reg1, reg2, label    # Branch if greater/equal (unsigned)
```

#### Signed Comparisons

```asm
brlts.(b/s/l/w) reg1, reg2, label   # Branch if less than (signed)
brges.(b/s/l/w) reg1, reg2, label   # Branch if greater/equal (signed)
```

#### Zero Tests

```asm
brz.(b/s/l/w) size reg, label            # Branch if zero
brnz.(b/s/l/w) size reg, label           # Branch if not zero
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

#### `swap.size dest, src`
Exchange values between two registers.
```asm
swap.l %r0, %r1       # Swap r0 and r1
```

## Complete Examples

### Example 1: Simple Loop (Countdown)

Count down from 10 to 0:

```asm
        mov.l %r0, $10        # Initialize counter

loop:
        subi.l %r0, $1        # Decrement
        brnz.l %r0, loop      # Continue if not zero

        # r0 is now 0
        nop
```

### Example 2: Sum of Numbers 1 to N

Calculate sum = 1 + 2 + 3 + ... + N:

```asm
        mov.l %r0, $0         # sum = 0
        mov.l %r1, $10        # n = 10

loop:
        add.l %r0, %r1        # sum += n
        subi.l %r1, $1        # n -= 1
        brnz.l %r1, loop      # continue if n != 0

        # Result in r0
        nop
```

### Example 3: Function with Stack Frame

Simple function that adds 5 to its argument:

```asm
main:
        mov.l %r0, $10        # Argument
        call add_five         # Call function
        jmp done              # Skip function definition

add_five:
        # Prologue: save registers
        push.w %r1

        # Function body
        mov.l %r1, %r0
        addi.l %r1, $5
        mov.l %r0, %r1        # Return value in r0

        # Epilogue: restore registers
        pop.w %r1
        ret

done:
        nop
```

### Example 4: Recursive Fibonacci

Calculate the Nth fibonacci number (from examples/fib.asm):

```asm
# Calculate fib(10)
        mov.l %r1, $10        # n = 10
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
        subi.l %r1, $1
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

        mov.l %r1, $0x1000    # Base address
        load.l %r0, 8(%r1)    # Load array[2] (2 * 4 = 8 bytes offset)

        # Modify and store back
        addi.l %r0, $10
        store.l %r0, 8(%r1)   # Store back to array[2]
```

## Tips and Best Practices

### 1. Use Short Jumps for Loops

Prefer `jmps` over `jmp` for nearby jumps (within +-512 instructions) to save space:

```asm
loop:
        subi.l %r0, $1
        brnz.l %r0, loop      # Branch to check condition
        jmps loop             # Or use jmps for unconditional backward jump
```

### 2. Preserve Caller-Saved Registers

If your function uses `%r2` and `%r3`, save them:

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
        div.l %r0, %r1
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
