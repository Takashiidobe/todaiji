# Todaiji ⛩️

Todaiji is a VM that's meant as a compiler target to lower into
assembly or bytecode.

Recently, I've been writing a lot of m68k code, so I wanted a 16-bit
opcode ISA of my own to try out. This is the result.

The docs themselves are in `docs/`. The first doc, `docs/todaiji_asm.md`,
gives a high-level overview of the instructions, effective addresses,
registers, sizes, all that you need to understand how the assembly
language itself works. There's a users guide as well in there,
`docs/writing_todaiji.md` that gives more information on how to write the
assembly language.

## Goals

At a high-level, I wanted:

- 16-bit opcodes (dense code)
- Simple decoding with grouping on nibble + minor (less branching)
- Support for frequent compiler operations 
- 16 registers (to reduce register pressure)
- a bare-bones set of effective addresses (only 4)
- supporting all common byte-sizes (takes up 2-bits)
- decent enough code density when compiled to binary (jmp S, ALU
  immediate ops)

Most of these are at odds with each other, which makes it hard to write
a "good" ISA. M68k is a straightforward assembler to write in, but what
I disliked most about it was the split of data registers and
address registers. Instead of having 16 GPRs, you have 8 that are for
storing addresses and 8 for storing data. This meant registers would fit
in 3-bits, as long as you provided the EA you wanted (3-bits + 3 extra
bits for the other register). I wanted 16 GPRs in my ISA, because I felt
only 8 registers was a problem; x86_32 has 8 GPRs, but after using the
stack pointer and the program counter, you're down to 6. Also, the frame
pointer was also sometimes used by programs, so you're down to 5, which
felt a little limiting.

But having 4-bits for registers means that any binary operation removes
8-bits from your 16-bit opcode. Take off another 4 for your group and
you have exactly 4 bits left. I wanted a 2-bit minor too, so each group
could support 4 instructions, so that only left a 2-bits, which is for
the size. Thus, ALU ops have this format:

|4-bit nibble|2-bit minor|2-bit size|4-bit dst reg|4-bit src reg|

So, you need to have expressive loading options. That's done with `lea`,
load effective address, load, and store.

Lea, load, and store get a whole group to itself, since it needs 2
registers, an EA, and size.

LEA: |0000|2-bit size|2-bit src EA|4-bit dst reg|4-bit src reg|

LOAD: |1001|2-bit size|2-bit src EA|4-bit dst reg|4-bit src reg|

STORE: |1010|2-bit size|2-bit dst EA|4-bit src reg|4-bit dst reg|

So you'll need to write a lot of loads/stores before the direct register
to register ops, which is a shame for writing the assembly by hand. For
a compiler, I figured it wouldn't matter as much (except for instruction
density) but I couldn't figure out a way to add EAs to the already
crowded ALU ops.
