# Todaiji Assembly Features

- Opcodes encode in 16-bits
- 16 registers (4-bits)
- No condition flags
- 2-bit effective addresses
- 2-bit sizes (8/16/32/64-bit)
- Stack pointer is `%r15`
- Program counter is `%r14`
- Frame pointer is `%r13`

## Sizes

- 0b00 = (b)yte
- 0b01 = (s)hort
- 0b10 = (l)ong
- 0b11 = (w)ord

## Effective Addresses

- Normal 2-bit EA

- 0b00 = register direct - `%rX`
- 0b01 = register indirect - `(%rX)`
- 0b10 = base + displacement - `disp(%rX)`
- 0b11 = immediate - `#imm`

- Sxt/Zxt 2-bit EA (no immediate form)

- 0b00 = register direct - `%rX`
- 0b01 = register indirect - `(%rX)`
- 0b10 = base + displacement - `disp(%rX)`
- 0b11 = unused

### EA extension widths and side effects

- Displacements are sign-extended; width follows the 2-bit size field: 00=8-bit, 01=16-bit, 10=32-bit, 11=64-bit.
- Immediates use the same size-based width mapping for inline data.
- Scaled forms place index register and scale in an extension word (index reg: 4 bits, scale: 2 bits for 1/2/4/8). Any displacement in these modes also follows the size-based width mapping above.

### Condition evaluation

- There are no condition flags. Use branch (`br`) instructions for
  conditional evaluation.

### Stack conventions

- Stack grows down in memory. `%sp` (alias `%r15`) points to the top of stack.
- Push/pop adjust `%sp` by the operand size in bytes (1, 2, 4, 8) before/after the memory access.
- `Call` pushes the return address to the stack before transferring control; `Ret` pops it into `%pc`.

### Register aliases

- `%sp` is an alias for `%r15`.
- `%pc` is an alias for `%r14`.
- `%fp` is an alias for `%r13`.

## Groups

- Group 0000: LEA only (uses full 12-bit payload)
- Group 0001: ALU reg-reg, 2-bit minor (10-bit body: 2-bit size + dst reg + src reg)
  - minor 00: Add
  - minor 01: Sub
  - minor 10: Mul
  - minor 11: Divmodu (unsigned)
- Group 0010: Logic reg-reg, 2-bit minor (10-bit body: 2-bit size + dst reg + src reg)
  - minor 00: And
  - minor 01: Or
  - minor 10: Xor
  - minor 11: Swap
- Group 0011: Unary ops, 2-bit minor (10-bit body: 2-bit size + 2-bit EA + 4-bit dst reg + 2 bits reserved)
  - minor 00: Not (bitwise) — EA must be register direct
  - minor 01: Neg (arithmetic) — EA must be register direct
  - minor 10: Sxt (sign-extend) — uses Sxt/Zxt EA set (no immediate)
  - minor 11: Zxt (zero-extend) — uses Sxt/Zxt EA set (no immediate)
- Group 0100: Conditional branches, 2-bit minor (10-bit body: 2-bit size + dst reg + src reg for reg-reg forms)
  - minor 00: BrEQ
  - minor 01: BrNE
  - minor 10: BrLT (unsigned)
  - minor 11: BrLTS (signed)
- Group 0101: Control transfer / zero-test branches, 2-bit minor
  - minor 00: BrZ (zero-test), 10-bit body (2-bit size + 2-bit EA + 4-bit reg + 2 bits reserved)
  - minor 01: BrNZ (nonzero-test), 10-bit body (2-bit size + 2-bit EA + 4-bit reg + 2 bits reserved)
  - minor 10: Jmps (Jump short) - PC-relative jump with signed 10-bit offset (-512 to +511 instructions)
  - minor 11: reserved
- Group 0110: Stack/control/move, 2-bit minor
  - minor 00: Ret (opcode+minor only; pops return address into %pc)
  - minor 01: Mov (reg-direct), 10-bit body (2-bit size + 4-bit dst reg + 4-bit src reg)
  - minor 10: Push 10-bit body (2-bit size + 2-bit src EA + 4-bit src reg; remaining bits reserved)
  - minor 11: Pop 10-bit body (2-bit size + 2-bit dst EA + 4-bit dst reg; remaining bits reserved)
- Group 0111: Jumps and fence (absolute/indirect/misc), 4-bit minor (8-bit body unless noted)
  - minor 0000: Jmp (opcode+minor only; target in following word(s))
  - minor 0001: Call (opcode+minor only; target in following word(s))
  - minor 0010: Jmpi (indirect) - 2-bit size + 2-bit EA + 4-bit target reg/base
  - minor 0011: Calli (indirect) - 2-bit size + 2-bit EA + 4-bit target reg/base
  - minor 0100: Fence (2-bit mode; remaining bits reserved)
  - minor 0110: Trap 
  - minor 0111: Nop
- Group 1000: Compare-to-boolean reg-reg, 2-bit minor (10-bit body: 2-bit size + dst reg + src reg; dst is set to 0/1)
  - minor 00: Cmpeq
  - minor 01: Cmpne
  - minor 10: Cmpltu (unsigned less-than)
  - minor 11: Cmplt (signed less-than)
- Group 1001: Load (12-bit body: 2-bit size + 2-bit EA + 4-bit dst reg + 4-bit base/index reg)
- Group 1010: Store (12-bit body: 2-bit size + 2-bit EA + 4-bit src reg + 4-bit base/index reg)
- Group 1011: ALU/Mov imm, 2-bit minor (12-bit body: 2-bit size + 4-bit dst reg + 6-bit unsigned immediate)
  - minor 00: Addi
  - minor 01: Subi
  - minor 10: Muli
  - minor 11: Movi 
- Group 1100: Shifts/Divmod reg-reg, 2-bit minor (10-bit body: 2-bit size + dst reg + src reg; dst and src are clobbered)
  - minor 00: Divmod (signed) — dst=quotient, src=remainder
  - minor 01: Shl — dst `<<=` src (count in src)
  - minor 10: Shr — logical right, zero-fill (count in src)
  - minor 11: Sar — arithmetic right, sign-extend (count in src)
- Group 1101: CAS (12-bit body: 2-bit size + 2-bit EA + 4-bit base/index reg + 4-bit expect/old reg; extension nibble names new_reg)
- Group 1111: Xchg (12-bit body: 2-bit size + 2-bit EA + 4-bit base/index reg + 4-bit swap reg)
