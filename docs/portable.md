# Portable Assembly Set

- 16 registers

## Sizes

- 0b00 = (b)yte
- 0b01 = (s)hort
- 0b10 = (l)ong
- 0b11 = (w)ord

## Effective Addresses

- Normal 2-bit EA

- 0b00 = register indirect - `(%rX)`
- 0b01 = base + displacement - `disp(%rX)`
- 0b10 = scaled `(base + index<<scale + optional disp, via extension word)`
- 0b11 = immediate - `#imm`

- Sxt/Zxt 2-bit EA (no immediate form)

- 0b00 = register direct - `%rX`
- 0b01 = register indirect - `(%rX)`
- 0b10 = base + displacement - `disp(%rX)`
- 0b11 = scaled `(base + index<<scale + optional disp, via extension word)`

### EA extension widths and side effects

- Displacements are sign-extended; width follows the 2-bit size field: 00=8-bit, 01=16-bit, 10=32-bit, 11=64-bit.
- Immediates use the same size-based width mapping for inline data.
- Scaled forms place index register and scale in an extension word (index reg: 4 bits, scale: 2 bits for 1/2/4/8). Any displacement in these modes also follows the size-based width mapping above.

### Condition evaluation

- There is no condition-code/flag register. Conditional branches compare their operands directly according to the mnemonic (e.g., EQ/NE/LT/GE unsigned, LTS/GES signed), based on the selected operand size.

### Stack conventions

- Stack grows down in memory. `%sp` (alias `%r15`) points to the top of stack.
- Push/pop adjust `%sp` by the operand size in bytes (1, 2, 4, 8) before/after the memory access.
- `Call` pushes the return address to the stack before transferring control; `Ret` pops it into `%pc`.

### Register aliases

- `%sp` is an alias for `%r15`.
- `%pc` is an alias for `%r14`.

## Groups

- Group 0000: LEA only (uses full 12-bit payload)
- Group 0001: ALU reg-reg, 2-bit minor (10-bit body: 2-bit size + dst reg + src reg)
  - minor 00: Add
  - minor 01: Sub
  - minor 10: Mul
  - minor 11: Div
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
- Group 0100: Conditional branches (unsigned), 2-bit minor (10-bit body: 2-bit size + dst reg + src reg)
  - minor 00: BrEQ
  - minor 01: BrNE
  - minor 10: BrLT
  - minor 11: BrGE
- Group 0101: Control transfer / signed branches, 2-bit minor
  - minor 00: BrLTS (signed), 10-bit body (2-bit size + dst reg + src reg)
  - minor 01: BrGES (signed), 10-bit body (2-bit size + dst reg + src reg)
  - minor 10: BrZ (zero-test), 10-bit body (2-bit size + 2-bit EA + 4-bit reg + 2 bits reserved)
  - minor 11: BrNZ (nonzero-test), 10-bit body (2-bit size + 2-bit EA + 4-bit reg + 2 bits reserved)
- Group 0110: Stack/control/move, 2-bit minor
  - minor 00: Ret (opcode+minor only; pops return address into %pc)
  - minor 01: Mov (reg-direct), 10-bit body (2-bit size + dst reg + src reg)
  - minor 10: Push, 10-bit body (2-bit size + 2-bit src EA + 4-bit src reg; remaining bits reserved)
  - minor 11: Pop, 10-bit body (2-bit size + 2-bit dst EA + 4-bit dst reg; remaining bits reserved)
- Group 0111: Jumps/shifts/rotates, 4-bit minor (8-bit body unless noted)
  - minor 0000: Jmp (opcode+minor only; target in following word(s))
  - minor 0001: Call (opcode+minor only; target in following word(s))
  - minor 0010: Jmpi (indirect) - 2-bit size + 2-bit EA + 4-bit target reg/base
  - minor 0011: Calli (indirect) - 2-bit size + 2-bit EA + 4-bit target reg/base
  - minor 0100: Shl (2-bit size + 2-bit EA + 4-bit dst reg; EA selects count source: reg direct or immediate via extension)
  - minor 0101: Rol (2-bit size + 2-bit EA + 4-bit dst reg; EA selects count source: reg direct or immediate via extension)
  - minor 0110: Shr (2-bit size + 2-bit EA + 4-bit dst reg; EA selects count source: reg direct or immediate via extension)
  - minor 0111: Ror (2-bit size + 2-bit EA + 4-bit dst reg; EA selects count source: reg direct or immediate via extension)
  - minor 1000-1111: reserved
- Group 1000: Trap/Nop, 2-bit minor (opcode+minor only)
  - minor 00: Trap
  - minor 01: Nop
  - minor 10: reserved
  - minor 11: reserved
- Group 1001: Load (12-bit body: 2-bit size + 2-bit EA + 4-bit dst reg + 4-bit base/index reg)
- Group 1010: Store (12-bit body: 2-bit size + 2-bit EA + 4-bit src reg + 4-bit base/index reg)
- Group 1011: ALU imm, 2-bit minor (12-bit body: 2-bit size + 4-bit dst reg + 6-bit signed immediate)
  - minor 00: Addi
  - minor 01: Subi
  - minor 10: Muli
  - minor 11: Remi (signed)
- Group 1100: Div/Rem reg-reg, 2-bit minor (10-bit body: 2-bit size + dst reg + src reg)
  - minor 00: Div (signed)
  - minor 01: Divu (unsigned)
  - minor 10: Rem (signed)
  - minor 11: Remu (unsigned)
- Group 1101: Short jumps, 2-bit minor (10-bit body: signed 10-bit offset)
  - minor 00: Jmps (Jump short) - PC-relative jump with signed 10-bit offset (-512 to +511 instructions)
  - minor 01-11: reserved
- Remaining groups: reserved for expansion

## Instructions

Groups take up 4 bits. So each instruction has 12 bits left.

1. Lea (Load Effective Address) - group 0000: 2-bit size, 2-bit src EA, 4-bit dst reg, 4-bit src reg

2. Add (reg-reg) - group 0001 minor 00: 2-bit size, 4-bit dst reg, 4-bit src reg
3. Sub (reg-reg) - group 0001 minor 01: 2-bit size, 4-bit dst reg, 4-bit src reg
4. Mul (reg-reg) - group 0001 minor 10: 2-bit size, 4-bit dst reg, 4-bit src reg
5. Div (reg-reg) - group 0001 minor 11: 2-bit size, 4-bit dst reg, 4-bit src reg

6. And (reg-reg) - group 0010 minor 00: 2-bit size, 4-bit dst reg, 4-bit src reg
7. Or (reg-reg) - group 0010 minor 01: 2-bit size, 4-bit dst reg, 4-bit src reg
8. Xor (reg-reg) - group 0010 minor 10: 2-bit size, 4-bit dst reg, 4-bit src reg
9. Swap (reg-reg) - group 0010 minor 11: 2-bit size, 4-bit dst reg, 4-bit src reg

10. Not (unary) - group 0011 minor 00: 2-bit size, 2-bit EA (must be reg direct), 4-bit dst reg
11. Neg (unary) - group 0011 minor 01: 2-bit size, 2-bit EA (must be reg direct), 4-bit dst reg
12. Sxt (sign-extend) - group 0011 minor 10: 2-bit size, 2-bit EA (no immediate), 4-bit dst reg
13. Zxt (zero-extend) - group 0011 minor 11: 2-bit size, 2-bit EA (no immediate), 4-bit dst reg

14. BrEQ (Branch If Equal, unsigned) - group 0100 minor 00: 2-bit size, 4-bit dst, 4-bit src, label/disp
15. BrNE (Branch If Not Equal, unsigned) - group 0100 minor 01: 2-bit size, 4-bit dst, 4-bit src, label/disp
16. BrLT (Branch Less Than, unsigned) - group 0100 minor 10: 2-bit size, 4-bit dst, 4-bit src, label/disp
17. BrGE (Branch Greater Equal, unsigned) - group 0100 minor 11: 2-bit size, 4-bit dst, 4-bit src, label/disp

18. BrLTS (Branch Less Than Signed) - group 0101 minor 00: 2-bit size, 4-bit dst, 4-bit src, label/disp
19. BrGES (Branch Greater Equal Signed) - group 0101 minor 01: 2-bit size, 4-bit dst, 4-bit src, label/disp
20. BrZ (Branch if Zero) - group 0101 minor 10: 2-bit size, 2-bit EA, 4-bit reg, label/disp
21. BrNZ (Branch if Not Zero) - group 0101 minor 11: 2-bit size, 2-bit EA, 4-bit reg, label/disp

22. Ret (Return) - group 0110 minor 00: opcode+minor only; pops return address from stack into %pc and resumes execution
23. Mov (Move Direct) - group 0110 minor 01: 2-bit size, 4-bit dst reg, 4-bit src reg
24. Push - group 0110 minor 10: 2-bit size, implicit dst EA = pre-decrement indirect of %sp, implicit dst reg = %sp, 2-bit src EA (including immediate), 4-bit src reg
25. Pop - group 0110 minor 11: 2-bit size, implicit src EA = post-increment indirect of %sp, implicit src reg = %sp, 2-bit dst EA (register direct or indirect), 4-bit dst reg

26. Jmp (Jump Always) - group 0111 minor 0000: opcode+minor only; target/label follows in next word(s)
27. Call (Call address) - group 0111 minor 0001: opcode+minor only; target/label follows in next word(s)
28. Jmpi (Jump indirect) - group 0111 minor 0010: 2-bit size, 2-bit EA, 4-bit target reg/base
29. Calli (Call indirect) - group 0111 minor 0011: 2-bit size, 2-bit EA, 4-bit target reg/base
30. Shl (Shift left) - group 0111 minor 0100: 2-bit size, 2-bit EA (count source: reg direct or immediate via extension), 4-bit dst reg
31. Rol (Rotate left) - group 0111 minor 0101: 2-bit size, 2-bit EA (count source: reg direct or immediate via extension), 4-bit dst reg
32. Shr (Shift right) - group 0111 minor 0110: 2-bit size, 2-bit EA (count source: reg direct or immediate via extension), 4-bit dst reg
33. Ror (Rotate right) - group 0111 minor 0111: 2-bit size, 2-bit EA (count source: reg direct or immediate via extension), 4-bit dst reg

34. Trap (Trap) - group 1000 minor 00: opcode+minor only
35. Nop (No Operation) - group 1000 minor 01: opcode+minor only

36. Load - group 1001: 2-bit size, 2-bit EA (memory source), 4-bit dst reg, 4-bit base/index reg
37. Store - group 1010: 2-bit size, 2-bit EA (memory destination), 4-bit src reg, 4-bit base/index reg
38. Addi (Add immediate) - group 1011 minor 00: 2-bit size, 4-bit dst reg, 6-bit signed immediate (-32..31)
39. Subi (Sub immediate) - group 1011 minor 01: 2-bit size, 4-bit dst reg, 6-bit signed immediate (-32..31)
40. Muli (Mul immediate) - group 1011 minor 10: 2-bit size, 4-bit dst reg, 6-bit signed immediate (-32..31)
41. Remi (Remainder immediate, signed) - group 1011 minor 11: 2-bit size, 4-bit dst reg, 6-bit signed immediate (-32..31)
42. Div (Divide, signed) - group 1100 minor 00: 2-bit size, 4-bit dst reg, 4-bit src reg
43. Divu (Divide, unsigned) - group 1100 minor 01: 2-bit size, 4-bit dst reg, 4-bit src reg
44. Rem (Remainder, signed) - group 1100 minor 10: 2-bit size, 4-bit dst reg, 4-bit src reg
45. Remu (Remainder, unsigned) - group 1100 minor 11: 2-bit size, 4-bit dst reg, 4-bit src reg
46. Jmps (Jump short) - group 1101 minor 00: signed 10-bit offset; PC-relative jump (PC = PC + offset)
