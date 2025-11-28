# Binary Format (.tji) - Implementation Status

## Overview

The Portable ISA supports both textual assembly (.asm) and binary bytecode (.tji) formats. This document describes the current implementation status and what remains to be completed.

## Current Implementation

### ✅ Infrastructure Complete

The following infrastructure is fully implemented:

1. **Binary file loading** (`decode_program`)
   - Reads `.tji` files as bytes
   - Converts little-endian bytes to 16-bit words
   - Decodes instruction opcodes, groups, and minors

2. **Binary file writing** (`encode_program`)
   - Encodes instructions to 16-bit words
   - Writes as little-endian bytes
   - Creates `.tji` files

3. **File type detection** (main.rs)
   - `.tji` extension → binary bytecode
   - `.asm/.s/.S` extension → textual assembly

4. **Opcode encoding/decoding** (decode.rs)
   - Group and minor extraction (with 2-bit vs 4-bit minor support)
   - Size suffix encoding
   - Special case: `jmps` with 10-bit relative offset

5. **Tests**
   - Binary round-trip tests for simple instructions
   - Jmps binary encoding tests

## ⚠️ Current Limitations

### Incomplete Operand Encoding

The current implementation **only encodes**:
- Opcode (group + minor)
- Size suffix
- Jmps offset (special case)

The current implementation **does NOT encode**:
- Register operands (e.g., `%r0`, `%r1`)
- Immediate values (e.g., `$42`)
- Memory addressing modes (e.g., `8(%sp)`)
- Effective addresses

### Why This Matters

When you compile a program like:
```asm
mov.l %r0, $10
```

The binary file will contain:
- ✅ Opcode for `mov`
- ✅ Size `.l` (long/32-bit)
- ❌ Destination register `%r0`
- ❌ Source immediate `$10`

When decoded and executed, the CPU will fail with `MissingOperand` because the instruction doesn't know which register or what value to use.

## What Works Now

### Fully Functional: Textual Assembly

The textual assembly format works perfectly:
```bash
cargo run -- examples/fib.asm    # ✅ Works
cargo run -- examples/jmps_demo.asm  # ✅ Works
```

### Partially Functional: Binary Format

Only the infrastructure is in place. To make binary execution work, operand encoding must be completed.

## What Needs to Be Implemented

To fully support binary execution, the `encode()` and `decode()` functions need to pack/unpack operands according to the ISA specification.

### Operand Encoding Needed

Each instruction group has a specific body format. Examples:

#### Group 0x1 (ALU reg-reg)
Format: `2-bit size + 4-bit dst reg + 4-bit src reg`
```rust
// In encode():
if matches!(inst.opcode, Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div) {
    let dst_reg = extract_reg(inst.dest)?;
    let src_reg = extract_reg(inst.src)?;
    word |= (dst_reg.to_u8() as u16) << 4;
    word |= (src_reg.to_u8() as u16);
}
```

#### Group 0xB (ALU immediate)
Format: `2-bit size + 4-bit dst reg + 6-bit signed immediate`
```rust
// In encode():
if matches!(inst.opcode, Opcode::Addi | Opcode::Subi | Opcode::Muli | Opcode::Remi) {
    let dst_reg = extract_reg(inst.dest)?;
    let imm = extract_imm_6bit(inst.src)?;
    word |= (dst_reg.to_u8() as u16) << 6;
    word |= (imm as u16) & 0x3F;
}
```

#### Group 0x6 (Mov)
Format: `2-bit size + 4-bit dst reg + 4-bit src reg`

### Decoding Needed

Corresponding decode logic must extract operands:
```rust
// In decode():
match group {
    0x1 => {
        // Extract registers from bits
        let dst_bits = ((word >> 4) & 0xF) as u8;
        let src_bits = (word & 0xF) as u8;
        dest = Some(Operand::Reg(Reg::from_u8(dst_bits)?));
        src = Some(Operand::Reg(Reg::from_u8(src_bits)?));
    }
    // ... other groups
}
```

## Recommended Implementation Order

1. **Implement common patterns first**
   - Register-register operations (Groups 0x1, 0x2, 0x6)
   - Immediate operations (Group 0xB)
   - These cover most instructions

2. **Add memory operations**
   - Load/Store (Groups 0x9, 0xA)
   - Stack operations (Push/Pop)

3. **Complete remaining groups**
   - Branches
   - Shifts/rotates
   - Type conversions

4. **Handle extension words**
   - Some instructions may need additional 16-bit words for full addresses or large immediates
   - Update `decode()` to return variable word consumption

## Testing Strategy

For each implemented encoding:

1. **Unit test round-trip**
   ```rust
   let inst = create_instruction();
   let bytes = encode_program(&[inst])?;
   let decoded = decode_program(&bytes)?;
   assert_eq!(decoded[0], inst);
   ```

2. **Integration test**
   ```rust
   let asm = "mov.l %r0, $42";
   let program = parse_program(asm)?;
   let bytes = encode_program(&program)?;
   let decoded = decode_program(&bytes)?;

   let mut cpu = Cpu::new(1024);
   cpu.run(&decoded)?;
   assert_eq!(cpu.regs[0], 42);
   ```

3. **File round-trip**
   - Compile .asm → .tji
   - Run .tji
   - Verify same results as .asm

## Current Workaround

Until operand encoding is complete, use textual assembly format:
```bash
# This works:
cargo run -- my_program.asm

# This needs operand encoding:
cargo run -- my_program.tji  # Will fail with MissingOperand
```

## Binary Format Specification

### File Structure

```
[16-bit word 1 (LE)]  # Instruction 0
[16-bit word 2 (LE)]  # Instruction 1
...
[16-bit word N (LE)]  # Instruction N-1
```

Each word is stored in little-endian format:
- Byte 0: Low byte of word
- Byte 1: High byte of word

### Word Format

```
Bits 15-12: Group (4 bits)
Bits 11-10: Minor (2 bits) for most groups
Bits 11-8:  Minor (4 bits) for group 0x7
Bits 9-0:   Body (varies by instruction)
```

## See Also

- `docs/portable.md` - Complete ISA specification with all instruction formats
- `src/portable/decode.rs` - Encoding/decoding implementation
- `src/portable/instruction.rs` - Instruction structure and parsing
