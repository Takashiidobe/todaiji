use thiserror::Error;

use super::instruction::{Instruction, Opcode, Size};

#[derive(Debug, Error, PartialEq, Eq)]
pub enum PortableError {
    #[error("invalid opcode group {0}")]
    InvalidOpcode(u16),
    #[error("invalid size bits {0}")]
    InvalidSize(u8),
    #[error("invalid Effective Address bits {0}")]
    InvalidEA(u8),
    #[error("missing size for opcode")]
    MissingSize,
    #[error("encoding not implemented for this operand set")]
    Unsupported,
}

#[derive(Debug, Clone, Copy)]
struct OpcodeSpec {
    group: u8,
    minor: Option<u8>,
    is_sized: bool,
}

fn opcode_spec(op: Opcode) -> OpcodeSpec {
    match op {
        Opcode::Lea => OpcodeSpec {
            group: 0x0,
            minor: None,
            is_sized: true,
        },
        Opcode::Add => OpcodeSpec {
            group: 0x1,
            minor: Some(0),
            is_sized: true,
        },
        Opcode::Sub => OpcodeSpec {
            group: 0x1,
            minor: Some(1),
            is_sized: true,
        },
        Opcode::Mul => OpcodeSpec {
            group: 0x1,
            minor: Some(2),
            is_sized: true,
        },
        Opcode::Div => OpcodeSpec {
            group: 0x1,
            minor: Some(3),
            is_sized: true,
        },
        Opcode::And => OpcodeSpec {
            group: 0x2,
            minor: Some(0),
            is_sized: true,
        },
        Opcode::Or => OpcodeSpec {
            group: 0x2,
            minor: Some(1),
            is_sized: true,
        },
        Opcode::Xor => OpcodeSpec {
            group: 0x2,
            minor: Some(2),
            is_sized: true,
        },
        Opcode::Swap => OpcodeSpec {
            group: 0x2,
            minor: Some(3),
            is_sized: true,
        },
        Opcode::Not => OpcodeSpec {
            group: 0x3,
            minor: Some(0),
            is_sized: true,
        },
        Opcode::Neg => OpcodeSpec {
            group: 0x3,
            minor: Some(1),
            is_sized: true,
        },
        Opcode::Sxt => OpcodeSpec {
            group: 0x3,
            minor: Some(2),
            is_sized: true,
        },
        Opcode::Zxt => OpcodeSpec {
            group: 0x3,
            minor: Some(3),
            is_sized: true,
        },
        Opcode::BrEq => OpcodeSpec {
            group: 0x4,
            minor: Some(0),
            is_sized: true,
        },
        Opcode::BrNe => OpcodeSpec {
            group: 0x4,
            minor: Some(1),
            is_sized: true,
        },
        Opcode::BrLt => OpcodeSpec {
            group: 0x4,
            minor: Some(2),
            is_sized: true,
        },
        Opcode::BrGe => OpcodeSpec {
            group: 0x4,
            minor: Some(3),
            is_sized: true,
        },
        Opcode::BrLts => OpcodeSpec {
            group: 0x5,
            minor: Some(0),
            is_sized: true,
        },
        Opcode::BrGes => OpcodeSpec {
            group: 0x5,
            minor: Some(1),
            is_sized: true,
        },
        Opcode::BrZ => OpcodeSpec {
            group: 0x5,
            minor: Some(2),
            is_sized: true,
        },
        Opcode::BrNz => OpcodeSpec {
            group: 0x5,
            minor: Some(3),
            is_sized: true,
        },
        Opcode::Ret => OpcodeSpec {
            group: 0x6,
            minor: Some(0),
            is_sized: false,
        },
        Opcode::Mov => OpcodeSpec {
            group: 0x6,
            minor: Some(1),
            is_sized: true,
        },
        Opcode::Push => OpcodeSpec {
            group: 0x6,
            minor: Some(2),
            is_sized: true,
        },
        Opcode::Pop => OpcodeSpec {
            group: 0x6,
            minor: Some(3),
            is_sized: true,
        },
        Opcode::Jmp => OpcodeSpec {
            group: 0x7,
            minor: Some(0),
            is_sized: false,
        },
        Opcode::Jmps => OpcodeSpec {
            group: 0xD,
            minor: Some(0),
            is_sized: false,
        },
        Opcode::Call => OpcodeSpec {
            group: 0x7,
            minor: Some(1),
            is_sized: false,
        },
        Opcode::Jmpi => OpcodeSpec {
            group: 0x7,
            minor: Some(2),
            is_sized: true,
        },
        Opcode::Calli => OpcodeSpec {
            group: 0x7,
            minor: Some(3),
            is_sized: true,
        },
        Opcode::Shl => OpcodeSpec {
            group: 0x7,
            minor: Some(4),
            is_sized: true,
        },
        Opcode::Rol => OpcodeSpec {
            group: 0x7,
            minor: Some(5),
            is_sized: true,
        },
        Opcode::Shr => OpcodeSpec {
            group: 0x7,
            minor: Some(6),
            is_sized: true,
        },
        Opcode::Ror => OpcodeSpec {
            group: 0x7,
            minor: Some(7),
            is_sized: true,
        },
        Opcode::Trap => OpcodeSpec {
            group: 0x8,
            minor: Some(0),
            is_sized: false,
        },
        Opcode::Nop => OpcodeSpec {
            group: 0x8,
            minor: Some(1),
            is_sized: false,
        },
        Opcode::Load => OpcodeSpec {
            group: 0x9,
            minor: None,
            is_sized: true,
        },
        Opcode::Store => OpcodeSpec {
            group: 0xA,
            minor: None,
            is_sized: true,
        },
        Opcode::Addi => OpcodeSpec {
            group: 0xB,
            minor: Some(0),
            is_sized: true,
        },
        Opcode::Subi => OpcodeSpec {
            group: 0xB,
            minor: Some(1),
            is_sized: true,
        },
        Opcode::Muli => OpcodeSpec {
            group: 0xB,
            minor: Some(2),
            is_sized: true,
        },
        Opcode::Remi => OpcodeSpec {
            group: 0xB,
            minor: Some(3),
            is_sized: true,
        },
        Opcode::Divu => OpcodeSpec {
            group: 0xC,
            minor: Some(1),
            is_sized: true,
        },
        Opcode::Rem => OpcodeSpec {
            group: 0xC,
            minor: Some(2),
            is_sized: true,
        },
        Opcode::Remu => OpcodeSpec {
            group: 0xC,
            minor: Some(3),
            is_sized: true,
        },
        Opcode::Reserved => OpcodeSpec {
            group: 0xF,
            minor: None,
            is_sized: false,
        },
    }
}

fn opcode_from_spec(group: u8, minor: u8) -> Result<(Opcode, bool), PortableError> {
    let op = match group {
        0x0 => Opcode::Lea,
        0x1 => match minor {
            0 => Opcode::Add,
            1 => Opcode::Sub,
            2 => Opcode::Mul,
            3 => Opcode::Div,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0x2 => match minor {
            0 => Opcode::And,
            1 => Opcode::Or,
            2 => Opcode::Xor,
            3 => Opcode::Swap,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0x3 => match minor {
            0 => Opcode::Not,
            1 => Opcode::Neg,
            2 => Opcode::Sxt,
            3 => Opcode::Zxt,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0x4 => match minor {
            0 => Opcode::BrEq,
            1 => Opcode::BrNe,
            2 => Opcode::BrLt,
            3 => Opcode::BrGe,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0x5 => match minor {
            0 => Opcode::BrLts,
            1 => Opcode::BrGes,
            2 => Opcode::BrZ,
            3 => Opcode::BrNz,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0x6 => match minor {
            0 => Opcode::Ret,
            1 => Opcode::Mov,
            2 => Opcode::Push,
            3 => Opcode::Pop,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0x7 => match minor {
            0 => Opcode::Jmp,
            1 => Opcode::Call,
            2 => Opcode::Jmpi,
            3 => Opcode::Calli,
            4 => Opcode::Shl,
            5 => Opcode::Rol,
            6 => Opcode::Shr,
            7 => Opcode::Ror,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0x8 => match minor {
            0 => Opcode::Trap,
            1 => Opcode::Nop,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0x9 => Opcode::Load,
        0xA => Opcode::Store,
        0xB => match minor {
            0 => Opcode::Addi,
            1 => Opcode::Subi,
            2 => Opcode::Muli,
            3 => Opcode::Remi,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0xC => match minor {
            0 => Opcode::Div,
            1 => Opcode::Divu,
            2 => Opcode::Rem,
            3 => Opcode::Remu,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0xD => match minor {
            0 => Opcode::Jmps,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        _ => return Err(PortableError::InvalidOpcode(group as u16)),
    };
    Ok((op, opcode_spec(op).is_sized))
}

/// Pack opcode/group/minor and optional size into 16 bits. Operands/EAs are not encoded yet.
pub fn encode(inst: &Instruction) -> Result<Vec<u16>, PortableError> {
    let spec = opcode_spec(inst.opcode);
    let mut word = (spec.group as u16) << 12;

    // Encode minor: group 0x7 has 4-bit minor at bits 11-8, others have 2-bit at bits 11-10
    if let Some(min) = spec.minor {
        let shift = if spec.group == 0x7 { 8 } else { 10 };
        word |= (min as u16) << shift;
    }

    if spec.is_sized {
        let sz = inst.size.ok_or(PortableError::MissingSize)?;
        word |= (sz.to_bits() as u16) << 6;
    }

    // Special encoding for Jmps: encode 10-bit signed offset
    if inst.opcode == super::instruction::Opcode::Jmps {
        use super::instruction::Operand;
        let offset = match inst.dest.as_ref().or(inst.src.as_ref()) {
            Some(Operand::Imm(val)) => *val,
            _ => return Err(PortableError::Unsupported),
        };
        // Encode as 10-bit signed value
        word |= (offset as u16) & 0x3FF;
    }

    Ok(vec![word])
}

pub fn decode(words: &[u16]) -> Result<(Instruction, usize), PortableError> {
    if words.is_empty() {
        return Err(PortableError::Unsupported);
    }
    let word = words[0];
    let group = ((word >> 12) & 0xF) as u8;

    // Extract minor based on group: group 0x7 has 4-bit minor, others have 2-bit
    let minor = if group == 0x7 {
        ((word >> 8) & 0xF) as u8  // 4-bit minor (bits 11-8)
    } else {
        ((word >> 10) & 0x3) as u8  // 2-bit minor (bits 11-10)
    };

    let size_bits = (word >> 6) & 0b11;
    let (opcode, is_sized) = opcode_from_spec(group, minor)?;
    let size = if is_sized {
        Some(Size::from_bits(size_bits as u8)?)
    } else {
        None
    };

    // Special decoding for Jmps: extract 10-bit signed offset
    let (dest, src) = if opcode == Opcode::Jmps {
        use super::instruction::Operand;
        // Extract lower 10 bits and sign-extend
        let offset_bits = word & 0x3FF;
        let offset = if offset_bits & 0x200 != 0 {
            // Negative: sign extend from 10 bits to 32 bits
            (offset_bits | 0xFC00) as i16 as i32
        } else {
            offset_bits as i32
        };
        (Some(Operand::Imm(offset)), None)
    } else {
        (None, None)
    };

    let inst = Instruction {
        opcode,
        size,
        dest,
        src,
    };
    Ok((inst, 1))
}
