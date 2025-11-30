use thiserror::Error;

use super::instruction::{
    DataSegment, EffectiveAddress, ImmediateValue, Instruction, Opcode, Operand, Program, Reg, Size,
};

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
    #[error("immediate value does not fit in required type")]
    ImmValueError,
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
        Opcode::Divmodu => OpcodeSpec {
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
        Opcode::BrLts => OpcodeSpec {
            group: 0x4,
            minor: Some(3),
            is_sized: true,
        },
        Opcode::Cmpeq => OpcodeSpec {
            group: 0x8,
            minor: Some(0),
            is_sized: true,
        },
        Opcode::Cmpne => OpcodeSpec {
            group: 0x8,
            minor: Some(1),
            is_sized: true,
        },
        Opcode::Cmpltu => OpcodeSpec {
            group: 0x8,
            minor: Some(2),
            is_sized: true,
        },
        Opcode::Cmplt => OpcodeSpec {
            group: 0x8,
            minor: Some(3),
            is_sized: true,
        },
        Opcode::BrZ => OpcodeSpec {
            group: 0x5,
            minor: Some(0),
            is_sized: true,
        },
        Opcode::BrNz => OpcodeSpec {
            group: 0x5,
            minor: Some(1),
            is_sized: true,
        },
        Opcode::Jmps => OpcodeSpec {
            group: 0x5,
            minor: Some(2),
            is_sized: false,
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
        Opcode::Fence => OpcodeSpec {
            group: 0x7,
            minor: Some(4),
            is_sized: false,
        },
        Opcode::Shl => OpcodeSpec {
            group: 0xC,
            minor: Some(1),
            is_sized: true,
        },
        Opcode::Shr => OpcodeSpec {
            group: 0xC,
            minor: Some(2),
            is_sized: true,
        },
        Opcode::Sar => OpcodeSpec {
            group: 0xC,
            minor: Some(3),
            is_sized: true,
        },
        Opcode::Trap => OpcodeSpec {
            group: 0x7,
            minor: Some(6),
            is_sized: false,
        },
        Opcode::Nop => OpcodeSpec {
            group: 0x7,
            minor: Some(7),
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
        Opcode::Movi => OpcodeSpec {
            group: 0xB,
            minor: Some(3),
            is_sized: true,
        },
        Opcode::Divmod => OpcodeSpec {
            group: 0xC,
            minor: Some(0),
            is_sized: true,
        },
        Opcode::Cas => OpcodeSpec {
            group: 0xD,
            minor: None,
            is_sized: true,
        },
        Opcode::Xchg => OpcodeSpec {
            group: 0xF,
            minor: None,
            is_sized: true,
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
            3 => Opcode::Divmodu,
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
            3 => Opcode::BrLts,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0x5 => match minor {
            0 => Opcode::BrZ,
            1 => Opcode::BrNz,
            2 => Opcode::Jmps,
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
            4 => Opcode::Fence,
            6 => Opcode::Trap,
            7 => Opcode::Nop,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0x8 => match minor {
            0 => Opcode::Cmpeq,
            1 => Opcode::Cmpne,
            2 => Opcode::Cmpltu,
            3 => Opcode::Cmplt,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0x9 => Opcode::Load,
        0xA => Opcode::Store,
        0xB => match minor {
            0 => Opcode::Addi,
            1 => Opcode::Subi,
            2 => Opcode::Muli,
            3 => Opcode::Movi,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0xC => match minor {
            0 => Opcode::Divmod,
            1 => Opcode::Shl,
            2 => Opcode::Shr,
            3 => Opcode::Sar,
            _ => return Err(PortableError::InvalidOpcode(group as u16)),
        },
        0xD => Opcode::Cas,
        0xE => return Err(PortableError::InvalidOpcode(group as u16)),
        0xF => Opcode::Xchg,
        _ => return Err(PortableError::InvalidOpcode(group as u16)),
    };
    Ok((op, opcode_spec(op).is_sized))
}

// Helper functions for operand extraction and encoding

fn extract_reg(op: &Option<Operand>) -> Result<u8, PortableError> {
    match op {
        Some(Operand::Reg(r)) => Ok(r.to_u8()),
        _ => Err(PortableError::Unsupported),
    }
}

fn extract_imm<T>(op: &Option<Operand>) -> Result<T, PortableError>
where
    T: TryFrom<i128>,
{
    match op {
        Some(Operand::Imm(val)) => {
            let v = val.as_i128();
            T::try_from(v).map_err(|_| PortableError::Unsupported)
        }
        _ => Err(PortableError::Unsupported),
    }
}

fn sign_extend(value: u64, bits: usize) -> i64 {
    if bits == 64 {
        value as i64
    } else {
        let shift = 64 - bits;
        ((value << shift) as i64) >> shift
    }
}

fn read_disp(words: &[u16], size: Size) -> Result<(i64, usize), PortableError> {
    let bytes = size.bytes();
    let needed = bytes.div_ceil(2);
    if words.len() < needed {
        return Err(PortableError::Unsupported);
    }
    let mut val = 0u64;
    for i in 0..needed {
        val |= (words[i] as u64) << (16 * i);
    }
    let disp = sign_extend(val, bytes * 8);
    Ok((disp, needed))
}

fn push_disp(
    disp: i64,
    size: Option<Size>,
    extension_words: &mut Vec<u16>,
) -> Result<(), PortableError> {
    let sz = size.ok_or(PortableError::MissingSize)?;
    let bytes = sz.bytes();
    let words_needed = bytes.div_ceil(2);
    let masked = sz.mask(disp as u64);
    for i in 0..words_needed {
        extension_words.push(((masked >> (16 * i)) & 0xFFFF) as u16);
    }
    Ok(())
}

fn extract_ea_info(
    op: &Option<Operand>,
    _size: Option<Size>,
) -> Result<(u8, u8, Option<i64>), PortableError> {
    match op {
        Some(Operand::Ea { reg, ea, disp }) => {
            let ea_bits = match ea {
                EffectiveAddress::RegDirect => 0b00,
                EffectiveAddress::RegIndirect => 0b01,
                EffectiveAddress::BaseDisp => 0b10,
                EffectiveAddress::Immediate => 0b11,
            };
            Ok((reg.to_u8(), ea_bits, *disp))
        }
        Some(Operand::Reg(r)) => {
            // Register direct mode (for Sxt/Zxt EA set)
            Ok((r.to_u8(), 0b00, None))
        }
        _ => Err(PortableError::Unsupported),
    }
}

/// Pack opcode/group/minor, size, and operands into 16 bits (and extension words if needed)
pub fn encode(inst: &Instruction) -> Result<Vec<u16>, PortableError> {
    let mut spec = opcode_spec(inst.opcode);

    // Special case: Jmp/Call with EA operands should encode as Jmpi/Calli
    if matches!(inst.opcode, Opcode::Jmp) {
        let target_is_ea = matches!(
            inst.dest.as_ref().or(inst.src.as_ref()),
            Some(Operand::Ea { .. })
        );
        if target_is_ea {
            spec.minor = Some(2); // Jmpi
            // Note: Jmpi is technically sized, but we'll default to Word if not specified
        }
    }

    if matches!(inst.opcode, Opcode::Call) {
        let target_is_ea = matches!(
            inst.dest.as_ref().or(inst.src.as_ref()),
            Some(Operand::Ea { .. })
        );
        if target_is_ea {
            spec.minor = Some(3); // Calli
            // Note: Calli is technically sized, but we'll default to Word if not specified
        }
    }

    let mut word = (spec.group as u16) << 12;

    // Encode minor: group 0x7 has 4-bit minor at bits 11-8, others have 2-bit at bits 11-10
    if let Some(min) = spec.minor {
        let shift = if spec.group == 0x7 { 8 } else { 10 };
        word |= (min as u16) << shift;
    }

    // Encode size if instruction is sized
    // Special case: Group 0x7 minors 2+ are sized even if base opcode isn't
    let needs_size = spec.is_sized;

    if needs_size {
        // Default to Word size if not specified (for Jmpi/Calli without explicit size)
        let sz = inst.size.unwrap_or(Size::Word);
        word |= (sz.to_bits() as u16) << 6;
    }

    // Track extension words for immediates/displacements
    let mut extension_words = Vec::new();

    // Encode operands based on instruction group/opcode
    match spec.group {
        // Group 0x0: LEA - 2-bit size, 2-bit src EA, 4-bit dst reg, 4-bit src reg
        0x0 => {
            let dst_reg = extract_reg(&inst.dest)?;
            let (src_reg, src_ea, disp) = extract_ea_info(&inst.src, inst.size)?;
            word |= (src_ea as u16) << 4;
            word |= (dst_reg as u16) << 2;
            word |= src_reg as u16;

            if let Some(d) = disp {
                push_disp(d, inst.size, &mut extension_words)?;
            }
        }

        // Group 0x1: ALU reg-reg - 2-bit size, 4-bit dst reg, 4-bit src reg
        0x1 => {
            let dst_reg = extract_reg(&inst.dest)?;
            let src_reg = extract_reg(&inst.src)?;
            word |= dst_reg as u16;
            extension_words.push(src_reg as u16);
        }

        // Group 0x2: Logic reg-reg - 2-bit size, 4-bit dst reg, 4-bit src reg
        0x2 => {
            let dst_reg = extract_reg(&inst.dest)?;
            let src_reg = extract_reg(&inst.src)?;
            word |= dst_reg as u16;
            extension_words.push(src_reg as u16);
        }

        // Group 0x3: Unary ops - 2-bit size, 2-bit EA, 4-bit dst reg
        0x3 => {
            let (dst_reg, ea, _) = extract_ea_info(&inst.dest, inst.size)?;
            word |= (ea as u16) << 4;
            word |= dst_reg as u16;
        }

        // Group 0x4: Unsigned branches - 2-bit size, 4-bit dst reg, 4-bit src reg
        // Note: Branch target is in a resolved immediate (src/target operand)
        0x4 => {
            let dst_reg = extract_reg(&inst.dest)?;
            let src_reg = extract_reg(&inst.src)?;
            let target: u32 = extract_imm(&inst.target).or_else(|_| extract_imm(&inst.src))?;
            word |= (dst_reg as u16) << 4;
            word |= src_reg as u16;
            extension_words.push(target as u16);
            extension_words.push((target >> 16) as u16);
        }

        // Group 0x5: Signed branches/control
        0x5 => {
            match spec.minor {
                Some(0) | Some(1) => {
                    // BrZ, BrNZ - 2-bit size, 2-bit EA, 4-bit reg
                    let (reg, ea, disp) = extract_ea_info(&inst.dest, inst.size)?;
                    let target: u32 =
                        extract_imm(&inst.target).or_else(|_| extract_imm(&inst.src))?;
                    word |= (ea as u16) << 4;
                    word |= reg as u16;
                    if let Some(d) = disp {
                        push_disp(d, inst.size, &mut extension_words)?;
                    }
                    extension_words.push(target as u16);
                    extension_words.push((target >> 16) as u16);
                }
                Some(2) => {
                    // Jmps - 10-bit signed offset (-512..511)
                    let op = inst.dest.clone().or(inst.src.clone());
                    let offset: i16 = extract_imm(&op)?;
                    word |= (offset as u16) & 0x3FF;
                }
                _ => {}
            }
        }

        // Group 0x6: Stack/control/move
        0x6 => {
            match spec.minor {
                Some(0) => { /* Ret - no operands */ }
                Some(1) => {
                    // Mov - 2-bit size, 4-bit dst reg, 4-bit src reg
                    let dst_reg = extract_reg(&inst.dest)?;
                    let src_reg = extract_reg(&inst.src)?;
                    word |= dst_reg as u16;
                    extension_words.push(src_reg as u16);
                }
                Some(2) => {
                    // Push - 2-bit size, 2-bit src EA, 4-bit src reg
                    let op = inst.src.clone().or(inst.dest.clone());
                    let (src_reg, src_ea, disp) = extract_ea_info(&op, inst.size)?;
                    word |= (src_ea as u16) << 4;
                    word |= src_reg as u16;
                    if let Some(d) = disp {
                        push_disp(d, inst.size, &mut extension_words)?;
                    }
                }
                Some(3) => {
                    // Pop - 2-bit size, 2-bit dst EA, 4-bit dst reg
                    let op = inst.dest.clone().or(inst.src.clone());
                    let (dst_reg, dst_ea, disp) = extract_ea_info(&op, inst.size)?;
                    word |= (dst_ea as u16) << 4;
                    word |= dst_reg as u16;
                    if let Some(d) = disp {
                        push_disp(d, inst.size, &mut extension_words)?;
                    }
                }
                _ => {}
            }
        }

        // Group 0x7: Jumps/shifts/rotates
        0x7 => {
            match spec.minor {
                Some(0) | Some(1) => {
                    // Jmp, Call - target stored in extension words (absolute instruction index)
                    let op = inst.dest.clone().or(inst.src.clone());
                    let target: u32 = extract_imm(&op)?;
                    extension_words.push(target as u16);
                    extension_words.push((target >> 16) as u16);
                }
                Some(2) | Some(3) => {
                    // Jmpi, Calli - 2-bit size, 2-bit EA, 4-bit target reg
                    let op = inst.dest.clone().or(inst.src.clone());
                    let (target_reg, ea, _) = extract_ea_info(&op, inst.size)?;
                    word |= (ea as u16) << 4;
                    word |= target_reg as u16;
                }
                Some(4) => {
                    // Fence - encode mode in size bits if provided
                    if let Some(mode) = inst.size {
                        word |= (mode.to_bits() as u16) << 6;
                    }
                }
                Some(6) | Some(7) => { /* Trap/Nop - no operands */ }
                _ => {}
            }
        }

        // Group 0x8: Compare-to-boolean reg-reg
        0x8 => {
            let dst_reg = extract_reg(&inst.dest)?;
            let src_reg = extract_reg(&inst.src)?;
            word |= dst_reg as u16;
            extension_words.push(src_reg as u16);
        }

        // Group 0x9: Load - 2-bit size, 2-bit EA, 4-bit dst reg, 4-bit base reg
        0x9 => {
            let size = inst.size.ok_or(PortableError::MissingSize)?;
            let dst_reg = extract_reg(&inst.dest)?;

            if let Some(Operand::Imm(val)) = inst.src.as_ref() {
                // Immediate load: size determines how many bytes we emit
                word |= (dst_reg as u16) << 8;
                word |= (0b11u16) << 4; // EA = immediate

                let imm = size.mask(val.as_u64());
                let bytes = size.bytes();
                let words_needed = bytes.div_ceil(2);
                for i in 0..words_needed {
                    extension_words.push(((imm >> (16 * i)) & 0xFFFF) as u16);
                }
            } else {
                let (base_reg, ea, disp) = extract_ea_info(&inst.src, inst.size)?;
                word |= (dst_reg as u16) << 8;
                word |= (ea as u16) << 4;
                word |= base_reg as u16;

                if let Some(d) = disp {
                    push_disp(d, inst.size, &mut extension_words)?;
                }
            }
        }

        // Group 0xA: Store - 2-bit size, 2-bit EA, 4-bit src reg, 4-bit base reg
        0xA => {
            let src_reg = extract_reg(&inst.dest)?;
            let (base_reg, ea, disp) = extract_ea_info(&inst.src, inst.size)?;
            word |= (src_reg as u16) << 8;
            word |= (ea as u16) << 4;
            word |= base_reg as u16;

            if let Some(d) = disp {
                push_disp(d, inst.size, &mut extension_words)?;
            }
        }

        // Group 0xB: ALU imm/Movi - 4-bit dst reg, 6-bit unsigned immediate (0-63)
        0xB => {
            let dst_reg = extract_reg(&inst.dest)?;
            let imm: u8 = extract_imm(&inst.src)?;
            if imm > 63 {
                return Err(PortableError::ImmValueError);
            }
            word |= dst_reg as u16;
            word |= (imm as u16) << 4;
        }

        // Group 0xC: Divmod/Shift reg-reg - 2-bit size, 4-bit dst reg, 4-bit src reg
        0xC => {
            let dst_reg = extract_reg(&inst.dest)?;
            let src_reg = extract_reg(&inst.src)?;
            word |= dst_reg as u16;
            extension_words.push(src_reg as u16);
        }

        // Group 0xD: CAS - 2-bit size, 2-bit EA, 4-bit base/index, 4-bit expect/old reg; extension word carries new_reg
        0xD => {
            let expect_reg = extract_reg(&inst.dest)?;
            let new_reg = extract_reg(&inst.src)?;
            let (base_reg, ea, disp) = extract_ea_info(&inst.target, inst.size)?;
            word |= (ea as u16) << 8;
            word |= (base_reg as u16) << 4;
            word |= expect_reg as u16;
            extension_words.push(new_reg as u16);
            if let Some(d) = disp {
                push_disp(d, inst.size, &mut extension_words)?;
            }
        }

        // Group 0xF: Xchg - 2-bit size, 2-bit EA, 4-bit base/index reg, 4-bit swap reg
        0xF => {
            let swap_reg = extract_reg(&inst.dest)?;
            let (base_reg, ea, disp) = extract_ea_info(&inst.src, inst.size)?;
            word |= (ea as u16) << 8;
            word |= (base_reg as u16) << 4;
            word |= swap_reg as u16;
            if let Some(d) = disp {
                push_disp(d, inst.size, &mut extension_words)?;
            }
        }

        _ => return Err(PortableError::InvalidOpcode(spec.group as u16)),
    }

    let mut result = vec![word];
    result.extend(extension_words);
    Ok(result)
}

pub fn decode(words: &[u16]) -> Result<(Instruction, usize), PortableError> {
    if words.is_empty() {
        return Err(PortableError::Unsupported);
    }
    let word = words[0];
    let group = ((word >> 12) & 0xF) as u8;
    let mut words_consumed = 1; // Track how many words this instruction uses

    // Extract minor based on group: group 0x7 has 4-bit minor, others have 2-bit
    let minor = if group == 0x7 {
        ((word >> 8) & 0xF) as u8 // 4-bit minor (bits 11-8)
    } else {
        ((word >> 10) & 0x3) as u8 // 2-bit minor (bits 11-10)
    };

    let size_bits = (word >> 6) & 0b11;
    let (opcode, is_sized) = opcode_from_spec(group, minor)?;
    let size = if is_sized {
        Some(Size::from_bits(size_bits as u8)?)
    } else {
        None
    };

    // Decode operands based on instruction group/opcode
    let mut target: Option<Operand> = None;

    let (dest, src) = match group {
        // Group 0x0: LEA - 2-bit src EA, 4-bit dst reg, 4-bit src reg
        0x0 => {
            let src_ea_bits = ((word >> 4) & 0x3) as u8;
            let dst_reg_bits = ((word >> 2) & 0xF) as u8;
            let src_reg_bits = (word & 0xF) as u8;

            let dst_reg = Reg::from_u8(dst_reg_bits).ok_or(PortableError::Unsupported)?;
            let src_reg = Reg::from_u8(src_reg_bits).ok_or(PortableError::Unsupported)?;
            let src_ea = match src_ea_bits {
                0b00 => EffectiveAddress::RegDirect,
                0b01 => EffectiveAddress::RegIndirect,
                0b10 => EffectiveAddress::BaseDisp,
                0b11 => EffectiveAddress::Immediate,
                _ => return Err(PortableError::InvalidEA(src_ea_bits)),
            };

            let mut disp = None;
            if matches!(src_ea, EffectiveAddress::BaseDisp) {
                let sz = size.ok_or(PortableError::MissingSize)?;
                let (d, used) = read_disp(&words[1..], sz)?;
                words_consumed = 1 + used;
                disp = Some(d);
            }

            (
                Some(Operand::Reg(dst_reg)),
                Some(Operand::Ea {
                    reg: src_reg,
                    ea: src_ea,
                    disp,
                }),
            )
        }

        // Groups 0x1, 0x2, 0xC: reg-reg ops - 4-bit dst reg, src reg in extension word
        0x1 | 0x2 | 0xC => {
            if words.len() < 2 {
                return Err(PortableError::Unsupported);
            }
            let dst_reg_bits = (word & 0xF) as u8;
            let src_reg_bits = (words[1] & 0xF) as u8;
            let dst_reg = Reg::from_u8(dst_reg_bits).ok_or(PortableError::Unsupported)?;
            let src_reg = Reg::from_u8(src_reg_bits).ok_or(PortableError::Unsupported)?;
            words_consumed = 2;
            (Some(Operand::Reg(dst_reg)), Some(Operand::Reg(src_reg)))
        }

        // Group 0x3: Unary ops - 2-bit EA, 4-bit dst reg
        0x3 => {
            let ea_bits = ((word >> 4) & 0x3) as u8;
            let dst_reg_bits = (word & 0xF) as u8;
            let dst_reg = Reg::from_u8(dst_reg_bits).ok_or(PortableError::Unsupported)?;

            if matches!(opcode, Opcode::Not | Opcode::Neg) {
                // Register direct only
                (Some(Operand::Reg(dst_reg)), None)
            } else {
                // Sxt/Zxt can use EA modes
                let ea = match ea_bits {
                    0b00 => EffectiveAddress::RegDirect,
                    0b01 => EffectiveAddress::RegIndirect,
                    0b10 => EffectiveAddress::BaseDisp,
                    _ => return Err(PortableError::InvalidEA(ea_bits)),
                };
                (
                    Some(Operand::Reg(dst_reg)),
                    Some(Operand::Ea {
                        reg: dst_reg,
                        ea,
                        disp: None,
                    }),
                )
            }
        }

        // Group 0x4: Unsigned branches - 4-bit dst reg, 4-bit src reg, target in extensions
        0x4 => {
            if words.len() < 3 {
                return Err(PortableError::Unsupported);
            }
            let dst_reg_bits = ((word >> 4) & 0xF) as u8;
            let src_reg_bits = (word & 0xF) as u8;
            let target_low = words[1] as u32;
            let target_high = words[2] as u32;
            let target_bits = ((target_high << 16) | target_low) as i32;
            let dst_reg = Reg::from_u8(dst_reg_bits).ok_or(PortableError::Unsupported)?;
            let src_reg = Reg::from_u8(src_reg_bits).ok_or(PortableError::Unsupported)?;
            words_consumed = 3;
            target = Some(Operand::Imm(ImmediateValue::Long(target_bits)));
            (Some(Operand::Reg(dst_reg)), Some(Operand::Reg(src_reg)))
        }

        // Group 0x5: Signed branches/control
        0x5 => {
            match minor {
                0 | 1 => {
                    // BrZ, BrNZ
                    let ea_bits = ((word >> 4) & 0x3) as u8;
                    let reg_bits = (word & 0xF) as u8;
                    let reg = Reg::from_u8(reg_bits).ok_or(PortableError::Unsupported)?;

                    let mut disp = None;
                    let mut idx = 1;
                    if ea_bits == 0b10 {
                        let sz = size.ok_or(PortableError::MissingSize)?;
                        let (d, used) = read_disp(&words[idx..], sz)?;
                        disp = Some(d);
                        idx += used;
                    }
                    if words.len() < idx + 2 {
                        return Err(PortableError::Unsupported);
                    }
                    let target_low = words[idx] as u32;
                    let target_high = words[idx + 1] as u32;
                    let target_bits = ((target_high << 16) | target_low) as i32;
                    words_consumed = idx + 2;
                    target = Some(Operand::Imm(ImmediateValue::Long(target_bits)));

                    let ea = match ea_bits {
                        0b00 => EffectiveAddress::RegDirect,
                        0b01 => EffectiveAddress::RegIndirect,
                        0b10 => EffectiveAddress::BaseDisp,
                        _ => return Err(PortableError::InvalidEA(ea_bits)),
                    };

                    if matches!(ea, EffectiveAddress::RegDirect) {
                        (Some(Operand::Reg(reg)), target.clone())
                    } else {
                        (Some(Operand::Ea { reg, ea, disp }), target.clone())
                    }
                }
                2 => {
                    // Jmps short
                    let offset_bits = word & 0x3FF;
                    let offset = if offset_bits & 0x200 != 0 {
                        (offset_bits | 0xFC00) as i16
                    } else {
                        offset_bits as i16
                    };
                    (Some(Operand::Imm(ImmediateValue::Short(offset))), None)
                }
                _ => (None, None),
            }
        }

        // Group 0x6: Stack/control/move
        0x6 => {
            match minor {
                0 => (None, None), // Ret
                1 => {
                    // Mov
                    if words.len() < 2 {
                        return Err(PortableError::Unsupported);
                    }
                    let dst_reg_bits = (word & 0xF) as u8;
                    let src_reg_bits = (words[1] & 0xF) as u8;
                    let dst_reg = Reg::from_u8(dst_reg_bits).ok_or(PortableError::Unsupported)?;
                    let src_reg = Reg::from_u8(src_reg_bits).ok_or(PortableError::Unsupported)?;
                    words_consumed = 2;
                    (Some(Operand::Reg(dst_reg)), Some(Operand::Reg(src_reg)))
                }
                2 => {
                    // Push
                    let ea_bits = ((word >> 4) & 0x3) as u8;
                    let reg_bits = (word & 0xF) as u8;
                    let reg = Reg::from_u8(reg_bits).ok_or(PortableError::Unsupported)?;

                    let mut disp = None;
                    words_consumed = 1;
                    let ea = match ea_bits {
                        0b00 => EffectiveAddress::RegDirect,
                        0b01 => EffectiveAddress::RegIndirect,
                        0b10 => {
                            let sz = size.ok_or(PortableError::MissingSize)?;
                            let (d, used) = read_disp(&words[1..], sz)?;
                            words_consumed = 1 + used;
                            disp = Some(d);
                            EffectiveAddress::BaseDisp
                        }
                        0b11 => EffectiveAddress::Immediate,
                        _ => return Err(PortableError::InvalidEA(ea_bits)),
                    };
                    if matches!(ea, EffectiveAddress::RegDirect) {
                        (None, Some(Operand::Reg(reg)))
                    } else {
                        (None, Some(Operand::Ea { reg, ea, disp }))
                    }
                }
                3 => {
                    // Pop
                    let ea_bits = ((word >> 4) & 0x3) as u8;
                    let reg_bits = (word & 0xF) as u8;
                    let reg = Reg::from_u8(reg_bits).ok_or(PortableError::Unsupported)?;

                    let mut disp = None;
                    words_consumed = 1;
                    let ea = match ea_bits {
                        0b00 => EffectiveAddress::RegDirect,
                        0b01 => EffectiveAddress::RegIndirect,
                        0b10 => {
                            let sz = size.ok_or(PortableError::MissingSize)?;
                            let (d, used) = read_disp(&words[1..], sz)?;
                            words_consumed = 1 + used;
                            disp = Some(d);
                            EffectiveAddress::BaseDisp
                        }
                        _ => return Err(PortableError::InvalidEA(ea_bits)),
                    };
                    if matches!(ea, EffectiveAddress::RegDirect) {
                        (Some(Operand::Reg(reg)), None)
                    } else {
                        (Some(Operand::Ea { reg, ea, disp }), None)
                    }
                }
                _ => (None, None),
            }
        }

        // Group 0x7: Jumps/shifts/rotates
        0x7 => {
            match minor {
                0 | 1 => {
                    // Jmp, Call use extension words for target
                    if words.len() < 3 {
                        return Err(PortableError::Unsupported);
                    }
                    let target_low = words[1] as u32;
                    let target_high = words[2] as u32;
                    let target = ((target_high << 16) | target_low) as i32;
                    words_consumed = 3;
                    (Some(Operand::Imm(ImmediateValue::Long(target))), None)
                }
                2 | 3 => {
                    // Jmpi, Calli
                    let ea_bits = ((word >> 4) & 0x3) as u8;
                    let reg_bits = (word & 0xF) as u8;
                    let reg = Reg::from_u8(reg_bits).ok_or(PortableError::Unsupported)?;

                    let mut disp = None;
                    words_consumed = 1;
                    let ea = match ea_bits {
                        0b00 => EffectiveAddress::RegDirect,
                        0b01 => EffectiveAddress::RegIndirect,
                        0b10 => {
                            let sz = size.ok_or(PortableError::MissingSize)?;
                            let (d, used) = read_disp(&words[1..], sz)?;
                            words_consumed = 1 + used;
                            disp = Some(d);
                            EffectiveAddress::BaseDisp
                        }
                        _ => return Err(PortableError::InvalidEA(ea_bits)),
                    };
                    if matches!(ea, EffectiveAddress::RegDirect) {
                        (Some(Operand::Reg(reg)), None)
                    } else {
                        (Some(Operand::Ea { reg, ea, disp }), None)
                    }
                }
                4 => (None, None),     // Fence (mode currently ignored)
                6 | 7 => (None, None), // Trap/Nop
                _ => (None, None),
            }
        }

        // Group 0x8: Compare-to-boolean reg-reg
        0x8 => {
            if words.len() < 2 {
                return Err(PortableError::Unsupported);
            }
            let dst_reg_bits = (word & 0xF) as u8;
            let src_reg_bits = (words[1] & 0xF) as u8;
            let dst_reg = Reg::from_u8(dst_reg_bits).ok_or(PortableError::Unsupported)?;
            let src_reg = Reg::from_u8(src_reg_bits).ok_or(PortableError::Unsupported)?;
            words_consumed = 2;
            (Some(Operand::Reg(dst_reg)), Some(Operand::Reg(src_reg)))
        }

        // Group 0x9: Load - 2-bit EA, 4-bit dst reg, 4-bit base reg
        0x9 => {
            let ea_bits = ((word >> 4) & 0x3) as u8;
            let dst_reg_bits = ((word >> 8) & 0xF) as u8;
            let base_reg_bits = (word & 0xF) as u8;

            let dst_reg = Reg::from_u8(dst_reg_bits).ok_or(PortableError::Unsupported)?;

            match ea_bits {
                0b11 => {
                    // Immediate mode - Load immediate value; size determines how many words to read
                    let sz = size.ok_or(PortableError::MissingSize)?;
                    let bytes = sz.bytes();
                    let words_needed = bytes.div_ceil(2);
                    if words.len() < 1 + words_needed {
                        return Err(PortableError::Unsupported);
                    }
                    let mut imm_val: u64 = 0;
                    for i in 0..words_needed {
                        imm_val |= (words[1 + i] as u64) << (16 * i);
                    }
                    words_consumed = 1 + words_needed;
                    let imm = match sz {
                        Size::Byte => ImmediateValue::UByte(imm_val as u8),
                        Size::Short => ImmediateValue::UShort(imm_val as u16),
                        Size::Long => ImmediateValue::ULong(imm_val as u32),
                        Size::Word => ImmediateValue::UWord(imm_val),
                    };
                    (Some(Operand::Reg(dst_reg)), Some(Operand::Imm(imm)))
                }
                _ => {
                    let base_reg = Reg::from_u8(base_reg_bits).ok_or(PortableError::Unsupported)?;
                    let ea = match ea_bits {
                        0b00 => EffectiveAddress::RegDirect,
                        0b01 => EffectiveAddress::RegIndirect,
                        0b10 => EffectiveAddress::BaseDisp,
                        _ => return Err(PortableError::InvalidEA(ea_bits)),
                    };

                    let mut disp = None;
                    words_consumed = 1;
                    if ea_bits == 0b10 {
                        let sz = size.ok_or(PortableError::MissingSize)?;
                        let (d, used) = read_disp(&words[1..], sz)?;
                        disp = Some(d);
                        words_consumed = 1 + used;
                    }

                    (
                        Some(Operand::Reg(dst_reg)),
                        Some(Operand::Ea {
                            reg: base_reg,
                            ea,
                            disp,
                        }),
                    )
                }
            }
        }

        // Group 0xA: Store - 2-bit EA, 4-bit src reg, 4-bit base reg
        0xA => {
            let ea_bits = ((word >> 4) & 0x3) as u8;
            let src_reg_bits = ((word >> 8) & 0xF) as u8;
            let base_reg_bits = (word & 0xF) as u8;

            let src_reg = Reg::from_u8(src_reg_bits).ok_or(PortableError::Unsupported)?;
            let base_reg = Reg::from_u8(base_reg_bits).ok_or(PortableError::Unsupported)?;
            let ea = match ea_bits {
                0b00 => EffectiveAddress::RegDirect,
                0b01 => EffectiveAddress::RegIndirect,
                0b10 => EffectiveAddress::BaseDisp,
                _ => return Err(PortableError::InvalidEA(ea_bits)),
            };

            let mut disp = None;
            words_consumed = 1;
            if ea_bits == 0b10 {
                let sz = size.ok_or(PortableError::MissingSize)?;
                let (d, used) = read_disp(&words[1..], sz)?;
                disp = Some(d);
                words_consumed = 1 + used;
            }

            (
                Some(Operand::Reg(src_reg)),
                Some(Operand::Ea {
                    reg: base_reg,
                    ea,
                    disp,
                }),
            )
        }

        // Group 0xB: ALU imm/Movi - 4-bit dst reg, 6-bit unsigned immediate (0-63)
        0xB => {
            let dst_reg_bits = (word & 0x0F) as u8;
            let dst_reg = Reg::from_u8(dst_reg_bits).ok_or(PortableError::Unsupported)?;
            let imm_bits = ((word >> 4) & 0x3F) as u8;
            words_consumed = 1;

            (
                Some(Operand::Reg(dst_reg)),
                Some(Operand::Imm(ImmediateValue::UByte(imm_bits))),
            )
        }

        0xD => {
            if words.len() < 2 {
                return Err(PortableError::Unsupported);
            }
            let ea_bits = ((word >> 8) & 0x3) as u8;
            let base_reg_bits = ((word >> 4) & 0xF) as u8;
            let expect_reg_bits = (word & 0xF) as u8;
            let new_reg_bits = (words[1] & 0xF) as u8;
            let base_reg = Reg::from_u8(base_reg_bits).ok_or(PortableError::Unsupported)?;
            let expect_reg = Reg::from_u8(expect_reg_bits).ok_or(PortableError::Unsupported)?;
            let new_reg = Reg::from_u8(new_reg_bits).ok_or(PortableError::Unsupported)?;
            let ea = match ea_bits {
                0b00 => EffectiveAddress::RegDirect,
                0b01 => EffectiveAddress::RegIndirect,
                0b10 => EffectiveAddress::BaseDisp,
                0b11 => EffectiveAddress::Immediate,
                _ => return Err(PortableError::InvalidEA(ea_bits)),
            };

            words_consumed = 2;
            let mut disp = None;
            if ea_bits == 0b10 {
                let sz = size.ok_or(PortableError::MissingSize)?;
                let (d, used) = read_disp(&words[2..], sz)?;
                disp = Some(d);
                words_consumed = 2 + used;
            }

            target = Some(Operand::Ea {
                reg: base_reg,
                ea,
                disp,
            });

            (Some(Operand::Reg(expect_reg)), Some(Operand::Reg(new_reg)))
        }

        0xF => {
            let ea_bits = ((word >> 8) & 0x3) as u8;
            let base_reg_bits = ((word >> 4) & 0xF) as u8;
            let swap_reg_bits = (word & 0xF) as u8;
            let base_reg = Reg::from_u8(base_reg_bits).ok_or(PortableError::Unsupported)?;
            let swap_reg = Reg::from_u8(swap_reg_bits).ok_or(PortableError::Unsupported)?;
            let ea = match ea_bits {
                0b00 => EffectiveAddress::RegDirect,
                0b01 => EffectiveAddress::RegIndirect,
                0b10 => EffectiveAddress::BaseDisp,
                _ => return Err(PortableError::InvalidEA(ea_bits)),
            };

            let mut disp = None;
            words_consumed = 1;
            if ea_bits == 0b10 {
                let sz = size.ok_or(PortableError::MissingSize)?;
                let (d, used) = read_disp(&words[1..], sz)?;
                disp = Some(d);
                words_consumed = 1 + used;
            }

            (
                Some(Operand::Reg(swap_reg)),
                Some(Operand::Ea {
                    reg: base_reg,
                    ea,
                    disp,
                }),
            )
        }

        _ => (None, None),
    };

    let inst = Instruction {
        opcode,
        size,
        dest,
        src,
        target,
    };
    Ok((inst, words_consumed))
}

/// Decode a binary bytecode program from bytes
/// Bytes are interpreted as little-endian 16-bit words
pub fn decode_program_bytes(bytes: &[u8]) -> Result<Vec<Instruction>, PortableError> {
    // Check that byte count is even (must be 16-bit words)
    if !bytes.len().is_multiple_of(2) {
        return Err(PortableError::Unsupported);
    }

    // Convert bytes to u16 words (little-endian)
    let words: Vec<u16> = bytes
        .chunks_exact(2)
        .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
        .collect();

    // Decode each word into an instruction
    let mut instructions = Vec::new();
    let mut i = 0;
    while i < words.len() {
        let (inst, consumed) = decode(&words[i..])?;
        instructions.push(inst);
        i += consumed;
    }

    Ok(instructions)
}

/// Encode a program to binary bytecode
/// Returns bytes in little-endian format
pub fn encode_program_bytes(instructions: &[Instruction]) -> Result<Vec<u8>, PortableError> {
    let mut bytes = Vec::new();

    for inst in instructions {
        let words = encode(inst)?;
        for word in words {
            // Convert to little-endian bytes
            bytes.extend_from_slice(&word.to_le_bytes());
        }
    }

    Ok(bytes)
}

/// Encode a full program (instructions + data) with a simple header.
/// Layout: [u32 code_len][u32 data_len][code bytes][data bytes]
pub fn encode_program(program: &Program) -> Result<Vec<u8>, PortableError> {
    let code_bytes = encode_program_bytes(&program.instructions)?;

    // Build data blob sized to the highest offset + len, zero-filled.
    let data_size = program
        .data
        .iter()
        .map(|seg| seg.offset + seg.bytes.len())
        .max()
        .unwrap_or(0);
    let mut data = vec![0u8; data_size];
    for seg in &program.data {
        let end = seg.offset + seg.bytes.len();
        if end > data.len() {
            data.resize(end, 0);
        }
        data[seg.offset..end].copy_from_slice(&seg.bytes);
    }

    let mut out = Vec::with_capacity(8 + code_bytes.len() + data.len());
    out.extend_from_slice(&(code_bytes.len() as u32).to_le_bytes());
    out.extend_from_slice(&(data.len() as u32).to_le_bytes());
    out.extend_from_slice(&code_bytes);
    out.extend_from_slice(&data);
    Ok(out)
}

/// Decode a program with optional data header (see `encode_program`).
/// If the header is not present, falls back to instruction-only decoding.
pub fn decode_program(bytes: &[u8]) -> Result<Program, PortableError> {
    if bytes.len() >= 8 {
        let code_len = u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]) as usize;
        let data_len = u32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]) as usize;
        let expected = 8 + code_len + data_len;
        if expected == bytes.len() && code_len.is_multiple_of(2) {
            let code_bytes = &bytes[8..8 + code_len];
            let data_bytes = bytes[8 + code_len..].to_vec();
            let instructions = decode_program_bytes(code_bytes)?;
            let data = if data_bytes.is_empty() {
                Vec::new()
            } else {
                vec![DataSegment {
                    offset: 0,
                    bytes: data_bytes,
                }]
            };
            return Ok(Program { instructions, data });
        }
    }

    // Fallback: instruction-only
    let instructions = decode_program_bytes(bytes)?;
    Ok(Program {
        instructions,
        data: Vec::new(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::portable::instruction::{ImmediateValue, Instruction, Opcode, Operand, Reg, Size};

    #[test]
    fn test_binary_roundtrip() {
        // Create a simple program
        let program = vec![
            Instruction {
                opcode: Opcode::Mov,
                size: Some(Size::Long),
                dest: Some(Operand::Reg(Reg::R0)),
                src: Some(Operand::Reg(Reg::R1)),
                target: None,
            },
            Instruction {
                opcode: Opcode::Addi,
                size: Some(Size::Long),
                dest: Some(Operand::Reg(Reg::R2)),
                src: Some(Operand::Imm(ImmediateValue::Short(42))),
                target: None,
            },
            Instruction {
                opcode: Opcode::Nop,
                size: None,
                dest: None,
                src: None,
                target: None,
            },
        ];

        // Encode to bytes
        let bytes = encode_program(&Program {
            instructions: program.clone(),
            data: Vec::new(),
        })
        .unwrap();
        assert!(!bytes.is_empty());
        assert_eq!(bytes.len() % 2, 0); // Should be even (16-bit words)

        // Decode back
        let decoded = decode_program(&bytes).unwrap();
        assert_eq!(decoded.instructions.len(), program.len());

        // Check first instruction
        assert_eq!(decoded.instructions[0].opcode, Opcode::Mov);
        assert_eq!(decoded.instructions[0].size, Some(Size::Long));

        // Check second instruction
        assert_eq!(decoded.instructions[1].opcode, Opcode::Addi);
        match decoded.instructions[1].src {
            Some(Operand::Imm(ImmediateValue::UByte(offset))) => assert_eq!(offset, 42),
            _ => panic!("Expected Imm(ImmediateValue::UByte(42)) operand"),
        }

        // Check third instruction
        assert_eq!(decoded.instructions[2].opcode, Opcode::Nop);
    }

    #[test]
    fn test_jmps_binary_encoding() {
        // Test Jmps with positive offset
        let inst = Instruction {
            opcode: Opcode::Jmps,
            size: None,
            dest: Some(Operand::Imm(ImmediateValue::Short(511))),
            src: None,
            target: None,
        };

        let bytes = encode_program(&Program {
            instructions: vec![inst],
            data: Vec::new(),
        })
        .unwrap();
        let decoded = decode_program(&bytes).unwrap();

        assert_eq!(decoded.instructions.len(), 1);
        assert_eq!(decoded.instructions[0].opcode, Opcode::Jmps);
        match decoded.instructions[0].dest {
            Some(Operand::Imm(ImmediateValue::Short(offset))) => assert_eq!(offset, 511),
            _ => panic!("Expected Imm(ImmediateValue::Long(10)) operand"),
        }

        // Test Jmps with negative offset
        let inst_neg = Instruction {
            opcode: Opcode::Jmps,
            size: None,
            dest: Some(Operand::Imm(ImmediateValue::Short(-512))),
            src: None,
            target: None,
        };

        let bytes = encode_program(&Program {
            instructions: vec![inst_neg],
            data: Vec::new(),
        })
        .unwrap();
        let decoded = decode_program(&bytes).unwrap();

        match decoded.instructions[0].dest {
            Some(Operand::Imm(ImmediateValue::Short(offset))) => assert_eq!(offset, -512),
            _ => panic!("Expected Imm(ImmediateValue::Long(-50)) operand"),
        }
    }
}
