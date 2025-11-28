use std::{collections::HashMap, str::FromStr};

use thiserror::Error;

use super::decode::{PortableError, encode};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

pub const REG_SP: Reg = Reg::R15;
pub const REG_PC: Reg = Reg::R14;

impl Reg {
    pub fn to_u8(self) -> u8 {
        match self {
            Reg::R0 => 0,
            Reg::R1 => 1,
            Reg::R2 => 2,
            Reg::R3 => 3,
            Reg::R4 => 4,
            Reg::R5 => 5,
            Reg::R6 => 6,
            Reg::R7 => 7,
            Reg::R8 => 8,
            Reg::R9 => 9,
            Reg::R10 => 10,
            Reg::R11 => 11,
            Reg::R12 => 12,
            Reg::R13 => 13,
            Reg::R14 => 14,
            Reg::R15 => 15,
        }
    }

    pub fn from_u8(val: u8) -> Option<Self> {
        Some(match val {
            0 => Reg::R0,
            1 => Reg::R1,
            2 => Reg::R2,
            3 => Reg::R3,
            4 => Reg::R4,
            5 => Reg::R5,
            6 => Reg::R6,
            7 => Reg::R7,
            8 => Reg::R8,
            9 => Reg::R9,
            10 => Reg::R10,
            11 => Reg::R11,
            12 => Reg::R12,
            13 => Reg::R13,
            14 => Reg::R14,
            15 => Reg::R15,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Opcode {
    Lea,
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Xor,
    Swap,
    Not,
    Neg,
    Sxt,
    Zxt,
    BrEq,
    BrNe,
    BrLt,
    BrGe,
    BrLts,
    BrGes,
    BrZ,
    BrNz,
    Ret,
    Mov,
    Push,
    Pop,
    Jmp,
    Jmps,
    Call,
    Jmpi,
    Calli,
    Shl,
    Rol,
    Shr,
    Ror,
    Trap,
    Nop,
    Load,
    Store,
    Addi,
    Subi,
    Muli,
    Remi,
    Divu,
    Rem,
    Remu,
    Reserved,
}

impl FromStr for Opcode {
    type Err = AsmError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let opcode = match s.to_ascii_lowercase().as_str() {
            "nop" => Opcode::Nop,
            "trap" => Opcode::Trap,
            "lea" => Opcode::Lea,
            "add" => Opcode::Add,
            "sub" => Opcode::Sub,
            "mul" => Opcode::Mul,
            "div" => Opcode::Div,
            "divu" => Opcode::Divu,
            "rem" => Opcode::Rem,
            "remu" => Opcode::Remu,
            "and" => Opcode::And,
            "or" => Opcode::Or,
            "xor" => Opcode::Xor,
            "swap" => Opcode::Swap,
            "not" => Opcode::Not,
            "neg" => Opcode::Neg,
            "sxt" => Opcode::Sxt,
            "zxt" => Opcode::Zxt,
            "breq" => Opcode::BrEq,
            "brne" => Opcode::BrNe,
            "brlt" => Opcode::BrLt,
            "brge" => Opcode::BrGe,
            "brlts" => Opcode::BrLts,
            "brges" => Opcode::BrGes,
            "brz" => Opcode::BrZ,
            "brnz" => Opcode::BrNz,
            "ret" => Opcode::Ret,
            "mov" => Opcode::Mov,
            "push" => Opcode::Push,
            "pop" => Opcode::Pop,
            "jmp" => Opcode::Jmp,
            "jmps" => Opcode::Jmps,
            "call" => Opcode::Call,
            "jmpi" => Opcode::Jmpi,
            "calli" => Opcode::Calli,
            "shl" => Opcode::Shl,
            "rol" => Opcode::Rol,
            "shr" => Opcode::Shr,
            "ror" => Opcode::Ror,
            "load" => Opcode::Load,
            "store" => Opcode::Store,
            "addi" => Opcode::Addi,
            "subi" => Opcode::Subi,
            "muli" => Opcode::Muli,
            "remi" => Opcode::Remi,
            _ => Opcode::Reserved,
        };
        Ok(opcode)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Size {
    Byte,
    Short,
    Long,
    Word,
}

impl TryFrom<char> for Size {
    type Error = AsmError;

    fn try_from(c: char) -> Result<Self, AsmError> {
        match c {
            'b' | 'B' => Ok(Size::Byte),
            's' | 'S' => Ok(Size::Short),
            'l' | 'L' => Ok(Size::Long),
            'w' | 'W' => Ok(Size::Word),
            other => Err(AsmError::UnknownSize(other.to_string())),
        }
    }
}

impl Size {
    pub fn to_bits(self) -> u8 {
        match self {
            Size::Byte => 0b00,
            Size::Short => 0b01,
            Size::Long => 0b10,
            Size::Word => 0b11,
        }
    }

    pub fn from_bits(bits: u8) -> Result<Self, PortableError> {
        match bits & 0b11 {
            0b00 => Ok(Size::Byte),
            0b01 => Ok(Size::Short),
            0b10 => Ok(Size::Long),
            0b11 => Ok(Size::Word),
            _ => Err(PortableError::InvalidSize(bits)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EffectiveAddress {
    RegIndirect,
    BaseDisp,
    Scaled,
    Immediate,
}

impl EffectiveAddress {
    pub fn from_bits(bits: u8) -> Result<Self, PortableError> {
        match bits & 0b11 {
            0b00 => Ok(EffectiveAddress::RegIndirect),
            0b01 => Ok(EffectiveAddress::BaseDisp),
            0b10 => Ok(EffectiveAddress::Scaled),
            0b11 => Ok(EffectiveAddress::Immediate),
            _ => Err(PortableError::InvalidEA(bits)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    Reg(Reg),
    Ea {
        reg: Reg,
        ea: EffectiveAddress,
        disp: Option<i32>,
    },
    Imm(i32),
    Label(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instruction {
    pub opcode: Opcode,
    pub size: Option<Size>, // control ops may not use size
    pub dest: Option<Operand>,
    pub src: Option<Operand>,
}

impl FromStr for Instruction {
    type Err = AsmError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_asm_line(s)
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum AsmError {
    #[error("unknown opcode '{0}'")]
    UnknownOpcode(String),
    #[error("unknown size suffix '{0}'")]
    UnknownSize(String),
    #[error("bad operand '{0}'")]
    BadOperand(String),
    #[error("bad register '{0}'")]
    BadRegister(String),
    #[error("too many operands")]
    TooManyOperands,
    #[error("{0}")]
    LineError(String),
    #[error(transparent)]
    Encode(#[from] PortableError),
}

/// Parse a single assembly line: `op[.size] [dest[, src]]`
pub fn parse_asm_line(line: &str) -> Result<Instruction, AsmError> {
    let trimmed = strip_comment(line).trim();
    if trimmed.is_empty() {
        return Err(AsmError::BadOperand("empty line".into()));
    }
    let mut parts = trimmed.splitn(2, char::is_whitespace);
    let op_part = parts
        .next()
        .ok_or_else(|| AsmError::BadOperand(trimmed.into()))?;
    let rest = parts.next().unwrap_or("").trim();

    let (opcode, size) = parse_opcode_with_size(op_part)?;

    let args = if rest.is_empty() {
        vec![]
    } else {
        rest.split(',')
            .map(|t| t.trim())
            .filter(|t| !t.is_empty())
            .collect()
    };
    if args.len() > 2 {
        return Err(AsmError::TooManyOperands);
    }
    let dest = args.first().map(|t| parse_operand(t)).transpose()?;
    let src = args.get(1).map(|t| parse_operand(t)).transpose()?;

    Ok(Instruction {
        opcode,
        size,
        dest,
        src,
    })
}

/// Assemble multiple lines (one instruction per line).
pub fn assemble_program(source: &str) -> Result<Vec<u16>, AsmError> {
    // First parse the program to resolve labels
    let instructions = parse_program(source)?;

    // Then encode each instruction
    let mut words = Vec::new();
    for inst in &instructions {
        let mut encoded = encode(inst)?;
        words.append(&mut encoded);
    }
    Ok(words)
}

/// Parse a textual program into instructions (no encoding).
pub fn parse_program(source: &str) -> Result<Vec<Instruction>, AsmError> {
    use std::collections::HashMap;

    let mut insts = Vec::new();
    let mut labels: HashMap<String, usize> = HashMap::new();

    // First pass: collect labels and raw instructions.
    for (idx, line) in source.lines().enumerate() {
        let trimmed = strip_comment(line).trim();
        if trimmed.is_empty() {
            continue;
        }
        if let Some(label) = trimmed.strip_suffix(':') {
            labels.insert(label.to_string(), insts.len());
            continue;
        }
        let inst = parse_asm_line(trimmed)
            .map_err(|e| AsmError::LineError(format!("line {}: {e}", idx + 1)))?;
        insts.push((inst, idx + 1));
    }

    // Second pass: resolve label operands into immediates.
    let mut resolved = Vec::with_capacity(insts.len());
    for (inst_idx, (mut inst, line_no)) in insts.into_iter().enumerate() {
        resolve_labels(&mut inst, &labels, inst_idx)
            .map_err(|e| AsmError::LineError(format!("line {}: {e}", line_no)))?;
        resolved.push(inst);
    }

    Ok(resolved)
}

fn parse_opcode_with_size(token: &str) -> Result<(Opcode, Option<Size>), AsmError> {
    let (op_str, size) = if let Some((base, suf)) = token.split_once('.') {
        let sz_char = suf.chars().next().unwrap_or_default();
        (base, Some(Size::try_from(sz_char)?))
    } else {
        (token, None)
    };
    let opcode =
        Opcode::from_str(op_str).map_err(|_| AsmError::UnknownOpcode(op_str.to_string()))?;
    Ok((opcode, size))
}

fn parse_operand(token: &str) -> Result<Operand, AsmError> {
    if let Some(rest) = token.strip_prefix('$') {
        let val: i32 = rest
            .parse()
            .map_err(|_| AsmError::BadOperand(token.to_string()))?;
        return Ok(Operand::Imm(val));
    }
    // scaled syntax not parsed yet; treat as base+disp or reg indirect
    if token.starts_with('(') && token.ends_with(')') {
        let reg = parse_reg(&token[1..token.len() - 1])?;
        return Ok(Operand::Ea {
            reg,
            ea: EffectiveAddress::RegIndirect,
            disp: None,
        });
    }
    if let Some(idx) = token.find('(')
        && token.ends_with(')')
    {
        let disp = token[..idx]
            .parse::<i32>()
            .map_err(|_| AsmError::BadOperand(token.to_string()))?;
        let reg = parse_reg(&token[idx + 1..token.len() - 1])?;
        return Ok(Operand::Ea {
            reg,
            ea: EffectiveAddress::BaseDisp,
            disp: Some(disp),
        });
    }
    // try register
    if let Ok(reg) = parse_reg(token) {
        Ok(Operand::Reg(reg))
    } else {
        // otherwise treat as label reference
        Ok(Operand::Label(token.to_string()))
    }
}

fn parse_reg(token: &str) -> Result<Reg, AsmError> {
    let trimmed = token.trim_start_matches('%');
    match trimmed.to_ascii_lowercase().as_str() {
        "sp" => return Ok(REG_SP),
        "pc" => return Ok(REG_PC),
        _ => {}
    }
    let stripped = trimmed
        .strip_prefix('r')
        .ok_or_else(|| AsmError::BadRegister(token.to_string()))?;
    let val: u8 = stripped
        .parse()
        .map_err(|_| AsmError::BadRegister(token.to_string()))?;
    Reg::from_u8(val).ok_or_else(|| AsmError::BadRegister(token.to_string()))
}

fn resolve_labels(
    inst: &mut Instruction,
    labels: &HashMap<String, usize>,
    current_pc: usize,
) -> Result<(), AsmError> {
    // For Jmps, compute relative offset; for others, use absolute address
    let is_relative = matches!(inst.opcode, Opcode::Jmps);

    let translate = |op: &mut Option<Operand>| -> Result<(), AsmError> {
        if let Some(Operand::Label(name)) = op {
            let target = labels
                .get(name)
                .ok_or_else(|| AsmError::BadOperand(format!("unknown label '{name}'")))?;

            let value = if is_relative {
                // Compute relative offset from current instruction
                (*target as i32) - (current_pc as i32)
            } else {
                *target as i32
            };

            // Check bounds for Jmps (10-bit signed: -512 to 511)
            if is_relative && !(-512..=511).contains(&value) {
                return Err(AsmError::BadOperand(format!(
                    "jump offset {} out of range for jmps (must be -512 to 511)",
                    value
                )));
            }

            *op = Some(Operand::Imm(value));
        }
        Ok(())
    };
    translate(&mut inst.dest)?;
    translate(&mut inst.src)?;
    Ok(())
}

fn strip_comment(line: &str) -> &str {
    if let Some(idx) = line.find("//") {
        return &line[..idx];
    }
    if let Some(idx) = line.find('#') {
        return &line[..idx];
    }
    line
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_two_operands() {
        let inst: Instruction = "add.l %r1, $5".parse().unwrap();
        assert_eq!(inst.opcode, Opcode::Add);
        assert_eq!(inst.size, Some(Size::Long));
        assert!(matches!(inst.dest, Some(Operand::Reg(Reg::R1))));
        assert!(matches!(inst.src, Some(Operand::Imm(5))));
    }

    #[test]
    fn assemble_program_lines() {
        let src = r#"
            mov.b %r1, 4(%r2)
            jmp 0(%pc)
        "#;
        let words = assemble_program(src).unwrap();
        assert!(!words.is_empty());
    }

    #[test]
    fn parse_label_definition() {
        let src = r#"
            mov.l %r0, $5
loop_start:
            subi.l %r0, $1
            jmp loop_start
        "#;
        let insts = parse_program(src).unwrap();
        assert_eq!(insts.len(), 3);

        // Check that the jmp instruction has the label resolved to instruction index 1
        match &insts[2].dest {
            Some(Operand::Imm(idx)) => assert_eq!(*idx, 1),
            _ => panic!("Expected immediate operand with resolved label"),
        }
    }

    #[test]
    fn parse_forward_label() {
        let src = r#"
            jmp target
            nop
target:
            nop
        "#;
        let insts = parse_program(src).unwrap();
        assert_eq!(insts.len(), 3);

        // Check that the jmp resolves to instruction 2
        match &insts[0].dest {
            Some(Operand::Imm(idx)) => assert_eq!(*idx, 2),
            _ => panic!("Expected immediate operand with resolved forward label"),
        }
    }

    #[test]
    fn parse_branch_with_label() {
        let src = r#"
            brz.l %r1, end
            addi.l %r1, $10
end:
            ret
        "#;
        let insts = parse_program(src).unwrap();
        assert_eq!(insts.len(), 3);

        // brz should have %r1 as dest and label as src
        match (&insts[0].dest, &insts[0].src) {
            (Some(Operand::Reg(Reg::R1)), Some(Operand::Imm(idx))) => {
                assert_eq!(*idx, 2);
            }
            _ => panic!("Expected reg and resolved label for brz"),
        }
    }

    #[test]
    fn unknown_label_error() {
        let src = r#"
            jmp unknown_label
        "#;
        let result = parse_program(src);
        assert!(result.is_err());
        assert!(matches!(result, Err(AsmError::LineError(_))));
    }

    #[test]
    fn jmps_forward_offset() {
        let src = r#"
            jmps target
            nop
            nop
target:
            nop
        "#;
        let insts = parse_program(src).unwrap();
        // jmps should have offset = 3 (from instruction 0 to instruction 3)
        match &insts[0].dest {
            Some(Operand::Imm(offset)) => assert_eq!(*offset, 3),
            _ => panic!("Expected immediate operand with offset"),
        }
    }

    #[test]
    fn jmps_backward_offset() {
        let src = r#"
loop:
            nop
            nop
            jmps loop
        "#;
        let insts = parse_program(src).unwrap();
        // jmps at instruction 2 should have offset = -2 (back to instruction 0)
        match &insts[2].dest {
            Some(Operand::Imm(offset)) => assert_eq!(*offset, -2),
            _ => panic!("Expected immediate operand with offset"),
        }
    }

    #[test]
    fn jmps_range_check() {
        // Test that offset out of range is rejected
        let mut src = String::from("jmps target\n");
        for _ in 0..600 {
            src.push_str("nop\n");
        }
        src.push_str("target:\nnop\n");
        let result = parse_program(&src);
        assert!(result.is_err());
        // Should contain "out of range" error
        if let Err(e) = result {
            assert!(e.to_string().contains("out of range"));
        }
    }

    #[test]
    fn jmps_encoding_decoding() {
        use super::super::decode::{decode, encode};

        // Test positive offset
        let inst_pos = Instruction {
            opcode: Opcode::Jmps,
            size: None,
            dest: Some(Operand::Imm(42)),
            src: None,
        };
        let encoded = encode(&inst_pos).unwrap();
        assert_eq!(encoded.len(), 1); // Should be single word
        let (decoded, _) = decode(&encoded).unwrap();
        assert_eq!(decoded.opcode, Opcode::Jmps);
        match decoded.dest {
            Some(Operand::Imm(offset)) => assert_eq!(offset, 42),
            _ => panic!("Expected immediate operand"),
        }

        // Test negative offset
        let inst_neg = Instruction {
            opcode: Opcode::Jmps,
            size: None,
            dest: Some(Operand::Imm(-100)),
            src: None,
        };
        let encoded = encode(&inst_neg).unwrap();
        let (decoded, _) = decode(&encoded).unwrap();
        match decoded.dest {
            Some(Operand::Imm(offset)) => assert_eq!(offset, -100),
            _ => panic!("Expected immediate operand"),
        }

        // Test max positive offset (511)
        let inst_max_pos = Instruction {
            opcode: Opcode::Jmps,
            size: None,
            dest: Some(Operand::Imm(511)),
            src: None,
        };
        let encoded = encode(&inst_max_pos).unwrap();
        let (decoded, _) = decode(&encoded).unwrap();
        match decoded.dest {
            Some(Operand::Imm(offset)) => assert_eq!(offset, 511),
            _ => panic!("Expected immediate operand"),
        }

        // Test max negative offset (-512)
        let inst_max_neg = Instruction {
            opcode: Opcode::Jmps,
            size: None,
            dest: Some(Operand::Imm(-512)),
            src: None,
        };
        let encoded = encode(&inst_max_neg).unwrap();
        let (decoded, _) = decode(&encoded).unwrap();
        match decoded.dest {
            Some(Operand::Imm(offset)) => assert_eq!(offset, -512),
            _ => panic!("Expected immediate operand"),
        }
    }
}
