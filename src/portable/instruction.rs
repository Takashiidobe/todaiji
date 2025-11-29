use std::num::ParseIntError;
use std::{collections::HashMap, fmt, str::FromStr};

use thiserror::Error;

use super::decode::{PortableError, encode};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataSegment {
    pub offset: usize,
    pub bytes: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub instructions: Vec<Instruction>,
    pub data: Vec<DataSegment>,
}

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
pub enum ImmediateValue {
    Byte(i8),
    UByte(u8),
    Short(i16),
    UShort(u16),
    Long(i32),
    ULong(u32),
    Word(i64),
    UWord(u64),
}

impl From<u64> for ImmediateValue {
    fn from(v: u64) -> Self {
        Self::UWord(v)
    }
}

impl From<i64> for ImmediateValue {
    fn from(v: i64) -> Self {
        Self::Word(v)
    }
}

impl From<u32> for ImmediateValue {
    fn from(v: u32) -> Self {
        Self::ULong(v)
    }
}

impl From<i32> for ImmediateValue {
    fn from(v: i32) -> Self {
        Self::Long(v)
    }
}

impl From<u16> for ImmediateValue {
    fn from(v: u16) -> Self {
        Self::UShort(v)
    }
}

impl From<i16> for ImmediateValue {
    fn from(v: i16) -> Self {
        Self::Short(v)
    }
}

impl From<u8> for ImmediateValue {
    fn from(v: u8) -> Self {
        Self::UByte(v)
    }
}

impl From<i8> for ImmediateValue {
    fn from(v: i8) -> Self {
        Self::Byte(v)
    }
}

impl ImmediateValue {
    pub fn as_u64(&self) -> u64 {
        match self {
            ImmediateValue::Byte(v) => (*v as i64) as u64,
            ImmediateValue::UByte(v) => *v as u64,
            ImmediateValue::Short(v) => (*v as i64) as u64,
            ImmediateValue::UShort(v) => *v as u64,
            ImmediateValue::Long(v) => (*v as i64) as u64,
            ImmediateValue::ULong(v) => *v as u64,
            ImmediateValue::Word(v) => *v as u64,
            ImmediateValue::UWord(v) => *v,
        }
    }

    pub fn as_i128(&self) -> i128 {
        match self {
            ImmediateValue::Byte(v) => *v as i128,
            ImmediateValue::UByte(v) => *v as i128,
            ImmediateValue::Short(v) => *v as i128,
            ImmediateValue::UShort(v) => *v as i128,
            ImmediateValue::Long(v) => *v as i128,
            ImmediateValue::ULong(v) => *v as i128,
            ImmediateValue::Word(v) => *v as i128,
            ImmediateValue::UWord(v) => *v as i128,
        }
    }
}

impl FromStr for ImmediateValue {
    type Err = AsmError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Try parsing with suffixes first (e.g., "$123b", "$123ub", "$123s", etc.)
        if let Some(val_str) = s.strip_prefix('$')
            && let Some(suffix) = val_str.chars().last()
        {
            let (num_str, suffix_char) = if suffix.is_alphabetic() {
                let mut chars = val_str.chars();
                chars.next_back(); // Remove the last char
                (chars.collect::<String>(), suffix)
            } else {
                (val_str.to_string(), 'l') // Default to long if no suffix
            };

            return match suffix_char.to_ascii_lowercase() {
                'b' => {
                    if val_str.contains('u') {
                        num_str.parse::<u8>().map(ImmediateValue::UByte)
                    } else {
                        num_str.parse::<i8>().map(ImmediateValue::Byte)
                    }
                }
                's' => {
                    if val_str.contains('u') {
                        num_str.parse::<u16>().map(ImmediateValue::UShort)
                    } else {
                        num_str.parse::<i16>().map(ImmediateValue::Short)
                    }
                }
                'l' => {
                    if val_str.contains('u') {
                        num_str.parse::<u32>().map(ImmediateValue::ULong)
                    } else {
                        num_str.parse::<i32>().map(ImmediateValue::Long)
                    }
                }
                'w' => {
                    if val_str.contains('u') {
                        num_str.parse::<u64>().map(ImmediateValue::UWord)
                    } else {
                        num_str.parse::<i64>().map(ImmediateValue::Word)
                    }
                }
                s => return Err(AsmError::UnknownSize(s.to_string())),
            }
            .map_err(|e: ParseIntError| {
                AsmError::BadOperand(format!("invalid immediate {}: {}", s, e))
            });
        }

        // If no suffix or '$', try parsing as i32 (default for labels/unspecified)
        s.parse::<i32>()
            .map(ImmediateValue::Long)
            .map_err(|e: ParseIntError| {
                AsmError::BadOperand(format!("invalid immediate {}: {}", s, e))
            })
    }
}

impl fmt::Display for ImmediateValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ImmediateValue::Byte(v) => write!(f, "{}", v),
            ImmediateValue::UByte(v) => write!(f, "{}", v),
            ImmediateValue::Short(v) => write!(f, "{}", v),
            ImmediateValue::UShort(v) => write!(f, "{}", v),
            ImmediateValue::Long(v) => write!(f, "{}", v),
            ImmediateValue::ULong(v) => write!(f, "{}", v),
            ImmediateValue::Word(v) => write!(f, "{}", v),
            ImmediateValue::UWord(v) => write!(f, "{}", v),
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
    Imm(ImmediateValue),
    Label(String),
}

impl From<ImmediateValue> for Operand {
    fn from(v: ImmediateValue) -> Self {
        Self::Imm(v)
    }
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
    let program = parse_program(source)?;

    // Then encode each instruction
    let mut words = Vec::new();
    for inst in &program.instructions {
        let mut encoded = encode(inst)?;
        words.append(&mut encoded);
    }
    Ok(words)
}

/// Parse a textual program into instructions (no encoding).
pub fn parse_program(source: &str) -> Result<Program, AsmError> {
    use std::collections::HashMap;

    #[derive(Debug, Clone)]
    enum Item {
        Inst(Instruction, usize),
        Data(Vec<u8>),
        Label(String),
    }

    let mut items = Vec::new();
    let mut labels: HashMap<String, usize> = HashMap::new();

    // First pass: collect labels, instructions, and data directives.
    for (idx, line) in source.lines().enumerate() {
        let trimmed = strip_comment(line).trim();
        if trimmed.is_empty() {
            continue;
        }
        if let Some(label) = trimmed.strip_suffix(':') {
            items.push(Item::Label(label.to_string()));
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix(".byte") {
            let mut bytes = Vec::new();
            for token in rest.split(',').map(|t| t.trim()).filter(|t| !t.is_empty()) {
                let val: i32 = token.parse().map_err(|_| {
                    AsmError::LineError(format!("line {}: bad byte '{token}'", idx + 1))
                })?;
                if !(0..=255).contains(&val) {
                    return Err(AsmError::LineError(format!(
                        "line {}: byte out of range",
                        idx + 1
                    )));
                }
                bytes.push(val as u8);
            }
            items.push(Item::Data(bytes));
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix(".ascii") {
            let strings = parse_string_directive(rest, idx + 1)?;
            let bytes: Vec<u8> = strings.into_iter().flatten().collect();
            items.push(Item::Data(bytes));
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix(".asciz") {
            let strings = parse_string_directive(rest, idx + 1)?;
            let mut bytes = Vec::new();
            for mut s in strings {
                bytes.append(&mut s);
                bytes.push(0);
            }
            items.push(Item::Data(bytes));
            continue;
        }

        let inst = parse_asm_line(trimmed)
            .map_err(|e| AsmError::LineError(format!("line {}: {e}", idx + 1)))?;
        items.push(Item::Inst(inst, idx + 1));
    }

    // Compute byte offsets by simulating encoding lengths.
    let mut offsets = Vec::new(); // parallel to items
    let mut current_offset = 0usize;
    for item in &items {
        offsets.push(current_offset);
        match item {
            Item::Label(_) => {}
            Item::Data(bytes) => {
                current_offset += bytes.len();
            }
            Item::Inst(inst, _) => {
                let mut cloned = inst.clone();
                strip_labels(&mut cloned);
                let words = encode(&cloned)
                    .map_err(|e| AsmError::LineError(format!("cannot size instruction: {e:?}")))?;
                current_offset += words.len() * 2;
            }
        }
    }

    // Collect label definitions with their byte offsets.
    for (item, &off) in items.iter().zip(offsets.iter()) {
        if let Item::Label(name) = item {
            labels.insert(name.clone(), off);
        }
    }

    // Second pass: resolve labels in instructions and collect data.
    let mut instructions = Vec::new();
    let mut data_segments = Vec::new();
    for (item, offset) in items.into_iter().zip(offsets.into_iter()) {
        match item {
            Item::Label(_) => {}
            Item::Data(bytes) => {
                data_segments.push(DataSegment { offset, bytes });
            }
            Item::Inst(mut inst, line_no) => {
                resolve_labels_with_offset(&mut inst, &labels, offset)
                    .map_err(|e| AsmError::LineError(format!("line {}: {e}", line_no)))?;
                instructions.push(inst);
            }
        }
    }

    Ok(Program {
        instructions,
        data: data_segments,
    })
}

fn parse_string_directive(rest: &str, line_no: usize) -> Result<Vec<Vec<u8>>, AsmError> {
    let tokens = split_string_tokens(rest, line_no)?;
    if tokens.is_empty() {
        return Err(AsmError::LineError(format!(
            "line {}: missing string literal",
            line_no
        )));
    }
    tokens
        .into_iter()
        .map(|tok| parse_string_literal(&tok, line_no))
        .collect()
}

fn split_string_tokens(rest: &str, line_no: usize) -> Result<Vec<String>, AsmError> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    let mut in_quotes = false;
    let mut escape = false;

    for ch in rest.trim().chars() {
        if in_quotes {
            current.push(ch);
            if escape {
                escape = false;
            } else if ch == '\\' {
                escape = true;
            } else if ch == '"' {
                in_quotes = false;
            }
        } else if ch == ',' {
            let token = current.trim();
            if !token.is_empty() {
                tokens.push(token.to_string());
            }
            current.clear();
        } else {
            if ch == '"' {
                in_quotes = true;
            }
            current.push(ch);
        }
    }

    if in_quotes {
        return Err(AsmError::LineError(format!(
            "line {}: unterminated string literal",
            line_no
        )));
    }

    let token = current.trim();
    if !token.is_empty() {
        tokens.push(token.to_string());
    }

    Ok(tokens)
}

fn parse_string_literal(token: &str, line_no: usize) -> Result<Vec<u8>, AsmError> {
    let trimmed = token.trim();
    if !trimmed.starts_with('"') || !trimmed.ends_with('"') || trimmed.len() < 2 {
        return Err(AsmError::LineError(format!(
            "line {}: expected string literal",
            line_no
        )));
    }

    let inner = &trimmed[1..trimmed.len() - 1];
    let mut bytes = Vec::new();
    let mut escape = false;
    let mut chars = inner.chars();

    while let Some(ch) = chars.next() {
        if escape {
            let byte = match ch {
                'n' => b'\n',
                'r' => b'\r',
                't' => b'\t',
                '0' => 0,
                '\\' => b'\\',
                '"' => b'"',
                '\'' => b'\'',
                'x' => {
                    let hi = chars.next().ok_or_else(|| {
                        AsmError::LineError(format!("line {}: incomplete \\x escape", line_no))
                    })?;
                    let lo = chars.next().ok_or_else(|| {
                        AsmError::LineError(format!("line {}: incomplete \\x escape", line_no))
                    })?;
                    let hex = format!("{hi}{lo}");
                    u8::from_str_radix(&hex, 16).map_err(|_| {
                        AsmError::LineError(format!("line {}: invalid \\x escape", line_no))
                    })?
                }
                other => {
                    return Err(AsmError::LineError(format!(
                        "line {}: unknown escape sequence '\\{other}'",
                        line_no
                    )));
                }
            };
            bytes.push(byte);
            escape = false;
        } else if ch == '\\' {
            escape = true;
        } else {
            let mut buf = [0u8; 4];
            let encoded = ch.encode_utf8(&mut buf);
            bytes.extend_from_slice(encoded.as_bytes());
        }
    }

    if escape {
        return Err(AsmError::LineError(format!(
            "line {}: unterminated escape sequence in string literal",
            line_no
        )));
    }

    Ok(bytes)
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
        // Use the new ImmediateValue::from_str to parse the immediate value
        let val = ImmediateValue::from_str(rest)?;
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

fn strip_labels(inst: &mut Instruction) {
    let replace = |op: &mut Option<Operand>| {
        if matches!(op, Some(Operand::Label(_))) {
            *op = Some(Operand::Imm(ImmediateValue::Long(0)));
        }
    };
    replace(&mut inst.dest);
    replace(&mut inst.src);
}

fn resolve_labels_with_offset(
    inst: &mut Instruction,
    labels: &HashMap<String, usize>,
    current_offset: usize,
) -> Result<(), AsmError> {
    // For Jmps, compute relative offset; for others, use absolute address
    let is_relative = matches!(inst.opcode, Opcode::Jmps);

    let translate = |op: &mut Option<Operand>| -> Result<(), AsmError> {
        if let Some(Operand::Label(name)) = op {
            let target = labels
                .get(name)
                .ok_or_else(|| AsmError::BadOperand(format!("unknown label '{name}'")))?;

            let value = if is_relative {
                // Compute relative offset from current instruction (bytes)
                (*target as i32) - (current_offset as i32)
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

            *op = Some(Operand::Imm(ImmediateValue::Long(value)));
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
        assert!(matches!(
            inst.src,
            Some(Operand::Imm(ImmediateValue::Long(5)))
        ));
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
        assert_eq!(insts.instructions.len(), 3);

        // Check that the jmp instruction has the label resolved to instruction index 1
        match &insts.instructions[2].dest {
            Some(Operand::Imm(_)) => {}
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
        assert_eq!(insts.instructions.len(), 3);

        // Check that the jmp resolves to instruction 2
        match &insts.instructions[0].dest {
            Some(Operand::Imm(_)) => {}
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
        assert_eq!(insts.instructions.len(), 3);

        // brz should have %r1 as dest and label as src
        match (&insts.instructions[0].dest, &insts.instructions[0].src) {
            (Some(Operand::Reg(Reg::R1)), Some(Operand::Imm(_))) => {}
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
        // jmps should have offset = 6 bytes (3 instructions * 2 bytes)
        match &insts.instructions[0].dest {
            Some(Operand::Imm(offset)) => assert_eq!(offset.as_i128(), 6),
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
        // jmps at instruction 2 should have offset = -4 bytes (back to instruction 0)
        match &insts.instructions[2].dest {
            Some(Operand::Imm(offset)) => assert_eq!(offset.as_i128(), -4),
            _ => panic!("Expected immediate operand with offset"),
        }
    }

    #[test]
    fn parse_ascii_directive() {
        let src = r#"
message:
        .ascii "Hi", "!"
start:
        nop
        "#;
        let program = parse_program(src).unwrap();
        assert_eq!(program.data.len(), 1);
        assert_eq!(program.data[0].offset, 0);
        assert_eq!(program.data[0].bytes, b"Hi!".to_vec());
        assert_eq!(program.instructions.len(), 1);
    }

    #[test]
    fn parse_asciz_directive_appends_null() {
        let src = r#"
        .asciz "OK"
        .byte 1
        "#;
        let program = parse_program(src).unwrap();
        assert_eq!(program.data.len(), 2);
        assert_eq!(program.data[0].bytes, b"OK\0".to_vec());
        assert_eq!(program.data[1].offset, 3);
        assert_eq!(program.data[1].bytes, vec![1]);
    }

    #[test]
    fn parse_ascii_with_escapes() {
        let src = ".ascii \"A\\nB\\tC\\\\\\\"D\\x21\"";
        let program = parse_program(src).unwrap();
        assert_eq!(program.data.len(), 1);
        assert_eq!(program.data[0].bytes, b"A\nB\tC\\\"D!".to_vec());
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
            dest: Some(Operand::Imm(ImmediateValue::from(42i16))),
            src: None,
        };
        let encoded = encode(&inst_pos).unwrap();
        assert_eq!(encoded.len(), 1); // Should be single word
        let (decoded, _) = decode(&encoded).unwrap();
        assert_eq!(decoded.opcode, Opcode::Jmps);
        match decoded.dest {
            Some(Operand::Imm(offset)) => assert_eq!(offset.as_u64(), 42),
            _ => panic!("Expected immediate operand"),
        }

        // Test negative offset
        let inst_neg = Instruction {
            opcode: Opcode::Jmps,
            size: None,
            dest: Some(Operand::Imm(ImmediateValue::from(-100i16))),
            src: None,
        };
        let encoded = encode(&inst_neg).unwrap();
        let (decoded, _) = decode(&encoded).unwrap();
        match decoded.dest {
            Some(Operand::Imm(offset)) => assert_eq!(offset.as_i128(), -100),
            _ => panic!("Expected immediate operand"),
        }

        // Test max positive offset (511)
        let inst_max_pos = Instruction {
            opcode: Opcode::Jmps,
            size: None,
            dest: Some(Operand::Imm(ImmediateValue::Short(511))),
            src: None,
        };
        let encoded = encode(&inst_max_pos).unwrap();
        let (decoded, _) = decode(&encoded).unwrap();
        match decoded.dest {
            Some(Operand::Imm(offset)) => assert_eq!(offset.as_u64(), 511),
            _ => panic!("Expected immediate operand"),
        }

        // Test max negative offset (-512)
        let inst_max_neg = Instruction {
            opcode: Opcode::Jmps,
            size: None,
            dest: Some(Operand::Imm(ImmediateValue::Short(-512))),
            src: None,
        };
        let encoded = encode(&inst_max_neg).unwrap();
        let (decoded, _) = decode(&encoded).unwrap();
        match decoded.dest {
            Some(Operand::Imm(offset)) => assert_eq!(offset.as_i128(), -512),
            _ => panic!("Expected immediate operand"),
        }
    }
}
