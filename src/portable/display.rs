use std::fmt;

use super::instruction::{
    EffectiveAddress, Instruction, Opcode, Operand, REG_PC, REG_SP, Reg, Size,
};

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Opcode::Nop => "nop",
            Opcode::Trap => "trap",
            Opcode::Lea => "lea",
            Opcode::Add => "add",
            Opcode::Sub => "sub",
            Opcode::Mul => "mul",
            Opcode::Div => "div",
            Opcode::Divu => "divu",
            Opcode::Rem => "rem",
            Opcode::Remu => "remu",
            Opcode::And => "and",
            Opcode::Or => "or",
            Opcode::Xor => "xor",
            Opcode::Swap => "swap",
            Opcode::Not => "not",
            Opcode::Neg => "neg",
            Opcode::Sxt => "sxt",
            Opcode::Zxt => "zxt",
            Opcode::BrEq => "breq",
            Opcode::BrNe => "brne",
            Opcode::BrLt => "brlt",
            Opcode::BrGe => "brge",
            Opcode::BrLts => "brlts",
            Opcode::BrGes => "brges",
            Opcode::BrZ => "brz",
            Opcode::BrNz => "brnz",
            Opcode::Ret => "ret",
            Opcode::Mov => "mov",
            Opcode::Push => "push",
            Opcode::Pop => "pop",
            Opcode::Jmp => "jmp",
            Opcode::Jmps => "jmps",
            Opcode::Call => "call",
            Opcode::Jmpi => "jmpi",
            Opcode::Calli => "calli",
            Opcode::Shl => "shl",
            Opcode::Rol => "rol",
            Opcode::Shr => "shr",
            Opcode::Ror => "ror",
            Opcode::Load => "load",
            Opcode::Store => "store",
            Opcode::Addi => "addi",
            Opcode::Subi => "subi",
            Opcode::Muli => "muli",
            Opcode::Remi => "remi",
            Opcode::Movi => "movi",
            Opcode::Reserved => "res",
        };
        f.write_str(s)
    }
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let suf = match self {
            Size::Byte => "b",
            Size::Short => "s",
            Size::Long => "l",
            Size::Word => "w",
        };
        f.write_str(suf)
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(sz) = self.size {
            write!(f, "{}.{}", self.opcode, sz)?;
        } else {
            write!(f, "{}", self.opcode)?;
        }
        if let Some(dest) = &self.dest {
            write!(f, " {}", fmt_operand(dest))?;
        }
        if let Some(src) = &self.src {
            write!(f, ", {}", fmt_operand(src))?;
        }
        Ok(())
    }
}

fn fmt_operand(op: &Operand) -> String {
    match op {
        Operand::Reg(r) => format_reg(*r),
        Operand::Imm(v) => format!("#{}", v),
        Operand::Label(name) => name.clone(),
        Operand::Ea { reg, ea, disp } => match ea {
            EffectiveAddress::RegIndirect => format!("({})", format_reg(*reg)),
            EffectiveAddress::BaseDisp => format_disp(*reg, *disp),
            EffectiveAddress::Scaled => format!("scaled({})", format_reg(*reg)),
            EffectiveAddress::Immediate => "#imm?".to_string(),
        },
    }
}

fn format_reg(reg: Reg) -> String {
    match reg {
        REG_SP => "%sp".to_string(),
        REG_PC => "%pc".to_string(),
        _ => format!("%r{}", reg.to_u8()),
    }
}

fn format_disp(reg: Reg, payload: Option<i32>) -> String {
    let disp = payload.unwrap_or(0);
    let reg_str = format_reg(reg);
    format!("{disp}({reg_str})")
}
