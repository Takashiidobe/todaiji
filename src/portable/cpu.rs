use crate::portable::instruction::{EffectiveAddress, Instruction, Operand, Opcode, Reg, Size, REG_SP};

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum CpuError {
    #[error("missing operand")]
    MissingOperand,
    #[error("invalid operand shape")]
    InvalidOperand,
    #[error("unsupported effective address")]
    UnsupportedEa,
    #[error("division by zero")]
    DivByZero,
    #[error("program counter out of range")]
    PcOob,
    #[error("unsupported opcode")]
    UnsupportedOpcode,
}

/// Simple in-memory simulator for the portable ISA.
pub struct Cpu {
    pub regs: [u64; 16],
    pub mem: Vec<u8>,
    pub pc: usize,
}

impl Cpu {
    pub fn new(mem_size: usize) -> Self {
        let mut cpu = Cpu {
            regs: [0; 16],
            mem: vec![0; mem_size],
            pc: 0,
        };
        cpu.regs[REG_SP.to_u8() as usize] = mem_size as u64;
        cpu
    }

    /// Execute the program starting at pc until it falls off the end.
    pub fn run(&mut self, program: &[Instruction]) -> Result<(), CpuError> {
        while self.pc < program.len() {
            let inst = &program[self.pc];
            self.step(inst)?;
            // pc incremented inside step for branches/jumps
        }
        Ok(())
    }

    fn step(&mut self, inst: &Instruction) -> Result<(), CpuError> {
        match inst.opcode {
            Opcode::Nop => {
                self.pc += 1;
            }
            Opcode::Trap => {
                // no-op trap for now
                self.pc += 1;
            }
            Opcode::Lea => {
                let dest = self.expect_reg(inst.dest.as_ref())?;
                let addr = self.resolve_address(inst.src.as_ref(), inst.size)?;
                self.regs[dest as usize] = addr;
                self.pc += 1;
            }
            Opcode::Mov => {
                let dest = self.expect_reg(inst.dest.as_ref())?;
                let val = self.read_operand(inst.src.as_ref(), inst.size)?;
                self.regs[dest as usize] = val;
                self.pc += 1;
            }
            Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div | Opcode::Divu | Opcode::Rem | Opcode::Remu => {
                let dest = self.expect_reg(inst.dest.as_ref())?;
                let lhs = self.regs[dest as usize];
                let rhs = self.read_operand(inst.src.as_ref(), inst.size)?;
                let res = match inst.opcode {
                    Opcode::Add => lhs.wrapping_add(rhs),
                    Opcode::Sub => lhs.wrapping_sub(rhs),
                    Opcode::Mul => lhs.wrapping_mul(rhs),
                    Opcode::Div => {
                        let r = if rhs == 0 { return Err(CpuError::DivByZero); } else { rhs };
                        (lhs as i64).checked_div(r as i64).ok_or(CpuError::DivByZero)? as u64
                    }
                    Opcode::Divu => {
                        let r = if rhs == 0 { return Err(CpuError::DivByZero); } else { rhs };
                        lhs / r
                    }
                    Opcode::Rem => {
                        let r = if rhs == 0 { return Err(CpuError::DivByZero); } else { rhs };
                        (lhs as i64).checked_rem(r as i64).ok_or(CpuError::DivByZero)? as u64
                    }
                    Opcode::Remu => {
                        let r = if rhs == 0 { return Err(CpuError::DivByZero); } else { rhs };
                        lhs % r
                    }
                    _ => unreachable!(),
                };
                self.regs[dest as usize] = self.mask_to_size(res, inst.size);
                self.pc += 1;
            }
            Opcode::And | Opcode::Or | Opcode::Xor | Opcode::Not | Opcode::Neg => {
                let dest = self.expect_reg(inst.dest.as_ref())?;
                let lhs = self.regs[dest as usize];
                let rhs = self.read_operand(inst.src.as_ref(), inst.size).unwrap_or(0);
                let res = match inst.opcode {
                    Opcode::And => lhs & rhs,
                    Opcode::Or => lhs | rhs,
                    Opcode::Xor => lhs ^ rhs,
                    Opcode::Not => !lhs,
                    Opcode::Neg => (0u64).wrapping_sub(lhs),
                    _ => unreachable!(),
                };
                self.regs[dest as usize] = self.mask_to_size(res, inst.size);
                self.pc += 1;
            }
            Opcode::Sxt | Opcode::Zxt => {
                let dest = self.expect_reg(inst.dest.as_ref())?;
                let val = self.read_operand(inst.src.as_ref(), inst.size)?;
                let res = match inst.opcode {
                    Opcode::Zxt => self.mask_to_size(val, inst.size),
                    Opcode::Sxt => self.sign_extend(val, inst.size),
                    _ => unreachable!(),
                };
                self.regs[dest as usize] = res;
                self.pc += 1;
            }
            Opcode::Push => {
                let val = self.read_operand(inst.src.as_ref().or(inst.dest.as_ref()), inst.size)?;
                let bytes = self.size_bytes(inst.size)?;
                let sp_idx = REG_SP.to_u8() as usize;
                let sp = self.regs[sp_idx].saturating_sub(bytes as u64);
                self.write_mem(sp, val, bytes)?;
                self.regs[sp_idx] = sp;
                self.pc += 1;
            }
            Opcode::Pop => {
                let dest = self.expect_reg(inst.dest.as_ref().or(inst.src.as_ref()))?;
                let bytes = self.size_bytes(inst.size)?;
                let sp_idx = REG_SP.to_u8() as usize;
                let sp = self.regs[sp_idx];
                let val = self.read_mem(sp, bytes)?;
                self.regs[dest as usize] = val;
                self.regs[sp_idx] = sp.saturating_add(bytes as u64);
                self.pc += 1;
            }
            Opcode::Load => {
                let dest = self.expect_reg(inst.dest.as_ref())?;
                let addr = self.resolve_address(inst.src.as_ref(), inst.size)?;
                let bytes = self.size_bytes(inst.size)?;
                let val = self.read_mem(addr, bytes)?;
                self.regs[dest as usize] = val;
                self.pc += 1;
            }
            Opcode::Store => {
                let src = self.read_operand(inst.src.as_ref(), inst.size)?;
                let addr = self.resolve_address(inst.dest.as_ref(), inst.size)?;
                let bytes = self.size_bytes(inst.size)?;
                self.write_mem(addr, src, bytes)?;
                self.pc += 1;
            }
            Opcode::Addi | Opcode::Subi | Opcode::Muli | Opcode::Remi => {
                let dest = self.expect_reg(inst.dest.as_ref())?;
                let lhs = self.regs[dest as usize];
                let rhs = self.read_operand(inst.src.as_ref(), inst.size)?;
                let res = match inst.opcode {
                    Opcode::Addi => lhs.wrapping_add(rhs),
                    Opcode::Subi => lhs.wrapping_sub(rhs),
                    Opcode::Muli => lhs.wrapping_mul(rhs),
                    Opcode::Remi => {
                        let r = if rhs == 0 { return Err(CpuError::DivByZero); } else { rhs };
                        (lhs as i64).checked_rem(r as i64).ok_or(CpuError::DivByZero)? as u64
                    }
                    _ => unreachable!(),
                };
                self.regs[dest as usize] = self.mask_to_size(res, inst.size);
                self.pc += 1;
            }
            Opcode::Ret => {
                // Pop return address from stack and jump to it
                let sp_idx = REG_SP.to_u8() as usize;
                let sp = self.regs[sp_idx];
                let ret_addr = self.read_mem(sp, 8)?;
                self.regs[sp_idx] = sp.saturating_add(8);
                self.pc = ret_addr as usize;
            }
            Opcode::Jmps => {
                // PC-relative jump with signed offset
                let offset = self.read_operand(inst.dest.as_ref().or(inst.src.as_ref()), inst.size)? as i64;
                self.pc = (self.pc as i64 + offset) as usize;
            }
            Opcode::Jmp | Opcode::Call | Opcode::Jmpi | Opcode::Calli => {
                let target = match inst.opcode {
                    Opcode::Jmp | Opcode::Call => {
                        self.read_operand(inst.dest.as_ref().or(inst.src.as_ref()), inst.size)?
                    }
                    _ => self.resolve_address(inst.dest.as_ref().or(inst.src.as_ref()), inst.size)?,
                };
                if matches!(inst.opcode, Opcode::Call | Opcode::Calli) {
                    let ret_addr = (self.pc + 1) as u64;
                    let sp_idx = REG_SP.to_u8() as usize;
                    let sp = self.regs[sp_idx].saturating_sub(8);
                    self.write_mem(sp, ret_addr, 8)?;
                    self.regs[sp_idx] = sp;
                }
                self.pc = target as usize;
            }
            Opcode::BrEq | Opcode::BrNe | Opcode::BrLt | Opcode::BrGe | Opcode::BrLts | Opcode::BrGes | Opcode::BrZ | Opcode::BrNz => {
                let lhs = self.read_operand(inst.dest.as_ref(), inst.size)?;
                let rhs = self.read_operand(inst.src.as_ref(), inst.size).unwrap_or(0);
                let take = match inst.opcode {
                    Opcode::BrEq => lhs == rhs,
                    Opcode::BrNe => lhs != rhs,
                    Opcode::BrLt => lhs < rhs,
                    Opcode::BrGe => lhs >= rhs,
                    Opcode::BrLts => (lhs as i64) < (rhs as i64),
                    Opcode::BrGes => (lhs as i64) >= (rhs as i64),
                    Opcode::BrZ => lhs == 0,
                    Opcode::BrNz => lhs != 0,
                    _ => false,
                };
                if take {
                    // branch target expected in src if present; otherwise immediate in dest for BrZ/BrNz
                    let target = self.read_operand(inst.src.as_ref().or(inst.dest.as_ref()), inst.size)?;
                    self.pc = target as usize;
                } else {
                    self.pc += 1;
                }
            }
            Opcode::Shl | Opcode::Shr | Opcode::Rol | Opcode::Ror => {
                let dest = self.expect_reg(inst.dest.as_ref())?;
                let count = self.read_operand(inst.src.as_ref(), inst.size)? as u32;
                let width = self.size_bits(inst.size)?;
                let mask = if width == 64 { u64::MAX } else { (1u64 << width) - 1 };
                let val = self.regs[dest as usize] & mask;
                let res = match inst.opcode {
                    Opcode::Shl => (val << count) & mask,
                    Opcode::Shr => (val >> count) & mask,
                    Opcode::Rol => ((val << (count % width)) | (val >> (width - (count % width)))) & mask,
                    Opcode::Ror => ((val >> (count % width)) | (val << (width - (count % width)))) & mask,
                    _ => unreachable!(),
                };
                self.regs[dest as usize] = res;
                self.pc += 1;
            }
            _ => return Err(CpuError::UnsupportedOpcode),
        }
        Ok(())
    }

    fn expect_reg(&self, op: Option<&Operand>) -> Result<u8, CpuError> {
        match op {
            Some(&Operand::Reg(r)) => Ok(r.to_u8()),
            Some(&Operand::Label(_)) => Err(CpuError::InvalidOperand),
            _ => Err(CpuError::MissingOperand),
        }
    }

    fn read_operand(&self, op: Option<&Operand>, size: Option<Size>) -> Result<u64, CpuError> {
        match op {
            Some(&Operand::Reg(r)) => Ok(self.regs[r.to_u8() as usize]),
            Some(&Operand::Imm(v)) => Ok(v as u64),
            Some(&Operand::Ea { reg, ea, disp }) => {
                let addr = self.resolve_ea(ea, reg, disp)?;
                let bytes = self.size_bytes(size)?;
                self.read_mem(addr, bytes)
            }
            Some(&Operand::Label(_)) => Err(CpuError::InvalidOperand),
            None => Err(CpuError::MissingOperand),
        }
    }

    fn resolve_address(&self, op: Option<&Operand>, _size: Option<Size>) -> Result<u64, CpuError> {
        match op {
            Some(&Operand::Ea { reg, ea, disp }) => self.resolve_ea(ea, reg, disp),
            Some(&Operand::Imm(v)) => Ok(v as u64),
            Some(&Operand::Reg(r)) => Ok(self.regs[r.to_u8() as usize]),
            Some(&Operand::Label(_)) => Err(CpuError::InvalidOperand),
            None => Err(CpuError::MissingOperand),
        }
    }

    fn resolve_ea(&self, ea: EffectiveAddress, reg: Reg, disp: Option<i32>) -> Result<u64, CpuError> {
        let base = self.regs[reg.to_u8() as usize];
        let displacement = disp.unwrap_or(0) as i64;
        match ea {
            EffectiveAddress::RegIndirect => Ok(base),
            EffectiveAddress::BaseDisp => Ok(base.wrapping_add(displacement as u64)),
            EffectiveAddress::Scaled => Err(CpuError::UnsupportedEa),
            EffectiveAddress::Immediate => Err(CpuError::UnsupportedEa),
        }
    }

    fn size_bytes(&self, size: Option<Size>) -> Result<usize, CpuError> {
        match size {
            Some(Size::Byte) => Ok(1),
            Some(Size::Short) => Ok(2),
            Some(Size::Long) => Ok(4),
            Some(Size::Word) => Ok(8),
            None => Err(CpuError::MissingOperand),
        }
    }

    fn size_bits(&self, size: Option<Size>) -> Result<u32, CpuError> {
        Ok((self.size_bytes(size)? as u32) * 8)
    }

    fn mask_to_size(&self, val: u64, size: Option<Size>) -> u64 {
        match size {
            Some(Size::Byte) => val & 0xFF,
            Some(Size::Short) => val & 0xFFFF,
            Some(Size::Long) => val & 0xFFFF_FFFF,
            Some(Size::Word) => val,
            None => val,
        }
    }

    fn sign_extend(&self, val: u64, size: Option<Size>) -> u64 {
        match size {
            Some(Size::Byte) => (val as i8) as i64 as u64,
            Some(Size::Short) => (val as i16) as i64 as u64,
            Some(Size::Long) => (val as i32) as i64 as u64,
            Some(Size::Word) | None => val,
        }
    }

    fn read_mem(&self, addr: u64, bytes: usize) -> Result<u64, CpuError> {
        let addr = addr as usize;
        if addr + bytes > self.mem.len() {
            return Err(CpuError::PcOob);
        }
        let mut v = 0u64;
        for i in 0..bytes {
            v |= (self.mem[addr + i] as u64) << (8 * i);
        }
        Ok(v)
    }

    fn write_mem(&mut self, addr: u64, val: u64, bytes: usize) -> Result<(), CpuError> {
        let addr = addr as usize;
        if addr + bytes > self.mem.len() {
            return Err(CpuError::PcOob);
        }
        for i in 0..bytes {
            self.mem[addr + i] = ((val >> (8 * i)) & 0xFF) as u8;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::portable::instruction::{Size, Instruction, Operand};

    #[test]
    fn addi_executes() {
        let program = vec![Instruction {
            opcode: Opcode::Addi,
            size: Some(Size::Long),
            dest: Some(Operand::Reg(Reg::R1)),
            src: Some(Operand::Imm(5)),
        }];
        let mut cpu = Cpu::new(1024);
        cpu.run(&program).unwrap();
        assert_eq!(cpu.regs[1], 5);
    }
}
