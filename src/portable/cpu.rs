use std::io::{self, Write};

use crate::portable::instruction::{
    DataSegment, EffectiveAddress, Instruction, Opcode, Operand, Program, REG_PC, REG_SP, Reg, Size,
};

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
    #[error("unsupported syscall {0}")]
    UnsupportedSyscall(u64),
    #[error("io error")]
    IoError,
}

/// Simple in-memory simulator for the portable ISA.
pub struct Cpu {
    pub regs: [u64; 16],
    pub mem: Vec<u8>,
    pub pc: usize,
    output: Box<dyn Write>,
}

impl Cpu {
    pub fn new(mem_size: usize) -> Self {
        Self::with_writer(mem_size, Box::new(io::stdout()))
    }

    pub fn with_writer(mem_size: usize, writer: Box<dyn Write>) -> Self {
        let mut cpu = Cpu {
            regs: [0; 16],
            mem: vec![0; mem_size],
            pc: 0,
            output: writer,
        };
        cpu.regs[REG_SP.to_u8() as usize] = mem_size as u64;
        cpu
    }

    /// Execute the program starting at pc until it falls off the end.
    pub fn run(&mut self, program: &Program) -> Result<(), CpuError> {
        // Precompute byte offsets for each instruction using encoder word lengths,
        // accounting for interleaved data segments.
        let mut offsets = Vec::with_capacity(program.instructions.len());
        let mut current = 0usize;
        let mut data_segments = program.data.clone();
        data_segments.sort_by_key(|d| d.offset);
        let mut data_idx = 0usize;

        let consume_data = |current: &mut usize, data_idx: &mut usize, data: &[DataSegment]| {
            while let Some(seg) = data.get(*data_idx) {
                if seg.offset < *current {
                    return Err(CpuError::PcOob);
                } else if seg.offset == *current {
                    *current = current.saturating_add(seg.bytes.len());
                    *data_idx += 1;
                } else {
                    break;
                }
            }
            Ok(())
        };

        consume_data(&mut current, &mut data_idx, &data_segments)?;

        for inst in &program.instructions {
            offsets.push(current);
            let words =
                crate::portable::decode::encode(inst).map_err(|_| CpuError::UnsupportedOpcode)?;
            current += words.len() * 2;
            consume_data(&mut current, &mut data_idx, &data_segments)?;
        }

        let mut end_offset = current;
        for seg in data_segments.iter().skip(data_idx) {
            if seg.offset < end_offset {
                return Err(CpuError::PcOob);
            }
            end_offset = seg.offset.saturating_add(seg.bytes.len());
        }

        while self.pc < program.instructions.len() {
            if let Some(off) = offsets.get(self.pc) {
                self.regs[REG_PC.to_u8() as usize] = *off as u64;
            }
            let inst = &program.instructions[self.pc];
            if let Err(e) = self.step(inst, &offsets, end_offset) {
                if matches!(e, CpuError::PcOob)
                    && std::env::var("TODAIJI_DEBUG_PC").is_ok()
                {
                    eprintln!(
                        "PcOob at pc={}, inst={inst:?}, offsets={offsets:?}, end_offset={end_offset}",
                        self.pc
                    );
                }
                return Err(e);
            }
            // pc incremented inside step for branches/jumps
        }
        Ok(())
    }

    fn step(
        &mut self,
        inst: &Instruction,
        offsets: &[usize],
        end_offset: usize,
    ) -> Result<(), CpuError> {
        match inst.opcode {
            Opcode::Nop => {
                self.pc += 1;
            }
            Opcode::Trap => {
                let num = self.regs[0];
                match num {
                    1 => {
                        // write(fd, buf, count)
                        let fd = self.regs[1];
                        let ptr = self.regs[2] as usize;
                        let count = self.regs[3] as usize;
                        if fd != 1 && fd != 2 {
                            return Err(CpuError::UnsupportedSyscall(fd));
                        }
                        if ptr
                            .checked_add(count)
                            .map(|end| end <= self.mem.len())
                            .unwrap_or(false)
                        {
                            let bytes = &self.mem[ptr..ptr + count];
                            self.output
                                .write_all(bytes)
                                .map_err(|_| CpuError::IoError)?;
                            self.output.flush().map_err(|_| CpuError::IoError)?;
                        } else {
                            if std::env::var("TODAIJI_DEBUG_PC").is_ok() {
                                eprintln!(
                                    "Trap write oob: fd={fd}, ptr={ptr}, count={count}, mem_len={}",
                                    self.mem.len()
                                );
                            }
                            return Err(CpuError::PcOob);
                        }
                    }
                    60 => {
                        // exit(code)
                        let code = self.regs[1] as i32;
                        std::process::exit(code);
                    }
                    _ => return Err(CpuError::UnsupportedSyscall(num)),
                }
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
            Opcode::Add
            | Opcode::Sub
            | Opcode::Mul
            | Opcode::Div
            | Opcode::Divu
            | Opcode::Rem
            | Opcode::Remu => {
                let dest = self.expect_reg(inst.dest.as_ref())?;
                let lhs = self.regs[dest as usize];
                let rhs = self.read_operand(inst.src.as_ref(), inst.size)?;
                let res = match inst.opcode {
                    Opcode::Add => lhs.wrapping_add(rhs),
                    Opcode::Sub => lhs.wrapping_sub(rhs),
                    Opcode::Mul => lhs.wrapping_mul(rhs),
                    Opcode::Div => {
                        let r = if rhs == 0 {
                            return Err(CpuError::DivByZero);
                        } else {
                            rhs
                        };
                        (lhs as i64)
                            .checked_div(r as i64)
                            .ok_or(CpuError::DivByZero)? as u64
                    }
                    Opcode::Divu => {
                        let r = if rhs == 0 {
                            return Err(CpuError::DivByZero);
                        } else {
                            rhs
                        };
                        lhs / r
                    }
                    Opcode::Rem => {
                        let r = if rhs == 0 {
                            return Err(CpuError::DivByZero);
                        } else {
                            rhs
                        };
                        (lhs as i64)
                            .checked_rem(r as i64)
                            .ok_or(CpuError::DivByZero)? as u64
                    }
                    Opcode::Remu => {
                        let r = if rhs == 0 {
                            return Err(CpuError::DivByZero);
                        } else {
                            rhs
                        };
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
                if let Some(Operand::Imm(v)) = inst.src.as_ref() {
                    // Immediate addressing encodes constants for Load
                    let masked = self.mask_to_size(v.as_u64(), inst.size);
                    self.regs[dest as usize] = masked;
                    self.pc += 1;
                    return Ok(());
                }

                let addr = self.resolve_address(inst.src.as_ref(), inst.size)?;
                let bytes = self.size_bytes(inst.size)?;
                let val = self.read_mem(addr, bytes)?;
                self.regs[dest as usize] = self.mask_to_size(val, inst.size);
                self.pc += 1;
            }
            Opcode::Store => {
                // Store takes value first, address second.
                let src = self.read_operand(inst.dest.as_ref(), inst.size)?;
                let addr = self.resolve_address(inst.src.as_ref(), inst.size)?;
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
                        let r = if rhs == 0 {
                            return Err(CpuError::DivByZero);
                        } else {
                            rhs
                        };
                        (lhs as i64)
                            .checked_rem(r as i64)
                            .ok_or(CpuError::DivByZero)? as u64
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
                if sp as usize >= self.mem.len() {
                    // Empty stack: treat as returning past the end
                    self.pc = offsets.len();
                    return Ok(());
                }
                let ret_addr = self.read_mem(sp, 8)?;
                self.regs[sp_idx] = sp.saturating_add(8);
                self.pc = match self.offset_to_pc(ret_addr as usize, offsets, end_offset) {
                    Ok(idx) => idx,
                    Err(_) => offsets.len(), // treat unknown return as end of program
                };
            }
            Opcode::Jmps => {
                // PC-relative jump with signed offset
                let offset =
                    self.read_operand(inst.dest.as_ref().or(inst.src.as_ref()), inst.size)? as i64;
                let curr_off = *offsets.get(self.pc).ok_or(CpuError::PcOob)? as i64;
                let target_off = curr_off + offset;
                self.pc = self.offset_to_pc(target_off as usize, offsets, end_offset)?;
            }
            Opcode::Jmp | Opcode::Call | Opcode::Jmpi | Opcode::Calli => {
                let target = match inst.opcode {
                    Opcode::Jmp | Opcode::Call => {
                        self.read_operand(inst.dest.as_ref().or(inst.src.as_ref()), inst.size)?
                    }
                    _ => {
                        self.resolve_address(inst.dest.as_ref().or(inst.src.as_ref()), inst.size)?
                    }
                };
                if matches!(inst.opcode, Opcode::Call | Opcode::Calli) {
                    let ret_addr = *offsets.get(self.pc + 1).ok_or(CpuError::PcOob)? as u64;
                    let sp_idx = REG_SP.to_u8() as usize;
                    let sp = self.regs[sp_idx].saturating_sub(8);
                    self.write_mem(sp, ret_addr, 8)?;
                    self.regs[sp_idx] = sp;
                }
                self.pc = self.offset_to_pc(target as usize, offsets, end_offset)?;
            }
            Opcode::BrEq
            | Opcode::BrNe
            | Opcode::BrLt
            | Opcode::BrGe
            | Opcode::BrLts
            | Opcode::BrGes
            | Opcode::BrZ
            | Opcode::BrNz => {
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
                    let target =
                        self.read_operand(inst.src.as_ref().or(inst.dest.as_ref()), inst.size)?;
                    self.pc = self.offset_to_pc(target as usize, offsets, end_offset)?;
                } else {
                    self.pc += 1;
                }
            }
            Opcode::Shl | Opcode::Shr | Opcode::Rol | Opcode::Ror => {
                let dest = self.expect_reg(inst.dest.as_ref())?;
                let count = self.read_operand(inst.src.as_ref(), inst.size)? as u32;
                let width = self.size_bits(inst.size)?;
                let mask = if width == 64 {
                    u64::MAX
                } else {
                    (1u64 << width) - 1
                };
                let val = self.regs[dest as usize] & mask;
                let res = match inst.opcode {
                    Opcode::Shl => (val << count) & mask,
                    Opcode::Shr => (val >> count) & mask,
                    Opcode::Rol => {
                        ((val << (count % width)) | (val >> (width - (count % width)))) & mask
                    }
                    Opcode::Ror => {
                        ((val >> (count % width)) | (val << (width - (count % width)))) & mask
                    }
                    _ => unreachable!(),
                };
                self.regs[dest as usize] = res;
                self.pc += 1;
            }
            Opcode::Movi => {
                let dest = self.expect_reg(inst.dest.as_ref())?;
                let val = self.read_operand(inst.src.as_ref(), inst.size)?;
                self.regs[dest as usize] = val;
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
            Some(&Operand::Imm(v)) => Ok(v.as_u64()),
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
            Some(&Operand::Imm(v)) => Ok(v.as_u64()),
            Some(&Operand::Reg(r)) => Ok(self.regs[r.to_u8() as usize]),
            Some(&Operand::Label(_)) => Err(CpuError::InvalidOperand),
            None => Err(CpuError::MissingOperand),
        }
    }

    fn resolve_ea(
        &self,
        ea: EffectiveAddress,
        reg: Reg,
        disp: Option<i32>,
    ) -> Result<u64, CpuError> {
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

    fn offset_to_pc(
        &self,
        offset: usize,
        offsets: &[usize],
        end_offset: usize,
    ) -> Result<usize, CpuError> {
        if offset == end_offset {
            return Ok(offsets.len());
        }
        if let Some(idx) = offsets
            .iter()
            .position(|&off| off == offset)
            .or_else(|| offsets.iter().position(|&off| off > offset))
        {
            Ok(idx)
        } else {
            if std::env::var("TODAIJI_DEBUG_PC").is_ok() {
                eprintln!("offset_to_pc miss: target={offset}, end={end_offset}, offsets={offsets:?}");
            }
            Err(CpuError::PcOob)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::portable::instruction::{ImmediateValue, Instruction, Operand, Size};

    #[test]
    fn addi_executes() {
        let program = Program {
            instructions: vec![Instruction {
                opcode: Opcode::Addi,
                size: Some(Size::Long),
                dest: Some(Operand::Reg(Reg::R1)),
                src: Some(Operand::Imm(ImmediateValue::Long(5))),
            }],
            data: Vec::new(),
        };
        let mut cpu = Cpu::new(1024);
        cpu.run(&program).unwrap();
        assert_eq!(cpu.regs[1], 5);
    }

    #[test]
    fn trap_write_syscall() {
        use std::cell::RefCell;
        use std::rc::Rc;

        struct Capture(Rc<RefCell<Vec<u8>>>);
        impl std::io::Write for Capture {
            fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
                self.0.borrow_mut().extend_from_slice(buf);
                Ok(buf.len())
            }
            fn flush(&mut self) -> std::io::Result<()> {
                Ok(())
            }
        }

        let sink = Rc::new(RefCell::new(Vec::new()));
        let writer: Box<dyn std::io::Write> = Box::new(Capture(sink.clone()));

        // Program:
        // r0 = 1 (sys_write)
        // r1 = 1 (stdout)
        // r2 = buf ptr
        // r3 = len
        // trap
        let program = Program {
            instructions: vec![
                Instruction {
                    opcode: Opcode::Load,
                    size: Some(Size::Long),
                    dest: Some(Operand::Reg(Reg::R0)),
                    src: Some(Operand::Imm(ImmediateValue::Long(1))),
                },
                Instruction {
                    opcode: Opcode::Load,
                    size: Some(Size::Long),
                    dest: Some(Operand::Reg(Reg::R1)),
                    src: Some(Operand::Imm(ImmediateValue::Long(1))),
                },
                Instruction {
                    opcode: Opcode::Load,
                    size: Some(Size::Long),
                    dest: Some(Operand::Reg(Reg::R2)),
                    src: Some(Operand::Imm(ImmediateValue::Long(16))),
                },
                Instruction {
                    opcode: Opcode::Load,
                    size: Some(Size::Long),
                    dest: Some(Operand::Reg(Reg::R3)),
                    src: Some(Operand::Imm(ImmediateValue::Long(12))),
                },
                Instruction {
                    opcode: Opcode::Trap,
                    size: None,
                    dest: None,
                    src: None,
                },
            ],
            data: Vec::new(),
        };

        let mut cpu = Cpu::with_writer(128, writer);
        cpu.mem[16..28].copy_from_slice(b"hello world\n");
        cpu.run(&program).unwrap();

        assert_eq!(sink.borrow().as_slice(), b"hello world\n");
    }

    #[test]
    fn load_store_roundtrip_from_mov_lowering() {
        // Store a byte via Store, then load it back.
        let program = Program {
            instructions: vec![
                Instruction {
                    opcode: Opcode::Load,
                    size: Some(Size::Long),
                    dest: Some(Operand::Reg(Reg::R2)),
                    src: Some(Operand::Imm(ImmediateValue::Long(32))), // base addr
                },
                Instruction {
                    opcode: Opcode::Load,
                    size: Some(Size::Long),
                    dest: Some(Operand::Reg(Reg::R1)),
                    src: Some(Operand::Imm(ImmediateValue::Long(0xAA))),
                },
                Instruction {
                    // Store byte r1 -> [r2 + 4]
                    opcode: Opcode::Store,
                    size: Some(Size::Byte),
                    dest: Some(Operand::Reg(Reg::R1)),
                    src: Some(Operand::Ea {
                        reg: Reg::R2,
                        ea: EffectiveAddress::BaseDisp,
                        disp: Some(4),
                    }),
                },
                Instruction {
                    // Load byte back into r3
                    opcode: Opcode::Load,
                    size: Some(Size::Byte),
                    dest: Some(Operand::Reg(Reg::R3)),
                    src: Some(Operand::Ea {
                        reg: Reg::R2,
                        ea: EffectiveAddress::BaseDisp,
                        disp: Some(4),
                    }),
                },
            ],
            data: Vec::new(),
        };

        let mut cpu = Cpu::new(128);
        cpu.run(&program).unwrap();

        assert_eq!(cpu.regs[Reg::R3.to_u8() as usize], 0xAA);
        assert_eq!(cpu.mem[36], 0xAA); // 32 + 4
    }
}
