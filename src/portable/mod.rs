pub mod decode;
pub mod display;
pub mod cpu;
pub mod instruction;

pub use decode::{decode, encode, PortableError};
pub use instruction::{
    assemble_program, parse_asm_line, parse_program, EffectiveAddress, Instruction, Opcode,
    Operand, Reg, Size,
};
