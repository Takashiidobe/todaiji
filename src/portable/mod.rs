pub mod decode;
pub mod display;
pub mod cpu;
pub mod instruction;

pub use decode::{
    decode, decode_program, decode_program_bytes, encode, encode_program, encode_program_bytes,
    PortableError,
};
pub use instruction::{
    assemble_program, parse_asm_line, parse_program, DataSegment, EffectiveAddress, Instruction,
    Opcode, Operand, Program, Reg, Size,
};
