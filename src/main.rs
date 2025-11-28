use std::env;
use std::fs;

use todaiji::portable::{cpu::Cpu, parse_program};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <asm-file>", args[0]);
        std::process::exit(1);
    }
    let input = match fs::read_to_string(&args[1]) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to read {}: {e}", args[1]);
            std::process::exit(1);
        }
    };

    let program = match parse_program(&input) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parse error: {e}");
            std::process::exit(1);
        }
    };

    let mut cpu = Cpu::new(64 * 1024);
    if let Err(e) = cpu.run(&program) {
        eprintln!("Execution error: {e:?}");
        std::process::exit(1);
    }

    println!("Execution finished. Registers:");
    for (i, v) in cpu.regs.iter().enumerate() {
        println!("r{i:02}: 0x{v:016x}");
    }
}
