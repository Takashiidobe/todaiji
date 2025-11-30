use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use todaiji::{
    pagoda::{bytecode::emit_exit_program, parse_source},
    portable::{cpu::Cpu, decode_program, encode_program, parse_program_from_path},
};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print_usage(&args[0]);
        std::process::exit(1);
    }

    let mut emit_input: Option<String> = None;
    let mut emit_output: Option<String> = None;
    let mut dump_input: Option<String> = None;
    let mut run_input: Option<String> = None;
    let mut pagoda_input: Option<String> = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-e" | "--emit" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Missing argument after {}", args[i - 1]);
                    std::process::exit(1);
                }
                emit_input = Some(args[i].clone());
            }
            "-o" | "--output" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Missing argument after {}", args[i - 1]);
                    std::process::exit(1);
                }
                emit_output = Some(args[i].clone());
            }
            "-d" | "--dump" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Missing argument after {}", args[i - 1]);
                    std::process::exit(1);
                }
                dump_input = Some(args[i].clone());
            }
            "-p" | "--pagoda" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Missing argument after {}", args[i - 1]);
                    std::process::exit(1);
                }
                pagoda_input = Some(args[i].clone());
            }
            arg => {
                // First positional is the program to run (backwards-compatible)
                if run_input.is_none() {
                    run_input = Some(arg.to_string());
                }
            }
        }
        i += 1;
    }

    if let Some(path) = pagoda_input {
        if let Err(e) = emit_pagoda(&path) {
            eprintln!("{e}");
            std::process::exit(1);
        }
        return;
    }

    if let Some(input) = emit_input {
        let output_path = emit_output
            .map(PathBuf::from)
            .unwrap_or_else(|| default_output_path(&input));
        if let Err(e) = emit_binary(&input, &output_path) {
            eprintln!("{e}");
            std::process::exit(1);
        }
        println!("Wrote {}", output_path.display());
        return;
    }

    if let Some(input) = dump_input {
        if let Err(e) = dump_listing(&input) {
            eprintln!("{e}");
            std::process::exit(1);
        }
        return;
    }

    let Some(main_file) = run_input else {
        print_usage(&args[0]);
        std::process::exit(1);
    };

    let file_path = Path::new(&main_file);
    let extension = file_path.extension().and_then(|s| s.to_str()).unwrap_or("");

    // Determine file type and load program
    let program = match extension.to_lowercase().as_str() {
        "tji" => {
            // Binary bytecode file
            let bytes = match fs::read(&args[1]) {
                Ok(b) => b,
                Err(e) => {
                    eprintln!("Failed to read {}: {e}", args[1]);
                    std::process::exit(1);
                }
            };

            match decode_program(&bytes) {
                Ok(p) => p,
                Err(e) => {
                    eprintln!("Decode error: {e:?}");
                    std::process::exit(1);
                }
            }
        }
        "asm" | "s" => {
            // Textual assembly file
            match parse_program_from_path(file_path) {
                Ok(p) => p,
                Err(e) => {
                    eprintln!("Parse error: {e}");
                    std::process::exit(1);
                }
            }
        }
        _ => {
            eprintln!("Unknown file extension '.{}'", extension);
            eprintln!("Supported: .asm, .s, .S, .ASM (text) or .tji (binary)");
            std::process::exit(1);
        }
    };

    let mut cpu = Cpu::new(64 * 1024);
    for seg in &program.data {
        let end = seg.offset + seg.bytes.len();
        if end > cpu.mem.len() {
            eprintln!("Data segment out of memory bounds");
            std::process::exit(1);
        }
        cpu.mem[seg.offset..end].copy_from_slice(&seg.bytes);
    }
    if let Err(e) = cpu.run(&program) {
        eprintln!("Execution error: {e:?}");
        std::process::exit(1);
    }

    // if debugging
    // println!("Execution finished. Registers:");
    // for (i, v) in cpu.regs.iter().enumerate() {
    //     println!("r{i:02}: 0x{v:016x}");
    // }
}

fn emit_binary(input: &str, output: &Path) -> Result<(), String> {
    let program =
        parse_program_from_path(input).map_err(|e| format!("Parse error in {input}: {e}"))?;
    let bytes = encode_program(&program).map_err(|e| format!("Encode error: {e:?}"))?;
    fs::write(output, bytes).map_err(|e| format!("Failed to write {}: {e}", output.display()))
}

fn default_output_path(input: &str) -> PathBuf {
    let mut path = PathBuf::from(input);
    path.set_extension("tji");
    path
}

fn print_usage(bin: &str) {
    eprintln!("Usage:");
    eprintln!("  {bin} <file.asm|file.tji>        # Run a program");
    eprintln!("  {bin} -e <file.asm> [-o out.tji] # Emit binary bytecode");
    eprintln!("  {bin} -d <file.asm|file.tji>     # Dump decoded instructions");
    eprintln!("  {bin} -p <file.pagoda>          # Compile Pagoda source to assembly");
    eprintln!("    .asm/.s/.S/ASM: Textual assembly file");
    eprintln!("    .tji: Binary bytecode file");
}

fn dump_listing(path: &str) -> Result<(), String> {
    let is_tji = Path::new(path)
        .extension()
        .and_then(|s| s.to_str())
        .map(|s| s.eq_ignore_ascii_case("tji"))
        .unwrap_or(false);

    if is_tji {
        let bytes = fs::read(path).map_err(|e| format!("Failed to read {path}: {e}"))?;
        let program = decode_program(&bytes).map_err(|e| format!("Decode error: {e:?}"))?;
        if !program.data.is_empty() {
            for seg in &program.data {
                println!("DATA @{}: {:?}", seg.offset, seg.bytes);
            }
        }
        for (i, inst) in program.instructions.iter().enumerate() {
            println!("{i:04}: {:?}", inst);
        }
    } else {
        let program = parse_program_from_path(path).map_err(|e| format!("Parse error: {e}"))?;
        if !program.data.is_empty() {
            for seg in &program.data {
                println!("DATA @{}: {:?}", seg.offset, seg.bytes);
            }
        }
        for (i, inst) in program.instructions.iter().enumerate() {
            println!("{i:04}: {:?}", inst);
        }
    }

    Ok(())
}

fn emit_pagoda(path: &str) -> Result<(), String> {
    let source =
        fs::read_to_string(path).map_err(|e| format!("Failed to read {path}: {e}"))?;
    let program =
        parse_source(&source).map_err(|e| todaiji::pagoda::format_error(&source, &e))?;
    emit_exit_program(&program, std::io::stdout())
        .map_err(|e| todaiji::pagoda::format_error(&source, &e.into()))
}
