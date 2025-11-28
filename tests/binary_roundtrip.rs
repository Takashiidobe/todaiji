use std::{
    fs,
    time::{SystemTime, UNIX_EPOCH},
};
use todaiji::portable::{cpu::Cpu, decode_program, encode_program_bytes, parse_program};

#[test]
fn test_binary_roundtrip_simple() {
    // Simple test program: sets r0 = 42, r1 = 100, then adds them
    let asm_src = r#"
        mov.l %r0, $42
        mov.l %r1, $100
        add.l %r0, %r1
    "#;

    // Parse assembly
    let program_from_asm = parse_program(asm_src).expect("Failed to parse assembly");
    assert_eq!(program_from_asm.instructions.len(), 3);

    // Encode to binary
    let binary = encode_program_bytes(&program_from_asm.instructions).expect("Failed to encode to binary");
    assert!(!binary.is_empty());
    assert_eq!(binary.len() % 2, 0, "Binary should be even number of bytes");

    // Decode from binary
    let program_from_binary = decode_program(&binary).expect("Failed to decode from binary");
    assert_eq!(program_from_binary.instructions.len(), 3);

    // Execute assembly version
    let mut cpu_asm = Cpu::new(1024);
    cpu_asm.run(&program_from_asm.instructions).expect("Failed to execute assembly");

    // Execute binary version
    let mut cpu_bin = Cpu::new(1024);
    cpu_bin
        .run(&program_from_binary.instructions)
        .expect("Failed to execute binary");

    // Compare results
    assert_eq!(cpu_asm.regs[0], 142, "r0 should be 142 (42 + 100)");
    assert_eq!(cpu_asm.regs[1], 100, "r1 should be 100");
    assert_eq!(
        cpu_asm.regs, cpu_bin.regs,
        "Assembly and binary execution should produce identical register states"
    );
}

#[test]
fn test_binary_roundtrip_with_memory() {
    // Test with memory operations: mov with EA (Load/Store)
    let asm_src = r#"
        mov.l %r0, $42
        mov.b %r1, 4(%r2)
    "#;

    let program_from_asm = parse_program(asm_src).expect("Failed to parse assembly");
    let binary = encode_program_bytes(&program_from_asm.instructions).expect("Failed to encode to binary");
    let program_from_binary = decode_program(&binary).expect("Failed to decode from binary");

    assert_eq!(
        program_from_asm.instructions.len(),
        program_from_binary.instructions.len()
    );

    // Both should execute without error (even though r2 might be 0)
    let mut cpu_asm = Cpu::new(1024);
    let mut cpu_bin = Cpu::new(1024);

    // We expect this might fail if r2 is uninitialized, but both should fail the same way
    let result_asm = cpu_asm.run(&program_from_asm.instructions);
    let result_bin = cpu_bin.run(&program_from_binary.instructions);

    // Either both succeed or both fail
    assert_eq!(
        result_asm.is_ok(),
        result_bin.is_ok(),
        "Assembly and binary should both succeed or both fail"
    );

    if result_asm.is_ok() {
        assert_eq!(cpu_asm.regs, cpu_bin.regs);
    }
}

#[test]
fn test_binary_roundtrip_jmps() {
    // Test with jmps instruction
    let asm_src = r#"
        mov.l %r0, $0
loop:
        addi.l %r0, $1
        jmps loop
    "#;

    let program_from_asm = parse_program(asm_src).expect("Failed to parse assembly");
    let binary = encode_program_bytes(&program_from_asm.instructions).expect("Failed to encode to binary");
    let program_from_binary = decode_program(&binary).expect("Failed to decode from binary");

    assert_eq!(
        program_from_asm.instructions.len(),
        program_from_binary.instructions.len()
    );

    // Note: This creates an infinite loop, so we can't actually execute it,
    // but we can verify the encoding/decoding works
}

#[test]
fn test_binary_encoding_formats() {
    // Test various instruction formats to ensure they all encode/decode correctly
    let test_cases = vec![
        // ALU reg-reg
        ("add.l %r0, %r1", 1),
        ("sub.w %r2, %r3", 1),
        // ALU immediate
        ("addi.l %r0, $10", 1),
        ("subi.l %r1, $-5", 1),
        // Move register-to-register
        ("mov.l %r0, %r1", 1),
        // Logic operations
        ("and.l %r0, %r1", 1),
        ("or.l %r2, %r3", 1),
        ("xor.l %r4, %r5", 1),
    ];

    for (asm, expected_len) in test_cases {
        let program = parse_program(asm).expect(&format!("Failed to parse: {}", asm));
        assert_eq!(program.instructions.len(), expected_len, "Wrong length for: {}", asm);

        let binary = encode_program_bytes(&program.instructions).expect(&format!("Failed to encode: {}", asm));
        let decoded = decode_program(&binary).expect(&format!("Failed to decode: {}", asm));

        assert_eq!(
            program.instructions.len(),
            decoded.instructions.len(),
            "Length mismatch for: {}",
            asm
        );
    }
}

#[test]
fn test_binary_roundtrip_call_and_jmp() {
    // Ensure call/jmp extension words are preserved
    let asm_src = r#"
        call func
        jmp end
func:
        mov.l %r0, $42
        ret
end:
        nop
    "#;

    let program_from_asm = parse_program(asm_src).expect("Failed to parse assembly");
    let binary =
        encode_program_bytes(&program_from_asm.instructions).expect("Failed to encode to binary");
    let program_from_binary = decode_program(&binary).expect("Failed to decode from binary");

    let mut cpu_asm = Cpu::new(1024);
    cpu_asm
        .run(&program_from_asm.instructions)
        .expect("asm run failed");

    let mut cpu_bin = Cpu::new(1024);
    cpu_bin
        .run(&program_from_binary.instructions)
        .expect("bin run failed");

    assert_eq!(cpu_asm.regs[0], 42);
    assert_eq!(cpu_asm.regs, cpu_bin.regs);
}

#[test]
fn test_asm_binary_file_executes() {
    let asm_src = r#"
        mov.l %r0, $5
        mov.l %r1, $7
        add.l %r0, %r1
        mov.w %r2, 8(%r3)
    "#;

    let program_from_asm = parse_program(asm_src).expect("Failed to parse assembly");
    let binary =
        encode_program_bytes(&program_from_asm.instructions).expect("Failed to encode to binary");

    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time went backwards")
        .as_nanos();
    let tmp_path = std::env::temp_dir().join(format!("todaiji_test_{unique}.tji"));
    fs::write(&tmp_path, &binary).expect("Failed to write binary to file");

    let bytes_from_disk = fs::read(&tmp_path).expect("Failed to read binary file");
    let program_from_binary = decode_program(&bytes_from_disk).expect("Failed to decode binary");
    let _ = fs::remove_file(&tmp_path);

    let mut cpu_text = Cpu::new(1024);
    cpu_text.run(&program_from_asm.instructions)
        .expect("Execution from asm should succeed");

    let mut cpu_bin = Cpu::new(1024);
    cpu_bin
        .run(&program_from_binary.instructions)
        .expect("Execution from binary should succeed");

    assert_eq!(
        cpu_text.regs, cpu_bin.regs,
        "Binary file execution should match text assembly execution"
    );
}
