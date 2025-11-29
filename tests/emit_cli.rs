use std::{
    env, fs,
    path::PathBuf,
    process::Command,
    time::{SystemTime, UNIX_EPOCH},
};

use todaiji::portable::decode_program;

#[test]
fn emit_flag_writes_binary_file() {
    let bin = find_binary();
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR missing");

    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time went backwards")
        .as_nanos();
    let out_path = env::temp_dir().join(format!("todaiji_emit_test_{timestamp}.tji"));

    let status = Command::new(&bin)
        .current_dir(manifest_dir)
        .args([
            "-e",
            "examples/fib.asm",
            "-o",
            out_path.to_str().expect("non-utf8 path"),
        ])
        .status()
        .expect("failed to execute binary");

    assert!(status.success(), "emit command failed");

    let bytes = fs::read(&out_path).expect("failed to read emitted file");
    let program = decode_program(&bytes).expect("decode failed");
    assert!(
        !program.instructions.is_empty(),
        "emitted program should not be empty"
    );

    let _ = fs::remove_file(out_path);
}

fn find_binary() -> PathBuf {
    if let Some(p) = option_env!("CARGO_BIN_EXE_todaiji") {
        return PathBuf::from(p);
    }

    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR missing");
    let status = Command::new("cargo")
        .current_dir(&manifest_dir)
        .args(["build", "--bin", "todaiji"])
        .status()
        .expect("failed to build binary");
    assert!(status.success(), "cargo build --bin todaiji failed");

    let mut path = PathBuf::from(manifest_dir).join("target/debug/todaiji");
    if cfg!(windows) {
        path.set_extension("exe");
    }
    path
}
