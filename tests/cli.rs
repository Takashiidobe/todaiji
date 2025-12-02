use datatest_stable as datatest;
use std::{
    ffi::OsStr,
    path::Path,
    process::{Command, Stdio},
};
use tempfile::Builder;

#[derive(Debug, Clone, PartialEq, Eq)]
struct RunLog {
    stdout: String,
    stderr: String,
    status: i32,
}

fn to_runlog(out: std::process::Output) -> RunLog {
    RunLog {
        stdout: String::from_utf8_lossy(&out.stdout).trim_end().to_string(),
        stderr: String::from_utf8_lossy(&out.stderr).trim_end().to_string(),
        status: out.status.code().unwrap_or(-1),
    }
}

fn run_todaiji<I, S>(args: I) -> datatest::Result<RunLog>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let out = Command::new(assert_cmd::cargo::cargo_bin!("todaiji"))
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;
    Ok(to_runlog(out))
}

fn encode_program(asm: &Path, out_path: &Path) -> datatest::Result<()> {
    let out = Command::new(assert_cmd::cargo::cargo_bin!("todaiji"))
        .args([
            "-e",
            asm.to_str().unwrap(),
            "-o",
            out_path.to_str().unwrap(),
        ])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;
    if !out.status.success() {
        return Err(format!(
            "encoding {} failed: status {:?}\nstdout: {}\nstderr: {}",
            asm.display(),
            out.status.code(),
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        )
        .into());
    }
    Ok(())
}

fn run_case(path: &Path) -> datatest::Result<()> {
    // Skip helper-only files that are meant to be included, not executed directly.
    if path.file_name().and_then(|s| s.to_str()) == Some("print.asm") {
        return Ok(());
    }

    let asm = path.canonicalize()?;

    // Run the interpreter directly on the asm source.
    let interp = run_todaiji([asm.as_os_str()])?;

    // Encode to a temp .tji, then run that binary.
    let tmp = Builder::new().suffix(".tji").tempfile()?;
    encode_program(&asm, tmp.path())?;
    let encoded = run_todaiji([tmp.path().as_os_str()])?;

    if interp != encoded {
        let mut msg = String::new();
        use std::fmt::Write;
        writeln!(&mut msg, "\n=== mismatch for {} ===", path.display()).ok();
        if interp.status != encoded.status {
            writeln!(
                &mut msg,
                "Exit code differs: interp={} encoded={}",
                interp.status, encoded.status
            )
            .ok();
        }
        if interp.stdout != encoded.stdout {
            writeln!(&mut msg, "\n--- stdout (interp) ---\n{}", interp.stdout).ok();
            writeln!(&mut msg, "\n--- stdout (encoded) ---\n{}", encoded.stdout).ok();
        }
        if interp.stderr != encoded.stderr {
            writeln!(&mut msg, "\n--- stderr (interp) ---\n{}", interp.stderr).ok();
            writeln!(&mut msg, "\n--- stderr (encoded) ---\n{}", encoded.stderr).ok();
        }
        return Err(msg.into());
    }

    Ok(())
}

fn run_pagoda_case(path: &Path) -> datatest::Result<()> {
    // Skip module files that are meant to be imported, not executed directly.
    // These are modules without a main function.
    let filename = path.file_name().and_then(|s| s.to_str()).unwrap_or("");

    // Skip known library modules
    if matches!(
        filename,
        "math.pag"
            | "vec2.pag"
            | "shapes.pag"
            | "utils.pag"
            | "graphics.pag"
            | "ui.pag"
            | "circular_a.pag"
            | "circular_b.pag"
            | "level1.pag"
            | "level2.pag"
            | "level3.pag"
            | "base.pag"
            | "left.pag"
            | "right.pag"
    ) {
        return Ok(());
    }

    // Skip test cases that are expected to fail (circular dependencies, etc.)
    if filename == "circular_test.pag" {
        return Ok(());
    }

    let pag = path.canonicalize()?;

    // Run the pagoda program with -pr (compile and run)
    let result = run_todaiji(["-pr", pag.to_str().unwrap()])?;

    // Check if there's an expected exit code file
    let expected_file = path.with_extension("expected");
    if expected_file.exists() {
        let expected_content = std::fs::read_to_string(&expected_file)?;
        let lines: Vec<&str> = expected_content.lines().collect();

        for line in lines {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            if let Some(code_str) = line.strip_prefix("exit_code:") {
                let expected_code: i32 = code_str.trim().parse().map_err(|e| {
                    format!("Invalid exit code in {}: {}", expected_file.display(), e)
                })?;

                if result.status != expected_code {
                    return Err(format!(
                        "\n=== Exit code mismatch for {} ===\nExpected: {}\nGot: {}\nStderr: {}",
                        path.display(),
                        expected_code,
                        result.status,
                        result.stderr
                    )
                    .into());
                }
            }
        }
    } else {
        // No expected file - just check that it didn't fail with an error
        if result.status == 1 && !result.stderr.is_empty() {
            return Err(format!(
                "\n=== Execution failed for {} ===\nExit code: {}\nStderr: {}",
                path.display(),
                result.status,
                result.stderr
            )
            .into());
        }
    }

    Ok(())
}

datatest::harness! {
    { test = run_case, root = "./examples", pattern = r#"^.*\.asm$"# },
    { test = run_pagoda_case, root = "./examples", pattern = r#"^.*\.pag$"# },
}
