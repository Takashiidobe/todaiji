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

datatest::harness! {
    { test = run_case, root = "./examples", pattern = r#"^.*\.asm$"# },
}
