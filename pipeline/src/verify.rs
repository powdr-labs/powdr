use std::{path::Path, process::Command};

pub fn verify(temp_dir: &Path) -> Result<(), String> {
    let pilcom = std::env::var("PILCOM")
        .expect("Please set the PILCOM environment variable to the path to the pilcom repository.");

    let constants_file = format!("{}/constants_estark.bin", temp_dir.to_str().unwrap());
    let commits_file = format!("{}/commits.bin", temp_dir.to_str().unwrap());
    let constraints_file = format!("{}/constraints.json", temp_dir.to_str().unwrap());

    let verifier_output = Command::new("node")
        .args([
            "--max-old-space-size=8000".to_string(), // 8GB of memory
            format!("{pilcom}/src/main_pilverifier.js"),
            commits_file,
            "-j".to_string(),
            constraints_file,
            "-c".to_string(),
            constants_file,
        ])
        .output()
        .expect("failed to run pil verifier");

    let output = String::from_utf8_lossy(&verifier_output.stdout);
    let result = if !verifier_output.status.success() {
        Err("Pil verifier run was unsuccessful.".to_string())
    } else if !output.trim().ends_with("PIL OK!!") {
        Err("Verifier did not say 'PIL OK'.".to_string())
    } else {
        Ok(())
    };

    if result.is_err() {
        log::error!(
            "Pil verifier run was unsuccessful.\nStdout: {}\nStderr: {}\n",
            output,
            String::from_utf8_lossy(&verifier_output.stderr)
        );
    }

    result
}
