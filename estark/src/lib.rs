use serde::{Deserialize, Serialize};
use std::{fs, process::Command};

pub fn prove(pil: &str, degree: u64, constants: &str, commits: &str, output_dir: &str) {
    let params = StarkParams::simple(degree);

    let pil_stark = std::env::var("PIL_STARK").expect(
        "Please set the PIL_STARK environment variable to the path to the pil-stark repository.",
    );
    let zkevm_prover= std::env::var("ZKEVM_PROVER")
        .expect("Please set the ZKEVM_PROVER environment variable to the path to the zkevm-prover repository.");

    assert!(
        fs::metadata(constants).unwrap().len() > 0,
        "Empty constants file"
    );

    assert!(
        fs::metadata(commits).unwrap().len() > 0,
        "Empty commits file"
    );

    let pil_file_name = format!("{}.json", pil);
    let starkinfo_file_name = format!("{output_dir}/starkinfo.json");
    let consttree_file_name = format!("{output_dir}/consttree.bin");
    let verification_key_file_name = format!("{output_dir}/verification_key.json");
    let stark_struct_file_name = format!("{output_dir}/starkstruct.json");
    let chelpers_dir = format!("{output_dir}/build/chelpers");

    let stark_struct = serde_json::to_string(&params).unwrap();
    fs::write(stark_struct_file_name.clone(), stark_struct).unwrap();

    // node --max-old-space-size=$2 "$PIL_STARK"/src/main_buildconsttree.js -c "$CONSTANTS" -j "$PIL" -s "$STARKSTRUCT"
    println!("Generating consttree...");
    let consttree_output = Command::new("node")
        .args([
            "--max-old-space-size=32000".to_string(), // 32GB of memory
            format!("{pil_stark}/src/main_buildconsttree.js"),
            "-c".to_string(),
            constants.to_string(),
            "-j".to_string(),
            pil_file_name.to_string(),
            "-s".to_string(),
            stark_struct_file_name.to_string(),
            "-t".to_string(),
            consttree_file_name.to_string(),
            "-v".to_string(),
            verification_key_file_name,
        ])
        .output()
        .expect("failed to run pil-stark consttree builder.");

    if !consttree_output.status.success() {
        panic!(
            "Consttree builder run was unsuccessful.\nStdout: {}\nStderr: {}\n",
            String::from_utf8_lossy(&consttree_output.stdout),
            String::from_utf8_lossy(&consttree_output.stderr)
        );
    }
    println!("Generated consttree.");

    // node --max-old-space-size=$2 "$PIL_STARK"/src/main_genstarkinfo.js -j "$PIL" -s "$STARKSTRUCT" -i "$STARKINFO"
    println!("Generating starkinfo...");
    let starkinfo_output = Command::new("node")
        .args([
            "--max-old-space-size=32000".to_string(), // 32GB of memory
            format!("{pil_stark}/src/main_genstarkinfo.js"),
            "-j".to_string(),
            pil_file_name.to_string(),
            "-s".to_string(),
            stark_struct_file_name,
            "-i".to_string(),
            starkinfo_file_name.to_string(),
        ])
        .output()
        .expect("failed to run pil-stark starkinfo generator.");

    if !starkinfo_output.status.success() {
        panic!(
            "Starkinfo generator run was unsuccessful.\nStdout: {}\nStderr: {}\n",
            String::from_utf8_lossy(&starkinfo_output.stdout),
            String::from_utf8_lossy(&starkinfo_output.stderr)
        );
    }
    println!("Generated starkinfo.");

    // node --max-old-space-size=$2 "$PIL_STARK"/src/main_buildchelpers.js -m -j "$PIL" -s "$STARKINFO" -c build/chelpers/zkevm.chelpers.cpp -C ZkevmSteps
    println!("Generating C helpers...");
    let chelpers_output = Command::new("node")
        .args([
            "--max-old-space-size=32000".to_string(), // 32GB of memory
            format!("{pil_stark}/src/main_buildchelpers.js"),
            "-m".to_string(),
            "-j".to_string(),
            pil_file_name,
            "-s".to_string(),
            starkinfo_file_name.to_string(),
            "-c".to_string(),
            format!("{chelpers_dir}/zkevm.chelpers.cpp"),
            "-C".to_string(),
            "ZkevmSteps".to_string(),
        ])
        .output()
        .expect("failed to run pil-stark C helpers generator.");

    if !chelpers_output.status.success() {
        panic!(
            "C helpers generator run was unsuccessful.\nStdout: {}\nStderr: {}\n",
            String::from_utf8_lossy(&chelpers_output.stdout),
            String::from_utf8_lossy(&chelpers_output.stderr)
        );
    }
    println!("Generated C helpers.");

    // rm build/chelpers/*public*.cpp
    let chelpers_output = Command::new("rm")
        .arg(format!("{chelpers_dir}/zkevm.chelpers.public.cpp"))
        .output()
        .expect("failed to remove public inputs from generated C helpers.");
    if !chelpers_output.status.success() {
        panic!(
            "C helpers generator run was unsuccessful.\nStdout: {}\nStderr: {}\n",
            String::from_utf8_lossy(&chelpers_output.stdout),
            String::from_utf8_lossy(&chelpers_output.stderr)
        );
    }

    // cp build/chelpers/* "$ZKEVM_PROVER"/src/starkpil/zkevm/chelpers/
    let chelpers_output = Command::new("bash")
        .args([
            "-c".to_string(),
            format!("cp {chelpers_dir}/*.cpp {zkevm_prover}/src/starkpil/zkevm/chelpers/"),
        ])
        .output()
        .expect("failed to copy generated C helpers.");
    if !chelpers_output.status.success() {
        panic!(
            "C helpers generator run was unsuccessful.\nStdout: {}\nStderr: {}\n",
            String::from_utf8_lossy(&chelpers_output.stdout),
            String::from_utf8_lossy(&chelpers_output.stderr)
        );
    }

    println!("Compiling prover...");
    let make_output = Command::new("bash")
        .args(["-c".to_string(), format!("cd {zkevm_prover} && make test")])
        .output()
        .expect("failed to compile prover.");
    if !make_output.status.success() {
        panic!(
            "Prover compilation run was unsuccessful.\nStdout: {}\nStderr: {}\n",
            String::from_utf8_lossy(&chelpers_output.stdout),
            String::from_utf8_lossy(&chelpers_output.stderr)
        );
    }
    println!("Compiled prover.");

    println!("Generating proof...");
    let proof_output = Command::new("bash")
        .args([
            "-c".to_string(),
            format!(
                "{zkevm_prover}/build/zkProverTest {} {} {} {}",
                constants, consttree_file_name, starkinfo_file_name, commits
            ),
        ])
        .output()
        .expect("failed to generate proof.");
    if !proof_output.status.success() {
        panic!(
            "Proof generation run was unsuccessful.\nStdout: {}\nStderr: {}\n",
            String::from_utf8_lossy(&chelpers_output.stdout),
            String::from_utf8_lossy(&chelpers_output.stderr)
        );
    }
    println!("Proof generated.");
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StarkParams {
    n_bits: u64,
    n_bits_ext: u64,
    n_queries: u64,
    verification_hash_type: StarkHashType,
    steps: Vec<Step>,
}

impl StarkParams {
    fn simple(n_bits: u64) -> Self {
        StarkParams {
            n_bits,
            n_bits_ext: n_bits + 2,
            n_queries: 1,
            verification_hash_type: StarkHashType::GL,
            steps: vec![Step { n_bits: n_bits + 2 }],
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum StarkHashType {
    GL,
    Bn256,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Step {
    n_bits: u64,
}
