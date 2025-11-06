use std::{
    fs::{create_dir_all, write},
    path::PathBuf,
};

use aws_config::{defaults, BehaviorVersion, Region};
use aws_sdk_s3::Client;
use clap::Parser;
use eyre::{eyre, Result};
use openvm_sdk::{
    config::DEFAULT_HALO2_VERIFIER_K,
    fs::{
        read_object_from_file, write_evm_halo2_verifier_to_folder, write_object_to_file,
        EVM_HALO2_VERIFIER_BASE_NAME, EVM_HALO2_VERIFIER_INTERFACE_NAME,
        EVM_HALO2_VERIFIER_PARENT_NAME,
    },
    Sdk,
};

use crate::{
    default::{
        default_agg_halo2_pk_path, default_agg_stark_pk_path, default_agg_stark_vk_path,
        default_asm_path, default_evm_halo2_verifier_path, default_params_dir,
    },
    util::read_default_agg_and_halo2_pk,
};

#[derive(Parser)]
#[command(
    name = "setup",
    about = "Set up for generating EVM proofs. ATTENTION: this requires large amounts of computation and memory. "
)]
pub struct SetupCmd {
    #[arg(
        long,
        default_value = "false",
        help = "use --evm to also generate proving keys for EVM verifier"
    )]
    pub evm: bool,
    #[arg(
        long,
        default_value = "false",
        help = "force keygen even if the proving keys already exist"
    )]
    pub force_agg_keygen: bool,
}

impl SetupCmd {
    pub async fn run(&self) -> Result<()> {
        let default_agg_stark_pk_path = default_agg_stark_pk_path();
        let default_agg_stark_vk_path = default_agg_stark_vk_path();
        let default_evm_halo2_verifier_path = default_evm_halo2_verifier_path();
        let default_asm_path = default_asm_path();
        if !self.evm {
            if PathBuf::from(&default_agg_stark_pk_path).exists() && !self.force_agg_keygen {
                println!("Aggregation stark proving key already exists");
                return Ok(());
            }
            // agg keygen does not depend on the app config
            let sdk = Sdk::standard();
            let (agg_pk, agg_vk) = sdk.agg_keygen()?;

            println!(
                "Writing STARK aggregation proving key to {}",
                &default_agg_stark_pk_path
            );
            write_object_to_file(default_agg_stark_pk_path, agg_pk)?;
            println!(
                "Writing STARK aggregation verifying key to {}",
                &default_agg_stark_vk_path
            );
            write_object_to_file(default_agg_stark_vk_path, agg_vk)?;

            println!("Generating root verifier ASM...");
            let root_verifier_asm = sdk.generate_root_verifier_asm();

            println!("Writing root verifier ASM to {}", &default_asm_path);
            write(&default_asm_path, root_verifier_asm)?;
        } else {
            let default_agg_halo2_pk_path = default_agg_halo2_pk_path();
            if PathBuf::from(&default_agg_stark_pk_path).exists()
                && PathBuf::from(&default_agg_halo2_pk_path).exists()
                && PathBuf::from(&default_evm_halo2_verifier_path)
                    .join(EVM_HALO2_VERIFIER_PARENT_NAME)
                    .exists()
                && PathBuf::from(&default_evm_halo2_verifier_path)
                    .join(EVM_HALO2_VERIFIER_BASE_NAME)
                    .exists()
                && PathBuf::from(&default_evm_halo2_verifier_path)
                    .join("interfaces")
                    .join(EVM_HALO2_VERIFIER_INTERFACE_NAME)
                    .exists()
            {
                println!("Aggregation proving key and verifier contract already exist");
                return Ok(());
            } else if !Self::check_solc_installed() {
                return Err(eyre!(
                    "solc is not installed, please install solc to continue"
                ));
            }

            Self::download_params(10, DEFAULT_HALO2_VERIFIER_K as u32).await?;
            // halo2 keygen does not depend on the app config
            let sdk = Sdk::standard();

            let agg_vk = if !self.force_agg_keygen
                && PathBuf::from(&default_agg_stark_pk_path).exists()
                && PathBuf::from(&default_agg_stark_vk_path).exists()
                && PathBuf::from(&default_agg_halo2_pk_path).exists()
            {
                let (agg_pk, halo2_pk) = read_default_agg_and_halo2_pk()?;
                sdk.set_agg_pk(agg_pk)
                    .map_err(|_| eyre!("agg_pk already existed"))?;
                sdk.set_halo2_pk(halo2_pk)
                    .map_err(|_| eyre!("halo2_pk already existed"))?;
                read_object_from_file(&default_agg_stark_vk_path)?
            } else {
                println!("Generating proving key...");
                let (_agg_pk, agg_vk) = sdk.agg_keygen()?;
                let _halo2_pk = sdk.halo2_pk();
                agg_vk
            };

            println!("Generating root verifier ASM...");
            let root_verifier_asm = sdk.generate_root_verifier_asm();

            println!("Generating verifier contract...");
            let verifier = sdk.generate_halo2_verifier_solidity()?;

            println!("Writing stark proving key to file...");
            write_object_to_file(&default_agg_stark_pk_path, sdk.agg_pk())?;

            println!("Writing stark verifying key to file...");
            write_object_to_file(&default_agg_stark_vk_path, agg_vk)?;

            println!("Writing halo2 proving key to file...");
            write_object_to_file(&default_agg_halo2_pk_path, sdk.halo2_pk())?;

            println!("Writing root verifier ASM to file...");
            write(&default_asm_path, root_verifier_asm)?;

            println!("Writing verifier contract to file...");
            write_evm_halo2_verifier_to_folder(verifier, &default_evm_halo2_verifier_path)?;
        }
        Ok(())
    }

    fn check_solc_installed() -> bool {
        std::process::Command::new("solc")
            .arg("--version")
            .output()
            .is_ok()
    }

    async fn download_params(min_k: u32, max_k: u32) -> Result<()> {
        let default_params_dir = default_params_dir();
        create_dir_all(&default_params_dir)?;

        let config = defaults(BehaviorVersion::latest())
            .region(Region::new("us-east-1"))
            .no_credentials()
            .load()
            .await;
        let client = Client::new(&config);

        for k in min_k..=max_k {
            let file_name = format!("kzg_bn254_{}.srs", k);
            let local_file_path = PathBuf::from(&default_params_dir).join(&file_name);
            if !local_file_path.exists() {
                println!("Downloading {}", file_name);
                let key = format!("challenge_0085/{}", file_name);
                let resp = client
                    .get_object()
                    .bucket("axiom-crypto")
                    .key(&key)
                    .send()
                    .await?;
                let data = resp.body.collect().await?;
                write(local_file_path, data.into_bytes())?;
            }
        }

        Ok(())
    }
}
