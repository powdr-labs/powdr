use std::{
    fs::{copy, create_dir_all},
    path::PathBuf,
};

use clap::Parser;
use eyre::Result;
use openvm_circuit::arch::OPENVM_DEFAULT_INIT_FILE_NAME;
use openvm_sdk::fs::write_to_file_json;

use super::{RunArgs, RunCargoArgs};
use crate::{
    commands::{load_app_pk, load_or_build_specialized, ExecutionMode},
    util::{
        get_app_commit_path, get_manifest_path_and_dir, get_single_target_name, get_target_dir,
        get_target_output_dir,
    },
    Sdk,
};

#[derive(Parser)]
#[command(
    name = "commit",
    about = "View the Bn254 commit of an OpenVM executable"
)]
pub struct CommitCmd {
    #[arg(
        long,
        action,
        help = "Path to app proving key, by default will be ${target_dir}/openvm/app.pk",
        help_heading = "OpenVM Options"
    )]
    pub app_pk: Option<PathBuf>,

    #[arg(
        long,
        action,
        help = "Path to OpenVM specalized, if specified build will be skipped",
        help_heading = "OpenVM Options"
    )]
    pub specialized: Option<PathBuf>,

    #[arg(
        long,
        help = "Path to the OpenVM config .toml file that specifies the VM extensions, by default will search for the file at ${manifest_dir}/openvm.toml",
        help_heading = "OpenVM Options"
    )]
    pub config: Option<PathBuf>,

    #[arg(
        long,
        help = "Output directory that OpenVM proving artifacts will be copied to",
        help_heading = "OpenVM Options"
    )]
    pub output_dir: Option<PathBuf>,

    #[arg(
        long,
        default_value = OPENVM_DEFAULT_INIT_FILE_NAME,
        help = "Name of the init file",
        help_heading = "OpenVM Options"
    )]
    pub init_file_name: String,

    #[command(flatten)]
    cargo_args: RunCargoArgs,
}

impl CommitCmd {
    pub fn run(&self) -> Result<()> {
        let app_pk = load_app_pk(&self.app_pk, &self.cargo_args)?;

        let run_args = RunArgs {
            specialized: self.specialized.clone(),
            config: self.config.clone(),
            output_dir: self.output_dir.clone(),
            init_file_name: self.init_file_name.clone(),
            input: None,
            mode: ExecutionMode::Pure,
        };
        let (specialized, target_name_stem) = load_or_build_specialized(&run_args, &self.cargo_args)?;
        let sdk = Sdk::new(app_pk.app_config())?.with_app_pk(app_pk);

        let app_commit = sdk.app_prover(specialized.exe)?.app_commit();
        println!("exe commit: {:?}", app_commit.app_exe_commit.to_bn254());
        println!("vm commit: {:?}", app_commit.app_vm_commit.to_bn254());

        let (manifest_path, _) = get_manifest_path_and_dir(&self.cargo_args.manifest_path)?;
        let target_dir = get_target_dir(&self.cargo_args.target_dir, &manifest_path);
        let target_output_dir = get_target_output_dir(&target_dir, &self.cargo_args.profile);

        // target_name_stem does not contain "examples/" prefix
        let target_name =
            get_single_target_name(&self.cargo_args).unwrap_or(target_name_stem.into());
        let commit_path = get_app_commit_path(&target_output_dir, target_name);

        println!("Writing app commit to {}", commit_path.display());
        write_to_file_json(&commit_path, app_commit)?;
        if let Some(output_dir) = &self.output_dir {
            create_dir_all(output_dir)?;
            let commit_name = commit_path.file_name().unwrap();
            let to_path = output_dir.join(commit_name);
            copy(commit_path, to_path)?;
        }

        Ok(())
    }
}
