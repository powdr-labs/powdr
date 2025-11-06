use std::path::PathBuf;

use clap::{Parser, ValueEnum};
use eyre::Result;
use openvm_circuit::arch::{instructions::exe::VmExe, OPENVM_DEFAULT_INIT_FILE_NAME};
use openvm_sdk::{config::SdkVmConfig, fs::read_object_from_file, keygen::AppProvingKey, Sdk, F};

use super::{build, BuildArgs, BuildCargoArgs};
use crate::{
    commands::keygen::keygen,
    input::{read_to_stdin, Input},
    util::{
        get_app_pk_path, get_app_vk_path, get_manifest_path_and_dir, get_single_target_name,
        get_target_dir, read_config_toml_or_default,
    },
};

#[derive(Clone, Debug, ValueEnum)]
pub enum ExecutionMode {
    /// Runs the program normally
    Pure,
    /// Runs the program and estimates the execution cost in terms of number of cells
    Meter,
    /// Runs the program and calculates the number of segments that the execution will be split
    /// into for proving
    Segment,
}

#[derive(Parser)]
#[command(name = "run", about = "Run an OpenVM program")]
pub struct RunCmd {
    #[clap(flatten)]
    run_args: RunArgs,

    #[clap(flatten)]
    cargo_args: RunCargoArgs,
}

#[derive(Clone, Parser)]
pub struct RunArgs {
    #[arg(
        long,
        action,
        help = "Path to OpenVM executable, if specified build will be skipped",
        help_heading = "OpenVM Options"
    )]
    pub exe: Option<PathBuf>,

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
        value_parser,
        help = "Input to OpenVM program",
        help_heading = "OpenVM Options"
    )]
    pub input: Option<Input>,

    #[arg(
        long,
        default_value = OPENVM_DEFAULT_INIT_FILE_NAME,
        help = "Name of the init file",
        help_heading = "OpenVM Options"
    )]
    pub init_file_name: String,

    #[arg(
        long,
        value_enum,
        default_value = "pure",
        help = "Execution mode",
        help_heading = "OpenVM Options"
    )]
    pub mode: ExecutionMode,
}

impl From<RunArgs> for BuildArgs {
    fn from(args: RunArgs) -> Self {
        BuildArgs {
            config: args.config,
            output_dir: args.output_dir,
            init_file_name: args.init_file_name,
            ..Default::default()
        }
    }
}

#[derive(Clone, Parser)]
pub struct RunCargoArgs {
    #[arg(
        long,
        short = 'p',
        value_name = "PACKAGES",
        help = "The package to run; by default is the package in the current workspace",
        help_heading = "Package Selection"
    )]
    pub package: Option<String>,

    #[arg(
        long,
        value_name = "BIN",
        help = "Run the specified binary",
        help_heading = "Target Selection"
    )]
    pub bin: Vec<String>,

    #[arg(
        long,
        value_name = "EXAMPLE",
        help = "Run the specified example",
        help_heading = "Target Selection"
    )]
    pub example: Vec<String>,

    #[arg(
        long,
        short = 'F',
        value_name = "FEATURES",
        value_delimiter = ',',
        help = "Space/comma separated list of features to activate",
        help_heading = "Feature Selection"
    )]
    pub features: Vec<String>,

    #[arg(
        long,
        help = "Activate all available features of all selected packages",
        help_heading = "Feature Selection"
    )]
    pub all_features: bool,

    #[arg(
        long,
        help = "Do not activate the `default` feature of the selected packages",
        help_heading = "Feature Selection"
    )]
    pub no_default_features: bool,

    #[arg(
        long,
        value_name = "NAME",
        default_value = "release",
        help = "Run with the given profile",
        help_heading = "Compilation Options"
    )]
    pub profile: String,

    #[arg(
        long,
        value_name = "DIR",
        help = "Directory for all generated artifacts and intermediate files",
        help_heading = "Output Options"
    )]
    pub target_dir: Option<PathBuf>,

    #[arg(
        long,
        short = 'v',
        help = "Use verbose output",
        help_heading = "Display Options"
    )]
    pub verbose: bool,

    #[arg(
        long,
        short = 'q',
        help = "Do not print cargo log messages",
        help_heading = "Display Options"
    )]
    pub quiet: bool,

    #[arg(
        long,
        value_name = "WHEN",
        default_value = "always",
        help = "Control when colored output is used",
        help_heading = "Display Options"
    )]
    pub color: String,

    #[arg(
        long,
        value_name = "PATH",
        help = "Path to the Cargo.toml file, by default searches for the file in the current or any parent directory",
        help_heading = "Manifest Options"
    )]
    pub manifest_path: Option<PathBuf>,

    #[arg(
        long,
        help = "Ignore rust-version specification in packages",
        help_heading = "Manifest Options"
    )]
    pub ignore_rust_version: bool,

    #[arg(
        long,
        help = "Asserts same dependencies and versions are used as when the existing Cargo.lock file was originally generated",
        help_heading = "Manifest Options"
    )]
    pub locked: bool,

    #[arg(
        long,
        help = "Prevents Cargo from accessing the network for any reason",
        help_heading = "Manifest Options"
    )]
    pub offline: bool,

    #[arg(
        long,
        help = "Equivalent to specifying both --locked and --offline",
        help_heading = "Manifest Options"
    )]
    pub frozen: bool,
}

impl From<RunCargoArgs> for BuildCargoArgs {
    fn from(args: RunCargoArgs) -> Self {
        BuildCargoArgs {
            package: args.package.into_iter().collect(),
            bin: args.bin,
            example: args.example,
            features: args.features,
            all_features: args.all_features,
            no_default_features: args.no_default_features,
            profile: args.profile,
            target_dir: args.target_dir,
            verbose: args.verbose,
            quiet: args.quiet,
            color: args.color,
            manifest_path: args.manifest_path,
            ignore_rust_version: args.ignore_rust_version,
            locked: args.locked,
            offline: args.offline,
            frozen: args.frozen,
            ..Default::default()
        }
    }
}

impl RunCmd {
    pub fn run(&self) -> Result<()> {
        let exe_path = if let Some(exe) = &self.run_args.exe {
            exe
        } else {
            // Build and get the executable name
            let target_name = get_single_target_name(&self.cargo_args)?;
            let build_args = self.run_args.clone().into();
            let cargo_args = self.cargo_args.clone().into();
            let output_dir = build(&build_args, &cargo_args)?;
            &output_dir.join(target_name.with_extension("vmexe"))
        };

        let (manifest_path, manifest_dir) =
            get_manifest_path_and_dir(&self.cargo_args.manifest_path)?;
        let config_path = self
            .run_args
            .config
            .to_owned()
            .unwrap_or_else(|| manifest_dir.join("openvm.toml"));
        let app_config = read_config_toml_or_default(&config_path)?;
        let exe: VmExe<F> = read_object_from_file(exe_path)?;
        let inputs = read_to_stdin(&self.run_args.input)?;

        // Create SDK
        let sdk = Sdk::new(app_config)?;

        // For metered modes, load existing app pk from disk or generate it
        if matches!(
            self.run_args.mode,
            ExecutionMode::Segment | ExecutionMode::Meter
        ) {
            let target_dir = get_target_dir(&self.cargo_args.target_dir, &manifest_path);
            let app_pk_path = get_app_pk_path(&target_dir);
            let app_vk_path = get_app_vk_path(&target_dir);

            // Generate app pk if it doesn't exist
            if !app_pk_path.exists() {
                let config_path = self
                    .run_args
                    .config
                    .to_owned()
                    .unwrap_or_else(|| manifest_dir.join("openvm.toml"));
                keygen(&config_path, &app_pk_path, &app_vk_path, None::<&str>)?;
            }

            // Load the app pk and set it
            let app_pk: AppProvingKey<SdkVmConfig> = read_object_from_file(&app_pk_path)?;
            sdk.set_app_pk(app_pk)
                .map_err(|_| eyre::eyre!("Failed to set app pk"))?;
        }

        match self.run_args.mode {
            ExecutionMode::Pure => {
                let output = sdk.execute(exe, inputs)?;
                println!("Execution output: {:?}", output);
            }
            ExecutionMode::Meter => {
                let (output, (cost, instret)) = sdk.execute_metered_cost(exe, inputs)?;
                println!("Execution output: {:?}", output);

                println!("Number of instructions executed: {}", instret);
                println!("Total cost: {}", cost);
            }
            ExecutionMode::Segment => {
                let (output, segments) = sdk.execute_metered(exe, inputs)?;
                println!("Execution output: {:?}", output);

                let total_instructions: u64 = segments.iter().map(|s| s.num_insns).sum();
                println!("Number of instructions executed: {}", total_instructions);
                println!("Total segments: {}", segments.len());
            }
        }

        Ok(())
    }
}
