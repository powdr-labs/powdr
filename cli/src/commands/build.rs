use std::{
    env::var,
    fs::{copy, create_dir_all, read},
    path::PathBuf,
};

use clap::Parser;
use eyre::Result;
use itertools::izip;
use openvm_build::{
    build_generic, get_package, get_workspace_packages, get_workspace_root, GuestOptions,
};
use openvm_circuit::arch::{
    instructions::exe::VmExe, InitFileGenerator, OPENVM_DEFAULT_INIT_FILE_NAME,
};
use openvm_sdk::{config::TranspilerConfig, fs::write_object_to_file};
use openvm_transpiler::{elf::Elf, openvm_platform::memory::MEM_SIZE, FromElf};

use crate::util::{
    get_manifest_path_and_dir, get_target_dir, get_target_output_dir, read_config_toml_or_default,
};

#[derive(Parser)]
#[command(name = "build", about = "Compile an OpenVM program")]
pub struct BuildCmd {
    #[clap(flatten)]
    build_args: BuildArgs,

    #[clap(flatten)]
    cargo_args: BuildCargoArgs,
}

impl BuildCmd {
    pub fn run(&self) -> Result<()> {
        build(&self.build_args, &self.cargo_args)?;
        Ok(())
    }
}

#[derive(Clone, Parser)]
pub struct BuildArgs {
    #[arg(
        long,
        help = "Skips transpilation into exe when set",
        help_heading = "OpenVM Options"
    )]
    pub no_transpile: bool,

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
}

impl Default for BuildArgs {
    fn default() -> Self {
        Self {
            no_transpile: false,
            config: None,
            output_dir: None,
            init_file_name: OPENVM_DEFAULT_INIT_FILE_NAME.to_string(),
        }
    }
}

#[derive(Clone, Parser)]
pub struct BuildCargoArgs {
    #[arg(
        long,
        short = 'p',
        value_name = "PACKAGES",
        help = "Build only specified packages",
        help_heading = "Package Selection"
    )]
    pub package: Vec<String>,

    #[arg(
        long,
        alias = "all",
        help = "Build all members of the workspace",
        help_heading = "Package Selection"
    )]
    pub workspace: bool,

    #[arg(
        long,
        value_name = "PACKAGES",
        help = "Exclude specified packages",
        help_heading = "Package Selection"
    )]
    pub exclude: Vec<String>,

    #[arg(
        long,
        help = "Build the package library",
        help_heading = "Target Selection"
    )]
    pub lib: bool,

    #[arg(
        long,
        value_name = "BIN",
        help = "Build the specified binary",
        help_heading = "Target Selection"
    )]
    pub bin: Vec<String>,

    #[arg(
        long,
        help = "Build all binary targets",
        help_heading = "Target Selection"
    )]
    pub bins: bool,

    #[arg(
        long,
        value_name = "EXAMPLE",
        help = "Build the specified example",
        help_heading = "Target Selection"
    )]
    pub example: Vec<String>,

    #[arg(
        long,
        help = "Build all example targets",
        help_heading = "Target Selection"
    )]
    pub examples: bool,

    #[arg(
        long,
        help = "Build all package targets",
        help_heading = "Target Selection"
    )]
    pub all_targets: bool,

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
        help = "Build with the given profile",
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

impl Default for BuildCargoArgs {
    fn default() -> Self {
        Self {
            package: vec![],
            workspace: false,
            exclude: vec![],
            lib: false,
            bin: vec![],
            bins: false,
            example: vec![],
            examples: false,
            all_targets: false,
            features: vec![],
            all_features: false,
            no_default_features: false,
            profile: "release".to_string(),
            target_dir: None,
            verbose: false,
            quiet: false,
            color: "always".to_string(),
            manifest_path: None,
            ignore_rust_version: false,
            locked: false,
            offline: false,
            frozen: false,
        }
    }
}

// Returns either a) the default transpilation output directory or b) the ELF output
// directory if no_transpile is set to true.
pub fn build(build_args: &BuildArgs, cargo_args: &BuildCargoArgs) -> Result<PathBuf> {
    println!("[openvm] Building the package...");

    // Find manifest_path, manifest_dir, and target_dir
    let (manifest_path, manifest_dir) = get_manifest_path_and_dir(&cargo_args.manifest_path)?;
    let target_dir = get_target_dir(&cargo_args.target_dir, &manifest_path);

    // Set guest options using build arguments; use found manifest directory for consistency
    let mut guest_options = GuestOptions::default()
        .with_features(cargo_args.features.clone())
        .with_profile(cargo_args.profile.clone())
        .with_rustc_flags(var("RUSTFLAGS").unwrap_or_default().split_whitespace());

    guest_options.target_dir = Some(target_dir.clone());
    guest_options
        .options
        .push(format!("--color={}", cargo_args.color));
    guest_options.options.push("--manifest-path".to_string());
    guest_options
        .options
        .push(manifest_path.to_string_lossy().to_string());

    for pkg in &cargo_args.package {
        guest_options.options.push("--package".to_string());
        guest_options.options.push(pkg.clone());
    }
    for pkg in &cargo_args.exclude {
        guest_options.options.push("--exclude".to_string());
        guest_options.options.push(pkg.clone());
    }
    for target in &cargo_args.bin {
        guest_options.options.push("--bin".to_string());
        guest_options.options.push(target.clone());
    }
    for example in &cargo_args.example {
        guest_options.options.push("--example".to_string());
        guest_options.options.push(example.clone());
    }

    let all_bins = cargo_args.bins || cargo_args.all_targets;
    let all_examples = cargo_args.examples || cargo_args.all_targets;

    let boolean_flags = [
        ("--workspace", cargo_args.workspace),
        ("--lib", cargo_args.lib || cargo_args.all_targets),
        ("--bins", all_bins),
        ("--examples", all_examples),
        ("--all-features", cargo_args.all_features),
        ("--no-default-features", cargo_args.no_default_features),
        ("--verbose", cargo_args.verbose),
        ("--quiet", cargo_args.quiet),
        ("--ignore-rust-version", cargo_args.ignore_rust_version),
        ("--locked", cargo_args.locked),
        ("--offline", cargo_args.offline),
        ("--frozen", cargo_args.frozen),
    ];
    for (flag, enabled) in boolean_flags {
        if enabled {
            guest_options.options.push(flag.to_string());
        }
    }

    // Write to init file
    let app_config = read_config_toml_or_default(
        build_args
            .config
            .to_owned()
            .unwrap_or_else(|| manifest_dir.join("openvm.toml")),
    )?;
    app_config
        .app_vm_config
        .write_to_init_file(&manifest_dir, Some(&build_args.init_file_name))?;

    // Build (allowing passed options to decide what gets built)
    let elf_target_dir = match build_generic(&guest_options) {
        Ok(raw_target_dir) => raw_target_dir,
        Err(None) => {
            return Err(eyre::eyre!("Failed to build guest"));
        }
        Err(Some(code)) => {
            return Err(eyre::eyre!("Failed to build guest: code = {}", code));
        }
    };
    println!("[openvm] Successfully built the packages");

    // If transpilation is skipped, return the raw target directory
    if build_args.no_transpile {
        if build_args.output_dir.is_some() {
            println!("[openvm] WARNING: Output directory set but transpilation skipped");
        }
        return Ok(elf_target_dir);
    }

    // Get all built packages
    let workspace_root = get_workspace_root(&manifest_path);
    let packages = if cargo_args.workspace || manifest_dir == workspace_root {
        get_workspace_packages(manifest_dir)
            .into_iter()
            .filter(|pkg| {
                (cargo_args.package.is_empty() || cargo_args.package.contains(&pkg.name))
                    && !cargo_args.exclude.contains(&pkg.name)
            })
            .collect()
    } else {
        vec![get_package(manifest_dir)]
    };

    // Find elf paths of all targets for all built packages
    let elf_targets = packages
        .iter()
        .flat_map(|pkg| pkg.targets.iter())
        .filter(|target| {
            // We only build bin and example targets (note they are mutually exclusive
            // types). If no target selection flags are set, then all bin targets are
            // built by default.
            if target.is_example() {
                all_examples || cargo_args.example.contains(&target.name)
            } else if target.is_bin() {
                all_bins
                    || cargo_args.bin.contains(&target.name)
                    || (!cargo_args.examples
                        && !cargo_args.lib
                        && cargo_args.bin.is_empty()
                        && cargo_args.example.is_empty())
            } else {
                false
            }
        })
        .collect::<Vec<_>>();
    let elf_paths = elf_targets
        .iter()
        .map(|target| {
            if target.is_example() {
                elf_target_dir.join("examples")
            } else {
                elf_target_dir.clone()
            }
            .join(&target.name)
        })
        .collect::<Vec<_>>();

    // Transpile, storing in ${target_dir}/openvm/${profile} by default
    let target_output_dir = get_target_output_dir(&target_dir, &cargo_args.profile);

    println!("[openvm] Transpiling the package...");
    for (elf_path, target) in izip!(&elf_paths, &elf_targets) {
        let transpiler = app_config.app_vm_config.transpiler();
        let data = read(elf_path.clone())?;
        let elf = Elf::decode(&data, MEM_SIZE as u32)?;
        let exe = VmExe::from_elf(elf, transpiler)?;

        let target_name = if target.is_example() {
            PathBuf::from("examples").join(&target.name)
        } else {
            PathBuf::from(&target.name)
        };
        let file_name = target_name.with_extension("vmexe");
        let file_path = target_output_dir.join(&file_name);

        write_object_to_file(&file_path, exe)?;
        if let Some(output_dir) = &build_args.output_dir {
            create_dir_all(output_dir)?;
            copy(file_path, output_dir.join(file_name))?;
        }
    }

    let final_output_dir = if let Some(output_dir) = &build_args.output_dir {
        output_dir
    } else {
        &target_output_dir
    };
    println!(
        "[openvm] Successfully transpiled to {}",
        final_output_dir.display()
    );
    Ok(final_output_dir.clone())
}
