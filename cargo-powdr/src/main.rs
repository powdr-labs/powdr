//! The cargo-powdr project creator.

use clap::{CommandFactory, Parser, Subcommand};

use std::{
    fs,
    io::{self, Write},
    path::Path,
};

#[derive(Parser)]
#[command(name = "cargo-powdr", author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Create a new powdr project.
    New {
        /// The name of the project
        name: String,

        /// The name of the guest
        #[arg(short, long)]
        #[arg(default_value_t = String::from("powdr-guest"))]
        guest_name: String,
    },
}

const HOST_CARGO_TOML_TEMPLATE: &str = include_str!("../template/Cargo.toml");
const HOST_TOOLCHAIN_TEMPLATE: &str = include_str!("../template/rust-toolchain.toml");
const HOST_MAIN_TEMPLATE: &str = include_str!("../template/src/main.rs");
const HOST_README_TEMPLATE: &str = include_str!("../template/README.md");
const GUEST_CARGO_TOML_TEMPLATE: &str = include_str!("../template/guest/Cargo.toml");
const GUEST_MAIN_TEMPLATE: &str = include_str!("../template/guest/src/main.rs");

fn main() -> Result<(), io::Error> {
    let args = Cli::parse();

    if let Some(command) = args.command {
        run_command(command);
        Ok(())
    } else {
        Cli::command().print_help()
    }
}

struct Error(pub String);

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error(e.to_string())
    }
}

#[allow(clippy::print_stderr)]
fn run_command(command: Commands) {
    let result = match command {
        Commands::New { name, guest_name } => new_project(name, guest_name),
    };
    if let Err(e) = result {
        eprintln!("{}", e.0);
        std::process::exit(1);
    }
}

fn new_project(project_name: String, guest_name: String) -> Result<(), Error> {
    let project_dir = Path::new(&project_name);

    if project_dir.exists() {
        return Err(Error("Directory already exists.".to_string()));
    }

    fs::create_dir(project_dir)?;

    // Create the `src` and `guest/src` subdirectories
    let src_dir = project_dir.join("src");
    let guest_dir = project_dir.join("guest");
    let guest_src_dir = guest_dir.join("src");

    for dir in [&src_dir, &guest_dir, &guest_src_dir] {
        fs::create_dir_all(dir)?;
    }

    create_file(&project_dir.join("Cargo.toml"), HOST_CARGO_TOML_TEMPLATE)?;
    create_file(
        &project_dir.join("rust-toolchain.toml"),
        HOST_TOOLCHAIN_TEMPLATE,
    )?;
    create_file(&project_dir.join("README.md"), HOST_README_TEMPLATE)?;
    create_file(&src_dir.join("main.rs"), HOST_MAIN_TEMPLATE)?;
    create_file(&guest_dir.join("Cargo.toml"), GUEST_CARGO_TOML_TEMPLATE)?;
    create_file(&guest_src_dir.join("main.rs"), GUEST_MAIN_TEMPLATE)?;

    replace_placeholders(project_dir, &project_name, &guest_name)?;

    println!("Project '{project_name}' created successfully!");

    Ok(())
}

fn create_file(path: &Path, content: &str) -> Result<(), Error> {
    let mut file = fs::File::create(path)?;
    file.write_all(content.as_bytes())?;
    Ok(())
}

fn replace_placeholders(
    project_dir: &Path,
    project_name: &str,
    guest_name: &str,
) -> Result<(), Error> {
    let project_cargo_toml = project_dir.join("Cargo.toml");
    let guest_cargo_toml = project_dir.join("guest/Cargo.toml");

    // Replace {{PROJECT_NAME}} in Cargo.toml
    if project_cargo_toml.exists() {
        let content = fs::read_to_string(&project_cargo_toml)?;
        let updated_content = content.replace("{{PROJECT_NAME}}", project_name);
        fs::write(&project_cargo_toml, updated_content)?;
    } else {
        return Err(Error(
            "Cargo.toml not found in the project directory.".to_string(),
        ));
    }

    // Replace {{GUEST_NAME}} in guest/Cargo.toml
    if guest_cargo_toml.exists() {
        let content = fs::read_to_string(&guest_cargo_toml)?;
        let updated_content = content.replace("{{GUEST_NAME}}", guest_name);
        fs::write(&guest_cargo_toml, updated_content)?;
    } else {
        return Err(Error(
            "guest/Cargo.toml not found in the project directory.".to_string(),
        ));
    }

    Ok(())
}
