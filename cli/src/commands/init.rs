use std::{
    fs::{read_to_string, write},
    path::{Path, PathBuf},
    process::Command,
};

use clap::Parser;
use eyre::Result;
use include_dir::{include_dir, Dir};
use toml_edit::{DocumentMut, Item, Value};

static TEMPLATES: Dir = include_dir!("$CARGO_MANIFEST_DIR/templates");

#[derive(Parser)]
#[command(
    name = "init",
    about = "Create an OpenVM package in an existing directory"
)]
pub struct InitCmd {
    #[arg(
        index = 1,
        help = "Path to create the package in",
        help_heading = "Arguments"
    )]
    pub path: Option<PathBuf>,

    #[arg(
        long,
        help = "Create a package with a binary target (src/main.rs)",
        help_heading = "Init Options"
    )]
    pub bin: bool,

    #[arg(
        long,
        help = "Create a package with a library target (src/lib.rs)",
        help_heading = "Init Options"
    )]
    pub lib: bool,

    #[arg(
        long,
        default_value = "2021",
        help = "Specify the Rust edition to use (2015, 2018, 2021, or 2024)",
        help_heading = "Init Options"
    )]
    pub edition: String,

    #[arg(
        long,
        help = "Set the package name, default is the directory name",
        help_heading = "Init Options"
    )]
    pub name: Option<String>,

    #[arg(
        long,
        default_value = "git",
        help = "Initialize a new VCS repository for the given version control system (git, hg, pijul, fossil, or none)",
        help_heading = "Init Options"
    )]
    pub vcs: String,

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
}

impl InitCmd {
    pub fn run(&self) -> Result<()> {
        let mut args = vec!["init"];
        args.extend_from_slice(&["--edition", &self.edition]);
        args.extend_from_slice(&["--vcs", &self.vcs]);
        args.extend_from_slice(&["--color", &self.color]);
        if let Some(name) = &self.name {
            args.extend_from_slice(&["--name", name]);
        }

        let boolean_flags = [
            ("--bin", self.bin),
            ("--lib", self.lib),
            ("--verbose", self.verbose),
            ("--quiet", self.quiet),
        ];
        for (flag, enabled) in boolean_flags {
            if enabled {
                args.push(flag);
            }
        }

        let path = self
            .path
            .clone()
            .unwrap_or(PathBuf::from(".").canonicalize()?);
        args.push(path.to_str().unwrap());

        let mut cmd = Command::new("cargo");
        cmd.args(&args);

        let status = cmd.status()?;
        if !status.success() {
            return Err(eyre::eyre!("cargo init failed with status: {}", status));
        }

        // Add openvm dependency to Cargo.toml, then write template main.rs or lib.rs
        if self.lib {
            add_openvm_dependency(&path, &[])?;
            write_template_file("lib.rs", &path.join("src"))?;
        } else {
            add_openvm_dependency(&path, &["std"])?;
            write_template_file("main.rs", &path.join("src"))?;
        }

        // Write template openvm.toml
        write_template_file("openvm.toml", &path)?;

        Ok(())
    }
}

fn add_openvm_dependency(path: &Path, features: &[&str]) -> Result<()> {
    let cargo_toml_path = path.join("Cargo.toml");
    let cargo_toml_content = read_to_string(&cargo_toml_path)?;
    let mut doc = cargo_toml_content.parse::<DocumentMut>()?;
    let mut openvm_table = toml_edit::InlineTable::new();
    let mut openvm_features = toml_edit::Array::new();
    for feature in features {
        openvm_features.push(Value::from(feature.to_string()));
    }
    openvm_table.insert(
        "git",
        Value::from("https://github.com/openvm-org/openvm.git"),
    );

    // Add version tag
    let version_tag = format!("v{}", env!("CARGO_PKG_VERSION"));
    openvm_table.insert("tag", Value::from(version_tag));

    openvm_table.insert("features", Value::Array(openvm_features));
    doc["dependencies"]["openvm"] = Item::Value(toml_edit::Value::InlineTable(openvm_table));
    write(cargo_toml_path, doc.to_string())?;
    Ok(())
}

fn write_template_file(file_name: &str, dest_dir: &Path) -> Result<()> {
    let file = TEMPLATES
        .get_file(file_name)
        .ok_or_else(|| eyre::eyre!("Template not found: {}", file_name))?;
    write(dest_dir.join(file_name), file.contents())?;
    Ok(())
}
