use mktemp::Temp;
use std::{
    fs::{self},
    process::Command,
    str::from_utf8,
    sync::Arc,
};

use powdr_ast::{
    analyzed::Analyzed,
    parsed::{
        display::format_type_scheme_around_name,
        types::{FunctionType, Type, TypeScheme},
    },
};
use powdr_number::FieldElement;

use crate::{codegen::escape_symbol, util_code::util_code, CompiledPIL, FixedColFunction};

pub fn generate_glue_code<T: FieldElement>(
    symbols: &[(&str, String)],
    analyzed: &Analyzed<T>,
) -> Result<String, String> {
    let utils = util_code::<T>()?;

    let mut glue = String::new();
    let int_int_fun: TypeScheme = Type::Function(FunctionType {
        params: vec![Type::Int],
        value: Box::new(Type::Int),
    })
    .into();
    for (sym, access) in symbols {
        let ty = analyzed.type_of_symbol(sym);
        if ty != int_int_fun && ty.ty != Type::Col {
            return Err(format!(
                "Only (int -> int) functions and columns are supported, but requested{}",
                format_type_scheme_around_name(sym, &Some(ty)),
            ));
        }

        // TODO we should use big int instead of u64
        glue.push_str(&format!(
            r#"
            #[no_mangle]
            pub extern "C" fn {}(i: u64) -> u64 {{
                u64::try_from(({access}).call(ibig::IBig::from(i))).unwrap()
            }}
            "#,
            extern_symbol_name(sym)
        ));
    }

    Ok(format!("{utils}\n{glue}\n",))
}

const CARGO_TOML: &str = r#"
[package]
name = "powdr_jit_compiled"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib"]

[dependencies]
ibig = { version = "0.3.6", features = [], default-features = false }
lazy_static = "1.4.0"
"#;

pub struct PathInTempDir {
    #[allow(dead_code)]
    dir: Temp,
    /// The absolute path
    pub path: String,
}

fn cargo_toml(opt_level: Option<u32>) -> String {
    match opt_level {
        Some(opt_level) => {
            format!("{CARGO_TOML}\n\n[profile.release]\nopt-level = {opt_level}\n",)
        }
        None => CARGO_TOML.to_string(),
    }
}

/// Compiles the given code and returns the path to the
/// temporary directory containing the compiled library
/// and the path to the compiled library.
pub fn call_cargo(code: &str, opt_level: Option<u32>) -> Result<PathInTempDir, String> {
    let dir = mktemp::Temp::new_dir().unwrap();
    fs::write(dir.join("Cargo.toml"), cargo_toml(opt_level)).unwrap();
    fs::create_dir(dir.join("src")).unwrap();
    fs::write(dir.join("src").join("lib.rs"), code).unwrap();
    let output_asm = false;
    let out = Command::new("cargo")
        .env(
            "RUSTFLAGS",
            format!(
                "-C target-cpu=native{}",
                if output_asm { " --emit asm" } else { "" }
            ),
        )
        .arg("build")
        .arg("--release")
        .current_dir(dir.clone())
        .output()
        .unwrap();
    if !out.status.success() {
        if log::log_enabled!(log::Level::Debug) {
            let stderr = from_utf8(&out.stderr).unwrap_or("UTF-8 error in error message.");
            return Err(format!(
                "Rust compiler error when JIT-compiling. Will use interpreter instead. Error message:\n{stderr}."
            ));
        } else {
            return Err("Rust compiler error when JIT-compiling. Will use interpreter instead. Set log level to DEBUG for reason.".to_string());
        }
    }
    #[allow(clippy::print_stdout)]
    if output_asm {
        let asm_file = dir
            .join("target")
            .join("release")
            .join("deps")
            .join("powdr_jit_compiled.s");
        println!("{}", fs::read_to_string(&asm_file).unwrap());
    }
    let extension = if cfg!(target_os = "windows") {
        "dll"
    } else if cfg!(target_os = "macos") {
        "dylib"
    } else {
        "so"
    };
    let lib_path = dir
        .join("target")
        .join("release")
        .join(format!("libpowdr_jit_compiled.{extension}"));
    Ok(PathInTempDir {
        dir,
        path: lib_path.to_str().unwrap().to_string(),
    })
}

/// Loads the given library and functions.
pub fn load_library(path: &str, fixed_column_names: &[&str]) -> Result<CompiledPIL, String> {
    let library = Arc::new(
        unsafe { libloading::Library::new(path) }
            .map_err(|e| format!("Error loading library at {path}: {e}"))?,
    );
    let fixed_columns = fixed_column_names
        .iter()
        .map(|&sym| {
            let extern_sym = extern_symbol_name(sym);
            let function =
                *unsafe { library.get::<extern "C" fn(u64) -> u64>(extern_sym.as_bytes()) }
                    .map_err(|e| format!("Error accessing symbol {sym}: {e}"))?;
            let fun = FixedColFunction {
                library: library.clone(),
                function,
            };
            Ok((sym.to_string(), fun))
        })
        .collect::<Result<_, String>>()?;
    let set_degree_fun = *unsafe { library.get::<extern "C" fn(u64)>(b"__set_degree") }
        .map_err(|e| format!("Error accessing symbol __set_degree: {e}"))?;
    Ok(CompiledPIL {
        library,
        fixed_columns,
        set_degree_fun,
    })
}

fn extern_symbol_name(sym: &str) -> String {
    format!("extern_{}", escape_symbol(sym))
}
