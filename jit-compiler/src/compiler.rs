use mktemp::Temp;
use std::{
    collections::HashMap,
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

use crate::{codegen::escape_symbol, LoadedFunction};

pub fn generate_glue_code<T: FieldElement>(
    symbols: &[&str],
    analyzed: &Analyzed<T>,
) -> Result<String, String> {
    if T::BITS > 64 {
        return Err(format!(
            "Fields with more than 64 bits not supported, requested {}",
            T::BITS,
        ));
    }
    let mut glue = String::new();
    let int_int_fun: TypeScheme = Type::Function(FunctionType {
        params: vec![Type::Int],
        value: Box::new(Type::Int),
    })
    .into();
    for sym in symbols {
        let ty = analyzed.type_of_symbol(sym);
        if ty != int_int_fun && ty.ty != Type::Col {
            return Err(format!(
                "Only (int -> int) functions and columns are supported, but requested{}",
                format_type_scheme_around_name(sym, &Some(ty)),
            ));
        }

        // TODO we should use big int instead of u64
        // TODO we actually don't know how we have to access this symbol. It might be a closure,
        // which needs to be accessed differently.
        let name = escape_symbol(sym);
        glue.push_str(&format!(
            r#"
            #[no_mangle]
            pub extern "C" fn {}(i: u64) -> u64 {{
                u64::try_from(({name}).call(ibig::IBig::from(i))).unwrap()
            }}
            "#,
            extern_symbol_name(sym)
        ));
    }

    Ok(format!("{PREAMBLE}\n{glue}\n",))
}

const PREAMBLE: &str = r#"
#![allow(unused_parens)]

#[derive(Clone, Copy)]
struct FieldElement(u64);
impl From<u64> for FieldElement {
    fn from(x: u64) -> Self {
        FieldElement(x)
    }
}
impl From<FieldElement> for u64 {
    fn from(x: FieldElement) -> u64 {
        x.0
    }
}
impl From<ibig::IBig> for FieldElement {
    fn from(x: ibig::IBig) -> Self {
        FieldElement(u64::try_from(x).unwrap())
    }
}
impl From<FieldElement> for ibig::IBig {
    fn from(x: FieldElement) -> Self {
        // TODO once we support proper field element operations,
        // this might be more complicated.
        ibig::IBig::from(x.0)
    }
}

#[derive(Clone)]
enum Callable<Args, Ret> {
    Fn(fn(Args) -> Ret),
    Closure(std::sync::Arc<dyn Fn(Args) -> Ret + Send + Sync>),
}
impl<Args, Ret> Callable<Args, Ret> {
    #[inline(always)]
    fn call(&self, args: Args) -> Ret {
        match self {
            Callable::Fn(f) => f(args),
            Callable::Closure(f) => f(args),
        }
    }
}

"#;

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

/// Compiles the given code and returns the path to the
/// temporary directory containing the compiled library
/// and the path to the compiled library.
pub fn call_cargo(code: &str) -> Result<PathInTempDir, String> {
    let dir = mktemp::Temp::new_dir().unwrap();
    fs::write(dir.join("Cargo.toml"), CARGO_TOML).unwrap();
    fs::create_dir(dir.join("src")).unwrap();
    fs::write(dir.join("src").join("lib.rs"), code).unwrap();
    let out = Command::new("cargo")
        .env("RUSTFLAGS", "-C target-cpu=native")
        .arg("build")
        .arg("--release")
        .current_dir(dir.clone())
        .output()
        .unwrap();
    if !out.status.success() {
        let stderr = from_utf8(&out.stderr).unwrap_or("UTF-8 error in error message.");
        return Err(format!("Failed to compile: {stderr}."));
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

/// Loads the given library and creates function pointers for the given symbols.
pub fn load_library(
    path: &str,
    symbols: &[&str],
) -> Result<HashMap<String, LoadedFunction>, String> {
    let library = Arc::new(
        unsafe { libloading::Library::new(path) }
            .map_err(|e| format!("Error loading library at {path}: {e}"))?,
    );
    symbols
        .iter()
        .map(|&sym| {
            let extern_sym = extern_symbol_name(sym);
            let function =
                *unsafe { library.get::<extern "C" fn(u64) -> u64>(extern_sym.as_bytes()) }
                    .map_err(|e| format!("Error accessing symbol {sym}: {e}"))?;
            let fun = LoadedFunction {
                library: library.clone(),
                function,
            };
            Ok((sym.to_string(), fun))
        })
        .collect::<Result<_, String>>()
}

fn extern_symbol_name(sym: &str) -> String {
    format!("extern_{}", escape_symbol(sym))
}
