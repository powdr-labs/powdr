use libc::{c_void, dlopen, dlsym, RTLD_NOW};
use mktemp::Temp;
use std::{
    collections::HashMap,
    ffi::CString,
    fs::{self},
    process::Command,
    str::from_utf8,
};

use powdr_ast::{
    analyzed::Analyzed,
    parsed::{
        display::format_type_scheme_around_name,
        types::{FunctionType, Type, TypeScheme},
    },
};
use powdr_number::FieldElement;

use crate::{
    codegen::{escape_symbol},
    SymbolMap,
};

// TODO make this depend on T

const PREAMBLE: &str = r#"
#![allow(unused_parens)]
//type FieldElement = powdr_number::GoldilocksField;
"#;

pub fn generate_glue_code<T: FieldElement>(
    symbols: &[&str],
    analyzed: &Analyzed<T>,
) -> Result<String, String> {
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
                "Only (int -> int) functions and columns are supported, but requested {}",
                format_type_scheme_around_name(sym, &Some(ty)),
            ));
        }

        // TODO we should use big int instead of u64
        let name = escape_symbol(sym);
        glue.push_str(&format!(
            r#"
            #[no_mangle]
            pub extern fn {}(i: u64) -> u64 {{
                u64::try_from({name}(ibig::IBig::from(i))).unwrap()
            }}
            "#,
            extern_symbol_name(sym)
        ));
    }

    Ok(format!("{PREAMBLE}\n{glue}\n",))
}

const CARGO_TOML: &str = r#"
[package]
name = "powdr_jit_compiled"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["dylib"]

[dependencies]
ibig = { version = "0.3.6", features = [], default-features = false }
"#;

/// Compiles the given code and returns the path to the
/// temporary directory containing the compiled library
/// and the path to the compiled library.
pub fn call_cargo(code: &str) -> Result<(Temp, String), String> {
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
    let lib_path = dir
        .join("target")
        .join("release")
        .join("libpowdr_jit_compiled.so");
    Ok((dir, lib_path.to_str().unwrap().to_string()))
}

/// Loads the given library and creates funtion pointers for the given symbols.
pub fn load_library(path: &str, symbols: &[&str]) -> Result<SymbolMap, String> {
    let c_path = CString::new(path).unwrap();
    let lib = unsafe { dlopen(c_path.as_ptr(), RTLD_NOW) };
    if lib.is_null() {
        return Err(format!("Failed to load library: {path:?}"));
    }
    let mut result = HashMap::new();
    for sym in symbols {
        let extern_sym = extern_symbol_name(sym);
        let sym_cstr = CString::new(extern_sym).unwrap();
        let fun_ptr = unsafe { dlsym(lib, sym_cstr.as_ptr()) };
        if fun_ptr.is_null() {
            return Err(format!("Failed to load symbol: {fun_ptr:?}"));
        }
        let fun = unsafe { std::mem::transmute::<*mut c_void, fn(u64) -> u64>(fun_ptr) };
        result.insert(sym.to_string(), fun);
    }
    Ok(result)
}

fn extern_symbol_name(sym: &str) -> String {
    format!("extern_{}", escape_symbol(sym))
}
