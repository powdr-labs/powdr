use libc::{c_void, dlopen, dlsym, RTLD_NOW};
use std::{
    collections::{HashMap},
    ffi::CString,
    fs::{self},
    path,
    process::Command,
};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{
        Analyzed,
    },
};
use powdr_number::FieldElement;

use crate::codegen::{escape_symbol, CodeGenerator};

// TODO make this depend on T

const PREAMBLE: &str = r#"
#![allow(unused_parens)]
type FieldElement = powdr_number::goldilocks::GoldilocksField;
"#;

// TODO this is the old impl of goldilocks

const CARGO_TOML: &str = r#"
[package]
name = "powdr_jit_compiled"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["dylib"]

[dependencies]
powdr-number = { git = "https://github.com/powdr-labs/powdr.git" }
"#;

pub fn compile<T: FieldElement>(
    analyzed: &Analyzed<T>,
    symbols: &[&str],
) -> Result<HashMap<String, fn(u64) -> u64>, String> {
    let mut codegen = CodeGenerator::new(analyzed);
    let mut glue = String::new();
    for sym in symbols {
        codegen.request_symbol(sym)?;
        // TODO verify that the type is `int -> int`.
        // TODO we should use big int instead of u64
        let name = escape_symbol(sym);
        glue.push_str(&format!(
            r#"
            #[no_mangle]
            pub extern fn extern_{name}(i: u64) -> u64 {{
                {name}(powdr_number::BigInt::from(i)).into_bigint().0[0]
            }}
            "#
        ));
    }

    let code = format!("{PREAMBLE}\n{}\n{glue}\n", codegen.compiled_symbols());
    println!("Compiled code:\n{code}");

    // TODO for testing, keep the dir the same
    //let dir = mktemp::Temp::new_dir().unwrap();
    let _ = fs::remove_dir_all("/tmp/powdr_constants");
    fs::create_dir("/tmp/powdr_constants").unwrap();
    let dir = path::Path::new("/tmp/powdr_constants");
    fs::write(dir.join("Cargo.toml"), CARGO_TOML).unwrap();
    fs::create_dir(dir.join("src")).unwrap();
    fs::write(dir.join("src").join("lib.rs"), code).unwrap();
    let out = Command::new("cargo")
        .arg("build")
        .arg("--release")
        .current_dir(dir)
        .output()
        .unwrap();
    out.stderr.iter().for_each(|b| print!("{}", *b as char));
    if !out.status.success() {
        panic!("Failed to compile.");
    }

    let lib_path = CString::new(
        dir.join("target")
            .join("release")
            .join("libpowdr_constants.so")
            .to_str()
            .unwrap(),
    )
    .unwrap();

    let lib = unsafe { dlopen(lib_path.as_ptr(), RTLD_NOW) };
    if lib.is_null() {
        panic!("Failed to load library: {lib_path:?}");
    }
    let mut result = HashMap::new();
    for sym in symbols {
        let sym = format!("extern_{}", escape_symbol(sym));
        let sym_cstr = CString::new(sym.clone()).unwrap();
        let fun_ptr = unsafe { dlsym(lib, sym_cstr.as_ptr()) };
        if fun_ptr.is_null() {
            return Err(format!("Failed to load symbol: {fun_ptr:?}"));
        }
        println!("Loaded symbol: {fun_ptr:?}");
        let fun = unsafe { std::mem::transmute::<*mut c_void, fn(u64) -> u64>(fun_ptr) };
        result.insert(sym, fun);
    }
    Ok(result)
}
