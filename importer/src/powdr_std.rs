use std::{env, path::PathBuf};

use powdr_ast::parsed::{
    asm::{
        non_unique::{
            ASMModule, ASMProgram, Import, Module, NonUniqueSymbols, Part, SymbolDefinition,
            SymbolPath, SymbolValue,
        },
        Symbols,
    },
    folder::{fold_module_value, Folder},
};
use powdr_parser::parse_asm;

use crate::load_module_files;

static POWDR_STD_ENV: &str = "POWDR_STD";
static MOD_FILE: &str = "mod.asm";

/// Loads the standard library module from the location specified in the <POWDR_STD_ENV> environment variable
/// (or, if unset, <project_root>/std).
///
/// # Panics
/// If there is an error loading the standard library
fn load_std() -> ASMModule {
    let default_std_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("std");
    let std_path = env::var(POWDR_STD_ENV)
        .map(PathBuf::from)
        .unwrap_or(default_std_path);
    let std_path = std_path.join(MOD_FILE);

    match std::fs::read_to_string(&std_path) {
        Err(_) => {
            panic!(
                "Couldn't find the powdr standard library at {}. Set the {} environment variable.",
                std_path.display(),
                POWDR_STD_ENV
            )
        }
        Ok(std_source) => {
            let std_content = parse_asm(Some(std_path.as_path().to_str().unwrap()), &std_source)
                .unwrap_or_else(|err| {
                    eprintln!("Error parsing powdr standard library file:");
                    err.output_to_stderr();
                    panic!();
                });
            // This resolves all submodules and returns the standard library's main module
            load_module_files(Some(std_path), std_content).unwrap().main
        }
    }
}

pub fn add_std(program: ASMProgram) -> Result<ASMProgram, String> {
    StdAdder().fold_program(program)
}

struct StdAdder();

type Error = String;

impl Folder<NonUniqueSymbols, NonUniqueSymbols> for StdAdder {
    type Error = Error;

    fn fold_program(&mut self, p: ASMProgram) -> Result<ASMProgram, Self::Error> {
        // Add `std` to the main module
        let mut main = p.main;
        main.symbols.insert(SymbolDefinition {
            name: "std".to_string(),
            value: SymbolValue::Module(Module::Local(load_std())),
        });

        // Recurse
        let main = self.fold_module_value(main)?;
        Ok(ASMProgram { main })
    }

    fn fold_module_value(&mut self, module: ASMModule) -> Result<ASMModule, Self::Error> {
        let mut module = fold_module_value(self, module)?;

        // Check whether the module already has a definition for `std`
        // (E.g. the main module)
        let has_std = module.symbols.iter().map(|d| d.name).any(|n| n == "std");

        if !has_std {
            // If not, add `use super::std;`
            let std_import_path =
                SymbolPath::from_parts([Part::Super, Part::Named("std".to_string())]);
            module.symbols.insert(SymbolDefinition {
                name: "std".to_string(),
                value: SymbolValue::Import(Import {
                    path: std_import_path,
                }),
            });
        }

        Ok(module)
    }
}
