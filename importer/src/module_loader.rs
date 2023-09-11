use std::path::PathBuf;

use ast::parsed::asm::{
    ASMModule, ASMProgram, Module, ModuleStatement, SymbolDefinition, SymbolValue,
};
use number::FieldElement;

static ASM_EXTENSION: &str = "asm";
static FOLDER_MODULE_NAME: &str = "mod";

pub fn load_module_files<T: FieldElement>(
    base_path: Option<PathBuf>,
    program: ASMProgram<T>,
) -> Result<ASMProgram<T>, String> {
    Ok(ASMProgram {
        main: load_module_files_in_module(base_path, program.main)?,
    })
}

pub fn load_module_files_in_module<T: FieldElement>(
    path: Option<PathBuf>,
    module: ASMModule<T>,
) -> Result<ASMModule<T>, String> {
    Ok(ASMModule {
        statements: module
            .statements
            .into_iter()
            .map(|s| match s {
                ModuleStatement::SymbolDefinition(SymbolDefinition { name, value }) => {
                    let value = match value {
                        SymbolValue::Module(m) => {
                            let m = match m {
                                Module::External(name) => path
                                    .clone()
                                    .map(|path| {
                                        // for this, we skip the last part of the current location as if we are at `a::b::c` and declare `d`, we are looking as `a/b/d`
                                        let path = path.parent().unwrap().join(name);

                                        // look for the module locally, `path/to/module.asm`
                                        let file_path = path.with_extension(ASM_EXTENSION);
                                        // look for the module in a subdirectory, `path/to/module/mod.asm`
                                        let file_in_folder_path = path
                                            .join(FOLDER_MODULE_NAME)
                                            .with_extension(ASM_EXTENSION);

                                        dbg!(file_path.display());
                                        dbg!(file_in_folder_path.display());

                                        let file = std::fs::read_to_string(&file_path);

                                        let file_in_folder =
                                            std::fs::read_to_string(&file_in_folder_path);

                                        match (file, file_in_folder) {
                                            // if we found it here, continue from here
                                            (Ok(file), Err(_)) => Ok((file, Some(path))),
                                            // if we found it in a subdirectory, continue from there
                                            (Err(_), Ok(file)) => {
                                                Ok((file, Some(path.join(FOLDER_MODULE_NAME))))
                                            }
                                            (Ok(_), Ok(_)) => Err(format!(
                                                "Expecting either `{}` or `{}`, found both",
                                                file_path.display(),
                                                file_in_folder_path.display()
                                            )),
                                            (Err(_), Err(_)) => Err(format!(
                                                "Expecting either `{}` or `{}`, found neither",
                                                file_path.display(),
                                                file_in_folder_path.display()
                                            )),
                                        }
                                        .and_then(
                                            |(file, path)| {
                                                parser::parse_module(None, &file)
                                                    .map(|res| (res, path))
                                                    .map_err(|e| format!("{e:?}"))
                                            },
                                        )
                                    })
                                    .unwrap_or(Err(
                                        "Cannot resolve external module without a base path".into(),
                                    )),
                                Module::Local(m) => Ok((m, path.clone())),
                            }
                            .and_then(|(m, path)| load_module_files_in_module(path, m));
                            m.map(|m| SymbolValue::Module(Module::Local(m)))
                        }
                        value => Ok(value),
                    };
                    value.map(|value| {
                        ModuleStatement::SymbolDefinition(SymbolDefinition { name, value })
                    })
                }
            })
            .collect::<Result<Vec<_>, _>>()?,
    })
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use number::Bn254Field;
    use parser::parse_asm;

    use super::*;

    fn test_dir(dir: &str, expected: Result<(), &str>) {
        let dir = Path::new(dir);
        let main_path = dir.join("main.asm").to_owned();
        let main_str = std::fs::read_to_string(&main_path).unwrap();
        let main = parse_asm::<Bn254Field>(None, &main_str).unwrap();
        let main = load_module_files(Some(main_path), main);

        let expected = expected
            .map(|_| {
                let expected_str = std::fs::read_to_string(dir.join("expected.asm")).unwrap();
                parse_asm::<Bn254Field>(None, &expected_str).unwrap()
            })
            .map_err(|e| e.to_string());

        assert_eq!(main, expected);
    }

    #[test]
    fn same_dir() {
        test_dir("test_data/same_dir", Ok(()));
    }

    #[test]
    fn other_dir() {
        test_dir("test_data/other_dir", Ok(()));
    }

    #[test]
    fn both() {
        test_dir(
            "test_data/both",
            Err(
                "Expecting either `test_data/both/A.asm` or `test_data/both/A/mod.asm`, found both",
            ),
        );
    }

    #[test]
    fn not_found_in_same_dir() {
        test_dir("test_data/not_found_in_same_dir", Err("Expecting either `test_data/not_found_in_same_dir/B.asm` or `test_data/not_found_in_same_dir/B/mod.asm`, found neither"));
    }

    #[test]
    fn not_found_in_other_dir() {
        test_dir("test_data/not_found_in_other_dir", Err("Expecting either `test_data/not_found_in_other_dir/A/B.asm` or `test_data/not_found_in_other_dir/A/B/mod.asm`, found neither"));
    }
}
