use std::path::PathBuf;

use powdr_ast::parsed::{
    asm::{
        non_unique::{self, NonUniqueSymbols},
        Module,
    },
    folder::Folder,
};
static ASM_EXTENSION: &str = "asm";
static FOLDER_MODULE_NAME: &str = "mod";

pub fn load_module_files(
    path: Option<PathBuf>,
    program: non_unique::ASMProgram,
) -> Result<non_unique::ASMProgram, String> {
    Loader { path }.fold_program(program)
}

struct Loader {
    path: Option<PathBuf>,
}

type Error = String;

impl Folder<NonUniqueSymbols, NonUniqueSymbols> for Loader {
    type Error = Error;

    fn fold_module(&mut self, m: non_unique::Module) -> Result<non_unique::Module, Self::Error> {
        match m {
            Module::External(name) => self
                .path
                .clone()
                .map(|path| {
                    // for this, we skip the last part of the current location as if we are at `a::b::c` and declare `d`, we are looking as `a/b/d`
                    let path = path.parent().unwrap().join(name);

                    // look for the module locally, `path/to/module.asm`
                    let file_path = path.with_extension(ASM_EXTENSION);
                    // look for the module in a subdirectory, `path/to/module/mod.asm`
                    let file_in_folder_path =
                        path.join(FOLDER_MODULE_NAME).with_extension(ASM_EXTENSION);

                    let file = std::fs::read_to_string(&file_path);

                    let file_in_folder = std::fs::read_to_string(&file_in_folder_path);

                    match (file, file_in_folder) {
                        // if we found it here, continue from here
                        (Ok(file), Err(_)) => Ok((file, Some(path))),
                        // if we found it in a subdirectory, continue from there
                        (Err(_), Ok(file)) => Ok((file, Some(path.join(FOLDER_MODULE_NAME)))),
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
                    .map(|(file, path)| {
                        powdr_parser::parse_module(Some(&file_path.display().to_string()), &file)
                            .map(|res| (res, path))
                            .unwrap_or_else(|err| {
                                eprintln!(
                                    "Error parsing powdr assembly file {}:",
                                    file_path.display()
                                );
                                err.output_to_stderr();
                                panic!();
                            })
                    })
                })
                .unwrap_or(Err(
                    "Cannot resolve external module without a base path".into()
                )),
            Module::Local(m) => Ok((m, self.path.clone())),
        }
        .and_then(|(m, path)| Loader { path }.fold_module_value(m))
        .map(Module::Local)
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use powdr_parser::parse_asm;

    use super::*;

    fn test_dir(dir: &str, expected: Result<(), &str>) {
        let dir = Path::new(dir);
        let main_path = dir.join("main.asm").to_owned();
        let main_str = std::fs::read_to_string(&main_path).unwrap();
        let main = parse_asm(None, &main_str).unwrap();
        let main = load_module_files(Some(main_path), main);

        let expected = expected
            .map(|_| {
                let expected_str = std::fs::read_to_string(dir.join("expected.asm")).unwrap();
                parse_asm(None, &expected_str).unwrap()
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
