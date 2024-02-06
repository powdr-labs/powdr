//! Parser for powdr assembly and PIL

#![deny(clippy::print_stdout)]

use lalrpop_util::*;
use powdr_ast::parsed::asm::ASMProgram;
use powdr_ast::SourceRef;

use powdr_number::FieldElement;
use powdr_parser_util::{handle_parse_error, ParseError};

use std::sync::Arc;

lalrpop_mod!(
    #[allow(clippy::all)]
    pub powdr,
    "/powdr.rs"
);

pub struct ParserContext {
    file_name: Option<Arc<str>>,
    line_starts: Vec<usize>,
}

impl ParserContext {
    pub fn new(file_name: Option<&str>, input: &str) -> Self {
        Self {
            file_name: file_name.map(|s| s.into()),
            line_starts: powdr_parser_util::lines::compute_line_starts(input),
        }
    }

    pub fn source_ref(&self, offset: usize) -> SourceRef {
        let (line, col) = powdr_parser_util::lines::offset_to_line_col(offset, &self.line_starts);
        SourceRef {
            file: self.file_name.clone(),
            line,
            col,
        }
    }
}

pub fn parse<'a, T: FieldElement>(
    file_name: Option<&str>,
    input: &'a str,
) -> Result<powdr_ast::parsed::PILFile<T>, ParseError<'a>> {
    let ctx = ParserContext::new(file_name, input);
    powdr::PILFileParser::new()
        .parse(&ctx, input)
        .map_err(|err| handle_parse_error(err, file_name, input))
}

pub fn parse_asm<'a, T: FieldElement>(
    file_name: Option<&str>,
    input: &'a str,
) -> Result<powdr_ast::parsed::asm::ASMProgram<T>, ParseError<'a>> {
    parse_module(file_name, input).map(|main| ASMProgram { main })
}

pub fn parse_module<'a, T: FieldElement>(
    file_name: Option<&str>,
    input: &'a str,
) -> Result<powdr_ast::parsed::asm::ASMModule<T>, ParseError<'a>> {
    let ctx = ParserContext::new(file_name, input);
    powdr::ASMModuleParser::new()
        .parse(&ctx, input)
        .map_err(|err| handle_parse_error(err, file_name, input))
}

/// Parse an escaped string - used in the grammar.
pub fn unescape_string(s: &str) -> String {
    assert!(s.len() >= 2);
    assert!(s.starts_with('"') && s.ends_with('"'));
    let mut chars = s[1..s.len() - 1].chars();
    let mut result: String = Default::default();
    while let Some(c) = chars.next() {
        result.push(if c == '\\' {
            match chars.next().unwrap() {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                'b' => 8 as char,
                'f' => 12 as char,
                other => other,
            }
        } else {
            c
        })
    }
    result
}

#[cfg(test)]
mod test {
    use super::*;
    use powdr_ast::parsed::{
        asm::ASMProgram, build::direct_reference, PILFile, PilStatement, PolynomialName,
        SelectedExpressions,
    };
    use powdr_number::Bn254Field;
    use powdr_number::GoldilocksField;
    use powdr_parser_util::UnwrapErrToStderr;
    use similar::TextDiff;
    use test_log::test;
    use walkdir::WalkDir;

    #[test]
    fn empty() {
        let input = "";
        let ctx = ParserContext::new(None, input);
        assert!(powdr::PILFileParser::new()
            .parse::<GoldilocksField>(&ctx, input)
            .is_ok());
    }

    #[test]
    fn simple_include() {
        let input = "include \"x\";";
        let ctx = ParserContext::new(None, input);
        let parsed = powdr::PILFileParser::new()
            .parse::<GoldilocksField>(&ctx, input)
            .unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![PilStatement::Include(
                SourceRef {
                    file: None,
                    line: 1,
                    col: 0,
                },
                "x".to_string()
            )])
        );
    }

    #[test]
    fn start_offsets() {
        let input = "include \"x\"; pol commit t;";
        let ctx = ParserContext::new(None, input);
        let parsed = powdr::PILFileParser::new()
            .parse::<GoldilocksField>(&ctx, input)
            .unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![
                PilStatement::Include(
                    SourceRef {
                        file: None,
                        line: 1,
                        col: 0,
                    },
                    "x".to_string()
                ),
                PilStatement::PolynomialCommitDeclaration(
                    SourceRef {
                        file: None,
                        line: 1,
                        col: 13,
                    },
                    vec![PolynomialName {
                        name: "t".to_string(),
                        array_size: None
                    }],
                    None
                )
            ])
        );
    }

    #[test]
    fn simple_plookup() {
        let input = "f in g;";
        let ctx = ParserContext::new(None, input);
        let parsed = powdr::PILFileParser::new()
            .parse::<GoldilocksField>(&ctx, "f in g;")
            .unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![PilStatement::PlookupIdentity(
                SourceRef {
                    file: None,
                    line: 1,
                    col: 0,
                },
                SelectedExpressions {
                    selector: None,
                    expressions: vec![direct_reference("f")]
                },
                SelectedExpressions {
                    selector: None,
                    expressions: vec![direct_reference("g")]
                }
            )])
        );
    }

    fn find_files_with_ext(
        dir: std::path::PathBuf,
        ext: String,
    ) -> impl Iterator<Item = (String, String)> {
        WalkDir::new(&dir).into_iter().filter_map(move |e| {
            let entry = e.unwrap();
            let path = entry.path();
            match path.extension() {
                Some(path_ext) if path_ext.to_str() == Some(&ext) => Some((
                    std::fs::canonicalize(path)
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .into(),
                    std::fs::read_to_string(path).unwrap(),
                )),
                _ => None,
            }
        })
    }

    // helper function to clear SourceRef's inside the AST so we can compare for equality
    fn pil_clear_source_refs<T>(ast: &mut PILFile<T>) {
        ast.0.iter_mut().for_each(pil_statement_clear_source_ref);
    }

    fn pil_statement_clear_source_ref<T>(stmt: &mut PilStatement<T>) {
        match stmt {
            PilStatement::Include(s, _)
            | PilStatement::Namespace(s, _, _)
            | PilStatement::LetStatement(s, _, _, _)
            | PilStatement::PolynomialDefinition(s, _, _)
            | PilStatement::PublicDeclaration(s, _, _, _, _)
            | PilStatement::PolynomialConstantDeclaration(s, _)
            | PilStatement::PolynomialConstantDefinition(s, _, _)
            | PilStatement::PolynomialCommitDeclaration(s, _, _)
            | PilStatement::PolynomialIdentity(s, _)
            | PilStatement::PlookupIdentity(s, _, _)
            | PilStatement::PermutationIdentity(s, _, _)
            | PilStatement::ConnectIdentity(s, _, _)
            | PilStatement::ConstantDefinition(s, _, _)
            | PilStatement::Expression(s, _) => *s = SourceRef::unknown(),
        }
    }

    // helper function to clear SourceRef's inside the AST so we can compare for equality
    fn asm_clear_source_refs<T>(ast: &mut ASMProgram<T>) {
        use powdr_ast::parsed::asm::{
            ASMModule, FunctionStatement, Instruction, InstructionBody, LinkDeclaration, Machine,
            MachineStatement, Module, ModuleStatement, SymbolDefinition, SymbolValue,
        };

        fn clear_machine_stmt<T>(stmt: &mut MachineStatement<T>) {
            match stmt {
                MachineStatement::Degree(s, _)
                | MachineStatement::Submachine(s, _, _)
                | MachineStatement::RegisterDeclaration(s, _, _)
                | MachineStatement::OperationDeclaration(s, _, _, _)
                | MachineStatement::LinkDeclaration(LinkDeclaration { source: s, .. }) => {
                    *s = SourceRef::unknown();
                }
                MachineStatement::Pil(s, stmt) => {
                    *s = SourceRef::unknown();
                    pil_statement_clear_source_ref(stmt)
                }
                MachineStatement::InstructionDeclaration(s, _, Instruction { body, .. }) => {
                    *s = SourceRef::unknown();
                    if let InstructionBody::Local(statements) = body {
                        statements
                            .iter_mut()
                            .for_each(pil_statement_clear_source_ref)
                    }
                }
                MachineStatement::FunctionDeclaration(s, _, _, statements) => {
                    *s = SourceRef::unknown();
                    for fstmt in statements {
                        match fstmt {
                            FunctionStatement::Assignment(s, _, _, _)
                            | FunctionStatement::Instruction(s, _, _)
                            | FunctionStatement::Label(s, _)
                            | FunctionStatement::DebugDirective(s, _)
                            | FunctionStatement::Return(s, _) => *s = SourceRef::unknown(),
                        }
                    }
                }
            }
        }

        fn clear_module_stmt<T>(stmt: &mut ModuleStatement<T>) {
            let ModuleStatement::SymbolDefinition(SymbolDefinition { value, .. }) = stmt;
            match value {
                SymbolValue::Machine(Machine { statements, .. }) => {
                    statements.iter_mut().for_each(clear_machine_stmt)
                }
                SymbolValue::Module(Module::Local(ASMModule { statements })) => {
                    statements.iter_mut().for_each(clear_module_stmt);
                }
                SymbolValue::Module(Module::External(_))
                | SymbolValue::Import(_)
                | SymbolValue::Expression(_) => (),
            }
        }

        ast.main.statements.iter_mut().for_each(clear_module_stmt);
    }

    #[test]
    /// Test that (source -> AST -> source -> AST) works properly for asm files
    fn parse_write_reparse_asm() {
        let crate_dir = env!("CARGO_MANIFEST_DIR");
        let basedir = std::path::PathBuf::from(format!("{crate_dir}/../test_data/"));
        let asm_files = find_files_with_ext(basedir, "asm".into());
        for (file, orig_string) in asm_files {
            let mut orig_asm =
                parse_asm::<Bn254Field>(Some(&file), &orig_string).unwrap_err_to_stderr();
            let orig_asm_to_string = format!("{}", orig_asm);
            let mut reparsed_asm = parse_asm::<Bn254Field>(
                Some((file.clone() + " reparsed").as_ref()),
                &orig_asm_to_string,
            )
            .unwrap_err_to_stderr();
            asm_clear_source_refs(&mut orig_asm);
            asm_clear_source_refs(&mut reparsed_asm);
            if orig_asm != reparsed_asm {
                let orig_ast = format!("{orig_asm:#?}");
                let reparsed_ast = format!("{reparsed_asm:#?}");
                let diff = TextDiff::from_lines(&orig_ast, &reparsed_ast);
                eprintln!("parsed and re-parsed ASTs differ:");
                for change in diff.iter_all_changes() {
                    let sign = match change.tag() {
                        similar::ChangeTag::Delete => "-",
                        similar::ChangeTag::Insert => "+",
                        similar::ChangeTag::Equal => " ",
                    };
                    eprint!("\t{}{}", sign, change);
                }
                panic!("parsed and re-parsed ASTs differ for file: {file}");
            }
        }
    }

    #[test]
    /// Test that (source -> AST -> source -> AST) works properly for pil files
    fn parse_write_reparse_pil() {
        let crate_dir = env!("CARGO_MANIFEST_DIR");
        let basedir = std::path::PathBuf::from(format!("{crate_dir}/../test_data/"));
        let pil_files = find_files_with_ext(basedir, "pil".into());
        for (file, orig_string) in pil_files {
            let mut orig_pil =
                parse::<Bn254Field>(Some(&file), &orig_string).unwrap_err_to_stderr();
            let orig_pil_to_string = format!("{}", orig_pil);
            let mut reparsed_pil = parse::<Bn254Field>(
                Some((file.clone() + " reparsed").as_ref()),
                &orig_pil_to_string,
            )
            .unwrap_err_to_stderr();
            pil_clear_source_refs(&mut orig_pil);
            pil_clear_source_refs(&mut reparsed_pil);
            assert_eq!(orig_pil, reparsed_pil);
            if orig_pil != reparsed_pil {
                let orig_ast = format!("{orig_pil:#?}");
                let reparsed_ast = format!("{reparsed_pil:#?}");
                let diff = TextDiff::from_lines(&orig_ast, &reparsed_ast);
                eprintln!("parsed and re-parsed ASTs differ:");
                for change in diff.iter_all_changes() {
                    let sign = match change.tag() {
                        similar::ChangeTag::Delete => "-",
                        similar::ChangeTag::Insert => "+",
                        similar::ChangeTag::Equal => " ",
                    };
                    eprint!("\t{}{}", sign, change);
                }
                panic!("parsed and re-parsed ASTs differ for file: {file}");
            }
        }
    }

    mod display {
        use powdr_number::GoldilocksField;

        use powdr_parser_util::UnwrapErrToStderr;
        use pretty_assertions::assert_eq;

        use crate::parse;

        #[test]
        fn reparse() {
            let input = r#"
    constant %N = 16;
namespace Fibonacci(%N);
    constant %last_row = (%N - 1);
    let bool: expr -> expr = (|X| (X * (1 - X)));
    let one_hot = (|i, which| match i { which => 1, _ => 0, });
    pol constant ISLAST(i) { one_hot(i, %last_row) };
    pol commit arr[8];
    pol commit x, y;
    { (x + 2), y' } in { ISLAST, 7 };
    y { (x + 2), y' } is ISLAST { ISLAST, 7 };
    ((x - 2) * y) = 8;
    public out = y(%last_row);"#;
            let printed = format!(
                "{}",
                parse::<GoldilocksField>(Some("input"), input).unwrap()
            );
            assert_eq!(input.trim(), printed.trim());
        }

        #[test]
        fn reparse_witness_query() {
            let input = r#"pol commit wit(i) query (x(i), y(i));"#;
            let printed = format!(
                "{}",
                parse::<GoldilocksField>(Some("input"), input).unwrap()
            );
            assert_eq!(input.trim(), printed.trim());
        }

        #[test]
        fn reparse_arrays() {
            let input = "    pol commit y[3];\n    (y - 2) = 0;\n    (y[2] - 2) = 0;\n    public out = y[1](2);";
            let printed = format!(
                "{}",
                parse::<GoldilocksField>(Some("input"), input).unwrap()
            );
            assert_eq!(input.trim(), printed.trim());
        }

        #[test]
        fn reparse_strings_and_tuples() {
            let input = r#"constant %N = ("abc", 3);"#;
            let printed = format!(
                "{}",
                parse::<GoldilocksField>(Some("input"), input).unwrap()
            );
            assert_eq!(input.trim(), printed.trim());
        }

        #[test]
        fn array_literals() {
            let input = r#"let x = [[1], [2], [(3 + 7)]];"#;
            let printed = format!(
                "{}",
                parse::<GoldilocksField>(Some("input"), input).unwrap_err_to_stderr()
            );
            assert_eq!(input.trim(), printed.trim());
        }

        #[test]
        fn type_names_simple() {
            let input = r#"
    let a: col;
    let b: int;
    let c: fe;
    let d: int[];
    let e: int[7];
    let f: (int, fe, fe[3])[2];"#;
            let printed = format!(
                "{}",
                parse::<GoldilocksField>(Some("input"), input).unwrap()
            );
            assert_eq!(input.trim(), printed.trim());
        }

        #[test]
        fn type_names_complex() {
            let input = r#"
    let a: int -> fe;
    let b: int -> ();
    let c: -> ();
    let d: int, int -> fe;
    let e: int, int -> (fe, int[2]);
    let f: ((int, fe), fe[2] -> (fe -> int))[];
    let g: (int -> fe) -> int;
    let h: int -> (fe -> int);"#;
            let printed = format!(
                "{}",
                parse::<GoldilocksField>(Some("input"), input).unwrap()
            );
            assert_eq!(input.trim(), printed.trim());
        }
    }
}
