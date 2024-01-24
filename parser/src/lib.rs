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

#[cfg(test)]
mod test {
    use super::*;
    use powdr_ast::parsed::{
        build::direct_reference, PILFile, PilStatement, PolynomialName, SelectedExpressions,
    };
    use powdr_number::GoldilocksField;
    use powdr_parser_util::UnwrapErrToStderr;
    use std::fs;
    use test_log::test;

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

    fn parse_file(name: &str) -> PILFile<GoldilocksField> {
        let file = std::path::PathBuf::from(format!(
            "{}/../test_data/{name}",
            env!("CARGO_MANIFEST_DIR")
        ));

        let input = fs::read_to_string(file).unwrap();
        parse(Some(name), &input).unwrap_err_to_stderr()
    }

    fn parse_asm_file(name: &str) -> ASMProgram<GoldilocksField> {
        let file = std::path::PathBuf::from(format!(
            "{}/../test_data/{name}",
            env!("CARGO_MANIFEST_DIR")
        ));

        let input = fs::read_to_string(file).unwrap();
        parse_asm(Some(name), &input).unwrap_err_to_stderr()
    }

    #[test]
    fn parse_example_files() {
        parse_file("polygon-hermez/arith.pil");
        parse_file("polygon-hermez/binary.pil");
        parse_file("polygon-hermez/byte4.pil");
        parse_file("polygon-hermez/config.pil");
        parse_file("polygon-hermez/global.pil");
        parse_file("polygon-hermez/keccakf.pil");
        parse_file("polygon-hermez/main.pil");
        parse_file("polygon-hermez/mem_align.pil");
        parse_file("polygon-hermez/mem.pil");
        parse_file("polygon-hermez/nine2one.pil");
        parse_file("polygon-hermez/padding_kk.pil");
        parse_file("polygon-hermez/padding_kkbit.pil");
        parse_file("polygon-hermez/padding_pg.pil");
        parse_file("polygon-hermez/poseidong.pil");
        parse_file("polygon-hermez/rom.pil");
        parse_file("polygon-hermez/storage.pil");
    }

    #[test]
    fn parse_example_asm_files() {
        parse_asm_file("asm/simple_sum.asm");
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
    let bool = [(|X| (X * (1 - X)))][0];
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
    }
}
