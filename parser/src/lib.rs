//! Parser for powdr assembly and PIL

#![deny(clippy::print_stdout)]

use ast::parsed::asm::ASMProgram;
use lalrpop_util::*;

use number::FieldElement;
use parser_util::{handle_parse_error, ParseError};

lalrpop_mod!(
    #[allow(clippy::all)]
    pub powdr,
    "/powdr.rs"
);

pub fn parse<'a, T: FieldElement>(
    file_name: Option<&str>,
    input: &'a str,
) -> Result<ast::parsed::PILFile<T>, ParseError<'a>> {
    powdr::PILFileParser::new()
        .parse(input)
        .map_err(|err| handle_parse_error(err, file_name, input))
}

pub fn parse_asm<'a, T: FieldElement>(
    file_name: Option<&str>,
    input: &'a str,
) -> Result<ast::parsed::asm::ASMProgram<T>, ParseError<'a>> {
    parse_module(file_name, input).map(|main| ASMProgram { main })
}

pub fn parse_module<'a, T: FieldElement>(
    file_name: Option<&str>,
    input: &'a str,
) -> Result<ast::parsed::asm::ASMModule<T>, ParseError<'a>> {
    powdr::ASMModuleParser::new()
        .parse(input)
        .map_err(|err| handle_parse_error(err, file_name, input))
}

#[cfg(test)]
mod test {
    use super::*;
    use ast::parsed::{
        build::direct_reference, PILFile, PilStatement, PolynomialName, SelectedExpressions,
    };
    use number::GoldilocksField;
    use parser_util::UnwrapErrToStderr;
    use std::fs;
    use test_log::test;

    #[test]
    fn empty() {
        assert!(powdr::PILFileParser::new()
            .parse::<GoldilocksField>("")
            .is_ok());
    }

    #[test]
    fn simple_include() {
        let parsed = powdr::PILFileParser::new()
            .parse::<GoldilocksField>("include \"x\";")
            .unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![PilStatement::Include(0, "x".to_string())])
        );
    }

    #[test]
    fn start_offsets() {
        let parsed = powdr::PILFileParser::new()
            .parse::<GoldilocksField>("include \"x\"; pol commit t;")
            .unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![
                PilStatement::Include(0, "x".to_string()),
                PilStatement::PolynomialCommitDeclaration(
                    13,
                    vec![PolynomialName {
                        name: "t".to_string(),
                        array_size: None
                    }],
                    None,
                    false
                )
            ])
        );
    }

    #[test]
    fn simple_plookup() {
        let parsed = powdr::PILFileParser::new()
            .parse::<GoldilocksField>("f in g;")
            .unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![PilStatement::PlookupIdentity(
                0,
                None,
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

    #[test]
    fn parse_permutation_attribute() {
        let parsed = powdr::PILFileParser::new()
            .parse::<GoldilocksField>(
                "
            #[attribute]
            { f } is { g };",
            )
            .unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![PilStatement::PermutationIdentity(
                13,
                Some("attribute".to_string()),
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
        use number::GoldilocksField;

        use parser_util::UnwrapErrToStderr;
        use pretty_assertions::assert_eq;

        use crate::parse;

        #[test]
        fn reparse() {
            let input = r#"
constant %N = 16;
namespace Fibonacci(%N);
constant %last_row = (%N - 1);
let bool = [|X| (X * (1 - X))][0];
let one_hot = |i, which| match i { which => 1, _ => 0, };
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
            let input = "pol commit y[3];\n(y - 2) = 0;\n(y[2] - 2) = 0;\npublic out = y[1](2);";
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
