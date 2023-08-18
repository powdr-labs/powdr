//! Parser for powdr assembly and PIL

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
) -> Result<ast::parsed::asm::ASMFile<T>, ParseError<'a>> {
    powdr::ASMFileParser::new()
        .parse(input)
        .map_err(|err| handle_parse_error(err, file_name, input))
}

#[cfg(test)]
mod test {
    use super::*;
    use ast::parsed::{
        asm::ASMFile, build::direct_reference, BinaryOperator, Expression, PILFile, PilStatement,
        PolynomialName, SelectedExpressions,
    };
    use number::GoldilocksField;
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
                    None
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
        let file = std::path::PathBuf::from("../test_data/").join(name);

        let input = fs::read_to_string(file).unwrap();
        parse(Some(name), &input).unwrap_or_else(|err| {
            eprintln!("Parse error during test:");
            err.output_to_stderr();
            panic!();
        })
    }

    fn parse_asm_file(name: &str) -> ASMFile<GoldilocksField> {
        let file = std::path::PathBuf::from("../test_data/").join(name);

        let input = fs::read_to_string(file).unwrap();
        parse_asm(Some(name), &input).unwrap_or_else(|err| {
            eprintln!("Parse error during test:");
            err.output_to_stderr();
            panic!();
        })
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
    fn simple_macro() {
        let parsed = powdr::PILFileParser::new()
            .parse::<GoldilocksField>("macro f(x) { x in g; x + 1 };")
            .unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![PilStatement::MacroDefinition(
                0,
                "f".to_string(),
                vec!["x".to_string()],
                vec![PilStatement::PlookupIdentity(
                    13,
                    SelectedExpressions {
                        selector: None,
                        expressions: vec![direct_reference("x")]
                    },
                    SelectedExpressions {
                        selector: None,
                        expressions: vec![direct_reference("g")]
                    }
                )],
                Some(Expression::BinaryOperation(
                    Box::new(direct_reference("x")),
                    BinaryOperator::Add,
                    Box::new(Expression::Number(1.into()))
                ))
            )])
        );
    }

    #[test]
    fn parse_example_asm_files() {
        parse_asm_file("asm/simple_sum.asm");
    }

    mod display {
        use number::GoldilocksField;

        use pretty_assertions::assert_eq;

        use crate::parse;

        #[test]
        fn reparse() {
            let input = r#"
constant %N = 16;
namespace Fibonacci(%N);
constant %last_row = (%N - 1);
macro bool(X) { (X * (1 - X)) = 0; };
macro is_nonzero(X) { match X { 0 => 0, _ => 1, } };
macro is_zero(X) { (1 - is_nonzero(X)) };
macro is_equal(A, B) { is_zero((A - B)) };
macro is_one(X) { is_equal(X, 1) };
macro ite(C, A, B) { ((is_nonzero(C) * A) + (is_zero(C) * B)) };
macro one_hot(i, index) { ite(is_equal(i, index), 1, 0) };
pol constant ISLAST(i) { one_hot(i, %last_row) };
pol commit x, y;
macro constrain_equal_expr(A, B) { (A - B) };
macro force_equal_on_last_row(poly, value) { (ISLAST * constrain_equal_expr(poly, value)) = 0; };
force_equal_on_last_row(x', 1);
force_equal_on_last_row(y', 1);
macro on_regular_row(cond) { ((1 - ISLAST) * cond) = 0; };
on_regular_row(constrain_equal_expr(x', y));
on_regular_row(constrain_equal_expr(y', (x + y)));
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
        fn reparse_strings_and_tuples() {
            let input = r#"constant %N = ("abc", 3);"#;
            let printed = format!(
                "{}",
                parse::<GoldilocksField>(Some("input"), input).unwrap()
            );
            assert_eq!(input.trim(), printed.trim());
        }
    }
}
