use lalrpop_util::*;

use crate::utils::{handle_parse_error, ParseError};

pub mod asm_ast;
pub mod ast;
pub mod display;

lalrpop_mod!(
    #[allow(clippy::all)]
    powdr,
    "/parser/powdr.rs"
);

pub fn parse<'a>(file_name: Option<&str>, input: &'a str) -> Result<ast::PILFile, ParseError<'a>> {
    powdr::PILFileParser::new()
        .parse(input)
        .map_err(|err| handle_parse_error(err, file_name, input))
}

pub fn parse_asm<'a>(
    file_name: Option<&str>,
    input: &'a str,
) -> Result<asm_ast::ASMFile, ParseError<'a>> {
    powdr::ASMFileParser::new()
        .parse(input)
        .map_err(|err| handle_parse_error(err, file_name, input))
}

#[cfg(test)]
mod test {
    use std::fs;

    use super::{asm_ast::ASMFile, *};
    use ast::*;

    #[test]
    fn empty() {
        assert!(powdr::PILFileParser::new().parse("").is_ok());
    }

    #[test]
    fn simple_include() {
        let parsed = powdr::PILFileParser::new().parse("include \"x\";").unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![Statement::Include(0, "x".to_string())])
        );
    }

    #[test]
    fn start_offsets() {
        let parsed = powdr::PILFileParser::new()
            .parse("include \"x\"; pol commit t;")
            .unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![
                Statement::Include(0, "x".to_string()),
                Statement::PolynomialCommitDeclaration(
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
        let parsed = powdr::PILFileParser::new().parse("f in g;").unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![Statement::PlookupIdentity(
                0,
                SelectedExpressions {
                    selector: None,
                    expressions: vec![Expression::PolynomialReference(PolynomialReference {
                        name: "f".to_string(),
                        ..Default::default()
                    })]
                },
                SelectedExpressions {
                    selector: None,
                    expressions: vec![Expression::PolynomialReference(PolynomialReference {
                        name: "g".to_string(),
                        ..Default::default()
                    })]
                }
            )])
        );
    }

    fn parse_file(name: &str) -> PILFile {
        let input = fs::read_to_string(name).unwrap();
        parse(Some(name), &input).unwrap_or_else(|err| {
            eprintln!("Parse error during test:");
            err.output_to_stderr();
            panic!();
        })
    }

    fn parse_asm_file(name: &str) -> ASMFile {
        let input = fs::read_to_string(name).unwrap();
        parse_asm(Some(name), &input).unwrap_or_else(|err| {
            eprintln!("Parse error during test:");
            err.output_to_stderr();
            panic!();
        })
    }

    #[test]
    fn parse_example_files() {
        parse_file("tests/polygon-hermez/arith.pil");
        parse_file("tests/polygon-hermez/binary.pil");
        parse_file("tests/polygon-hermez/byte4.pil");
        parse_file("tests/polygon-hermez/config.pil");
        parse_file("tests/polygon-hermez/global.pil");
        parse_file("tests/polygon-hermez/keccakf.pil");
        parse_file("tests/polygon-hermez/main.pil");
        parse_file("tests/polygon-hermez/mem_align.pil");
        parse_file("tests/polygon-hermez/mem.pil");
        parse_file("tests/polygon-hermez/nine2one.pil");
        parse_file("tests/polygon-hermez/padding_kk.pil");
        parse_file("tests/polygon-hermez/padding_kkbit.pil");
        parse_file("tests/polygon-hermez/padding_pg.pil");
        parse_file("tests/polygon-hermez/poseidong.pil");
        parse_file("tests/polygon-hermez/rom.pil");
        parse_file("tests/polygon-hermez/storage.pil");
    }

    #[test]
    fn simple_macro() {
        let parsed = powdr::PILFileParser::new()
            .parse("macro f(x) { x in g; x + 1 };")
            .unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![Statement::MacroDefinition(
                0,
                "f".to_string(),
                vec!["x".to_string()],
                vec![Statement::PlookupIdentity(
                    13,
                    SelectedExpressions {
                        selector: None,
                        expressions: vec![Expression::PolynomialReference(PolynomialReference {
                            name: "x".to_string(),
                            ..Default::default()
                        })]
                    },
                    SelectedExpressions {
                        selector: None,
                        expressions: vec![Expression::PolynomialReference(PolynomialReference {
                            name: "g".to_string(),
                            ..Default::default()
                        })]
                    }
                )],
                Some(Expression::BinaryOperation(
                    Box::new(Expression::PolynomialReference(PolynomialReference {
                        namespace: None,
                        name: "x".to_string(),
                        index: None,
                        next: false
                    })),
                    BinaryOperator::Add,
                    Box::new(Expression::Number(1.into()))
                ))
            )])
        );
    }

    #[test]
    fn parse_example_asm_files() {
        parse_asm_file("tests/asm_data/simple_sum.asm");
    }
}
