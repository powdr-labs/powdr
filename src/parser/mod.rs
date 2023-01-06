use lalrpop_util::*;

pub mod ast;

lalrpop_mod!(pil, "/parser/pil.rs");

#[cfg(test)]
mod test {
    use super::*;
    use ast::*;

    #[test]
    fn empty() {
        assert!(pil::PILFileParser::new().parse("").is_ok());
    }

    #[test]
    fn simple_include() {
        let parsed = pil::PILFileParser::new().parse("include \"x\";").unwrap();
        assert_eq!(
            parsed,
            PILFile {
                statements: vec![Statement::IncludeStatement(IncludeStatement {
                    file: "x".to_string()
                })]
            }
        );
    }
}
