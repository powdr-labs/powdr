use crate::ast::{FunctionOpKind, Register, Statement};

pub fn parse_asm<R: Register, F: FunctionOpKind, P: Parser<R, F>>(
    parser: P,
    input: &str,
) -> Vec<Statement<R, F>> {
    input
        .split('\n')
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .flat_map(|line| parser.parse(line).unwrap())
        .collect()
}

pub trait Parser<R: Register, F: FunctionOpKind> {
    fn parse(&self, input: &str) -> Result<Vec<Statement<R, F>>, String>;
}
