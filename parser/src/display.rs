use std::fmt::{Display, Formatter, Result};

use crate::asm_ast::{
    analysis::{
        AnalysisASMFile, DegreeStatement, Incompatible, IncompatibleSet,
        InstructionDefinitionStatement, PilBlock, RegisterDeclarationStatement,
    },
    ASMFile, ASMStatement, InstructionBodyElement, InstructionParam, InstructionParamList,
    InstructionParams, PlookupOperator, RegisterFlag,
};

#[cfg(test)]
mod test {
    use number::GoldilocksField;

    use crate::{parse, parse_asm};

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

    #[test]
    fn reparse_asm() {
        let input = r#"instr loop { pc' = pc }"#;
        let printed = format!(
            "{}",
            parse_asm::<GoldilocksField>(Some("input"), input).unwrap()
        );
        assert_eq!(input.trim(), printed.trim());
    }
}
