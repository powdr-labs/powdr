use powdr_ast::parsed;
use powdr_number::{BigInt, GoldilocksField};

use crate::{
    evaluator::{self, EvalError},
    expression_processor::ExpressionProcessor,
    AnalysisDriver,
};

// TODO This should maybe be implement as a separate evaluator that is able to
// safely run before type checking and is field-independent (only uses integers)?

/// Evaluates an expression that has not been type-checked.
/// This is mainly used to evaluate array lengths in types and namespace degrees.
pub fn evaluate_expression_to_int(
    driver: impl AnalysisDriver,
    expr: parsed::Expression,
) -> Result<BigInt, EvalError> {
    evaluator::evaluate_expression::<GoldilocksField>(
        &ExpressionProcessor::new(driver).process_expression(expr),
        driver.definitions(),
    )?
    .try_to_integer()
}
