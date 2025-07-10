mod block_machine_processor;
pub(crate) mod code_cleaner;
mod compiler;
mod debug_formatter;
mod effect;
pub(crate) mod function_cache;
mod identity_queue;
mod interpreter;
mod processor;
mod prover_function_heuristics;
mod single_step_processor;
mod variable;
pub(crate) mod witgen_inference;

type QuadraticSymbolicExpression<T, V> =
    powdr_constraint_solver::grouped_expression::GroupedExpression<
        powdr_constraint_solver::symbolic_expression::SymbolicExpression<T, V>,
        V,
    >;

#[cfg(test)]
pub(crate) mod test_util;
