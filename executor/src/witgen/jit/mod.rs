pub(crate) mod affine_symbolic_expression;
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
pub(crate) mod quadratic_symbolic_expression;
mod single_step_processor;
mod symbolic_expression;
mod variable;
pub(crate) mod witgen_inference;

#[cfg(test)]
pub(crate) mod test_util;
