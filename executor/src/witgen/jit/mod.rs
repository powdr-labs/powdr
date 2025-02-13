pub(crate) mod affine_symbolic_expression;
mod block_machine_processor;
mod compiler;
mod debug_formatter;
mod effect;
pub(crate) mod function_cache;
mod identity_queue;
mod interpreter;
mod processor;
mod prover_function_heuristics;
mod single_step_processor;
pub mod symbolic_expression;
mod variable;
pub(crate) mod witgen_inference;

#[cfg(test)]
pub(crate) mod test_util;
