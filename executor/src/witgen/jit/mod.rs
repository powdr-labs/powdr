pub(crate) mod affine_symbolic_expression;
mod block_machine_processor;
mod compiler;
mod effect;
pub(crate) mod function_cache;
mod symbolic_expression;
mod variable;
pub(crate) mod witgen_inference;

mod single_step_processor;
#[cfg(test)]
pub(crate) mod test_util;
mod single_step_processor;
