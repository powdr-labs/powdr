pub(crate) mod affine_symbolic_expression;
mod block_machine_processor;
mod compiler;
pub(crate) mod jit_processor;
mod symbolic_expression;
mod variable;
pub(crate) mod witgen_inference;

#[cfg(test)]
pub(crate) mod test_util;
