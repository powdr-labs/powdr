use std::ops::ControlFlow;

use crate::parsed::{utils::previsit_expression_mut, Expression};

use super::FunctionStatement;

/// Traverses the expression tree and calls `f` in pre-order.
pub fn previsit_expression_in_statement_mut<T, F, B>(
    s: &mut FunctionStatement<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    match s {
        FunctionStatement::Assignment(assignment) => {
            previsit_expression_mut(assignment.rhs.as_mut(), f)?;
        }
        FunctionStatement::Instruction(instruction) => {
            for i in &mut instruction.inputs {
                previsit_expression_mut(i, f)?;
            }
        }
        FunctionStatement::Label(_) | FunctionStatement::DebugDirective(..) => {}
        FunctionStatement::Return(ret) => {
            for e in &mut ret.values {
                previsit_expression_mut(e, f);
            }
        }
    }

    ControlFlow::Continue(())
}
