use std::iter;

use itertools::Itertools;
use powdr_expression::visitors::{AllChildren, Children};
use serde::{Deserialize, Serialize};

use crate::powdr::UniqueReferences;

#[derive(Debug, Serialize, Deserialize, deepsize2::DeepSizeOf, PartialEq, Eq, Clone)]
pub struct OptimisticConstraint<A, V> {
    pub left: OptimisticExpression<A, V>,
    pub right: OptimisticExpression<A, V>,
}

impl<A, V> Children<OptimisticExpression<A, V>> for OptimisticConstraint<A, V> {
    fn children(&self) -> Box<dyn Iterator<Item = &OptimisticExpression<A, V>> + '_> {
        Box::new([&self.left, &self.right].into_iter())
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut OptimisticExpression<A, V>> + '_> {
        Box::new([&mut self.left, &mut self.right].into_iter())
    }
}

impl<
        'a,
        A: 'a + Copy + PartialEq + Eq + std::hash::Hash,
        V: 'a,
        E: AllChildren<OptimisticExpression<A, V>>,
    > UniqueReferences<'a, (A, V), OptimisticLiteral<A>> for E
{
    fn unique_references(&'a self) -> impl Iterator<Item = OptimisticLiteral<A>> {
        self.all_children()
            .filter_map(|e| {
                if let OptimisticExpression::Literal(r) = e {
                    Some(r.clone())
                } else {
                    None
                }
            })
            .unique()
    }
}

impl<A, V> AllChildren<OptimisticExpression<A, V>> for OptimisticExpression<A, V> {
    fn all_children(&self) -> Box<dyn Iterator<Item = &OptimisticExpression<A, V>> + '_> {
        Box::new(iter::once(self).chain(self.children().flat_map(|e| e.all_children())))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, deepsize2::DeepSizeOf, PartialEq, Eq)]
pub enum OptimisticExpression<A, V> {
    Number(V),
    Literal(OptimisticLiteral<A>),
}

impl<A, V> OptimisticExpression<A, V> {
    fn children(&self) -> Box<dyn Iterator<Item = &OptimisticExpression<A, V>> + '_> {
        match self {
            OptimisticExpression::Literal(_) | OptimisticExpression::Number(_) => {
                Box::new(iter::empty())
            }
        }
    }
}

#[derive(
    Debug, Clone, Copy, Serialize, Deserialize, deepsize2::DeepSizeOf, PartialEq, Eq, Hash,
)]
pub enum LocalOptimisticLiteral<A> {
    /// A limb of a register
    // TODO: The code below ignores the limb index; support it properly
    // Search for "TODO: Support limb accesses"
    RegisterLimb(A, usize),
    Pc,
}

#[derive(Debug, Clone, Serialize, Deserialize, deepsize2::DeepSizeOf, PartialEq, Eq, Hash)]
pub struct OptimisticLiteral<A> {
    pub instr_idx: usize,
    pub val: LocalOptimisticLiteral<A>,
}
