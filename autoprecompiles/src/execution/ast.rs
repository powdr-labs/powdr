use std::iter;

use itertools::Itertools;
use powdr_expression::visitors::{AllChildren, Children};
use serde::{Deserialize, Serialize};

use crate::{execution::ExecutionState, powdr::UniqueReferences};

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
                    Some(*r)
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
    RegisterLimb(A, usize),
    Pc,
}

impl<A> From<LocalOptimisticLiteral<A>> for LocalFetch<A> {
    fn from(value: LocalOptimisticLiteral<A>) -> Self {
        match value {
            LocalOptimisticLiteral::RegisterLimb(a, _) => Self::Register(a),
            LocalOptimisticLiteral::Pc => Self::Pc,
        }
    }
}

#[derive(
    Debug, Clone, Copy, Serialize, Deserialize, deepsize2::DeepSizeOf, PartialEq, Eq, Hash,
)]
pub enum LocalFetch<A> {
    Register(A),
    Pc,
}

impl<A> LocalFetch<A> {
    pub fn get<E: ExecutionState<RegisterAddress = A>>(&self, state: &E) -> E::Value {
        match self {
            LocalFetch::Register(a) => state.reg(a),
            LocalFetch::Pc => state.pc(),
        }
    }
}

#[derive(
    Debug, Clone, Copy, Serialize, Deserialize, deepsize2::DeepSizeOf, PartialEq, Eq, Hash,
)]
pub struct Fetch<A> {
    pub instr_idx: usize,
    pub val: LocalFetch<A>,
}

impl<A> From<OptimisticLiteral<A>> for Fetch<A> {
    fn from(value: OptimisticLiteral<A>) -> Self {
        Self {
            instr_idx: value.instr_idx,
            val: value.val.into(),
        }
    }
}

#[derive(
    Debug, Clone, Copy, Serialize, Deserialize, deepsize2::DeepSizeOf, PartialEq, Eq, Hash,
)]
pub struct OptimisticLiteral<A> {
    pub instr_idx: usize,
    pub val: LocalOptimisticLiteral<A>,
}
