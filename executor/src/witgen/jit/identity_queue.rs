use std::{
    collections::{BTreeSet, HashMap},
    rc::Rc,
};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{
        AlgebraicExpression as Expression, AlgebraicReferenceThin, PolynomialIdentity,
        PolynomialType,
    },
    parsed::visitor::{AllChildren, Children},
};
use powdr_number::FieldElement;

use crate::witgen::{
    data_structures::identity::Identity, jit::variable::MachineCallVariable, FixedData,
};

use super::{prover_function_heuristics::ProverFunction, variable::Variable};

/// Keeps track of identities that still need to be processed and
/// updates this list based on the occurrence of updated variables
/// in identities.
#[derive(Clone)]
pub struct IdentityQueue<'a, T: FieldElement> {
    queue: BTreeSet<QueueItem<'a, T>>,
    occurrences: Rc<HashMap<Variable<T>, Vec<QueueItem<'a, T>>>>,
}

impl<'a, T: FieldElement> IdentityQueue<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        items: impl IntoIterator<Item = QueueItem<'a, T>>,
    ) -> Self {
        let queue: BTreeSet<_> = items.into_iter().collect();
        let mut references = ReferencesComputer::new(fixed_data);
        let occurrences = Rc::new(
            queue
                .iter()
                .flat_map(|item| {
                    references
                        .references(item)
                        .iter()
                        .map(|v| (v.clone(), item.clone()))
                        .collect_vec()
                })
                .into_group_map(),
        );
        Self { queue, occurrences }
    }

    /// Returns the next identity to be processed and its row and
    /// removes it from the queue.
    pub fn next(&mut self) -> Option<QueueItem<'a, T>> {
        self.queue.pop_first()
    }

    pub fn variables_updated(&mut self, variables: impl IntoIterator<Item = Variable<T>>) {
        // Note that this will usually re-add the item that caused the update,
        // which is fine, since there are situations where we can further process
        // it from an update (for example a range constraint).
        self.queue.extend(
            variables
                .into_iter()
                .flat_map(|var| self.occurrences.get(&var))
                .flatten()
                .cloned(),
        )
    }
}

#[derive(Clone)]
pub enum QueueItem<'a, T: FieldElement> {
    Identity(&'a Identity<T>, i32),
    VariableAssignment(VariableAssignment<'a, T>),
    ConstantAssignment(ConstantAssignment<'a, T>),
    ProverFunction(ProverFunction<'a, T>, i32),
}

impl<T: FieldElement> Ord for QueueItem<'_, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (QueueItem::Identity(id1, row1), QueueItem::Identity(id2, row2)) => {
                (row1, id1.id()).cmp(&(row2, id2.id()))
            }
            (QueueItem::VariableAssignment(a1), QueueItem::VariableAssignment(a2)) => a1.cmp(a2),
            (QueueItem::ConstantAssignment(a1), QueueItem::ConstantAssignment(a2)) => a1.cmp(a2),
            (QueueItem::ProverFunction(p1, row1), QueueItem::ProverFunction(p2, row2)) => {
                (row1, p1.index).cmp(&(row2, p2.index))
            }
            (a, b) => a.order().cmp(&b.order()),
        }
    }
}

impl<'a, T: FieldElement> QueueItem<'a, T> {
    pub fn constant_assignment(lhs: &'a Expression<T>, rhs: T, row_offset: i32) -> Self {
        QueueItem::ConstantAssignment(ConstantAssignment {
            lhs,
            row_offset,
            rhs,
        })
    }

    pub fn variable_assignment(lhs: &'a Expression<T>, rhs: Variable<T>, row_offset: i32) -> Self {
        QueueItem::VariableAssignment(VariableAssignment {
            lhs,
            row_offset,
            rhs,
        })
    }

    fn order(&self) -> u32 {
        match self {
            QueueItem::ConstantAssignment(..) => 0,
            QueueItem::VariableAssignment(..) => 1,
            QueueItem::Identity(..) => 2,
            QueueItem::ProverFunction(..) => 3,
        }
    }
}

impl<T: FieldElement> PartialOrd for QueueItem<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: FieldElement> PartialEq for QueueItem<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == std::cmp::Ordering::Equal
    }
}

impl<T: FieldElement> Eq for QueueItem<'_, T> {}

/// An equality constraint between an algebraic expression evaluated
/// on a certain row offset and a variable.
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
pub struct VariableAssignment<'a, T: FieldElement> {
    pub lhs: &'a Expression<T>,
    pub row_offset: i32,
    pub rhs: Variable<T>,
}

/// An equality constraint between an algebraic expression evaluated
/// on a certain row offset and a constant.
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
pub struct ConstantAssignment<'a, T: FieldElement> {
    pub lhs: &'a Expression<T>,
    pub row_offset: i32,
    pub rhs: T,
}

/// Utility to compute the variables that occur in a queue item.
/// Follows intermediate column references and employs caches.
struct ReferencesComputer<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    intermediate_cache: HashMap<AlgebraicReferenceThin, Vec<AlgebraicReferenceThin>>,
    /// A cache to store algebraic references in a polynomial identity, so that it
    /// can be re-used on all rows.
    references_per_identity: HashMap<u64, Vec<AlgebraicReferenceThin>>,
}

impl<'a, T: FieldElement> ReferencesComputer<'a, T> {
    pub fn new(fixed_data: &'a FixedData<'a, T>) -> Self {
        Self {
            fixed_data,
            intermediate_cache: HashMap::new(),
            references_per_identity: HashMap::new(),
        }
    }
    pub fn references(&mut self, item: &QueueItem<'a, T>) -> Vec<Variable<T>> {
        let vars: Box<dyn Iterator<Item = _>> = match item {
            QueueItem::Identity(id, row) => match id {
                Identity::Polynomial(poly_id) => Box::new(
                    self.references_in_polynomial_identity(poly_id)
                        .into_iter()
                        .map(|r| self.reference_to_variable(&r, *row)),
                ),
                Identity::BusSend(bus_send) => Box::new(
                    self.variables_in_expression(&bus_send.selected_payload.selector, *row)
                        .into_iter()
                        .chain(
                            (0..bus_send.selected_payload.expressions.len()).map(|index| {
                                Variable::MachineCallParam(MachineCallVariable {
                                    identity_id: bus_send.identity_id,
                                    index,
                                    row_offset: *row,
                                    _phantom: Default::default(),
                                })
                            }),
                        ),
                ),
                Identity::Connect(..) => Box::new(std::iter::empty()),
            },
            QueueItem::ConstantAssignment(a) => Box::new(
                self.variables_in_expression(a.lhs, a.row_offset)
                    .into_iter(),
            ),
            QueueItem::VariableAssignment(a) => Box::new(
                std::iter::once(a.rhs.clone())
                    .chain(self.variables_in_expression(a.lhs, a.row_offset)),
            ),
            QueueItem::ProverFunction(p, row) => Box::new(
                p.condition
                    .iter()
                    .flat_map(|c| self.variables_in_expression(c, *row))
                    .chain(
                        p.input_columns
                            .iter()
                            .map(|r| Variable::from_reference(r, *row)),
                    ),
            ),
        };
        vars.unique().collect_vec()
    }

    fn variables_in_expression(&mut self, expression: &Expression<T>, row: i32) -> Vec<Variable<T>> {
        self.references_in_expression(expression)
            .iter()
            .map(|r| {
                let name = self.fixed_data.column_name(&r.poly_id).to_string();
                Variable::from_reference(&r.with_name(name), row)
            })
            .collect()
    }

    /// Turns AlgebraicReferenceThin to Variable, by including the row offset.
    fn reference_to_variable(&self, reference: &AlgebraicReferenceThin, row: i32) -> Variable<T> {
        let name = self.fixed_data.column_name(&reference.poly_id).to_string();
        Variable::from_reference(&reference.with_name(name), row)
    }

    fn references_in_polynomial_identity(
        &mut self,
        identity: &PolynomialIdentity<T>,
    ) -> Vec<AlgebraicReferenceThin> {
        // Clippy suggests to use `entry()...or_insert_with()`,
        // but the code does not work, since we need `&mut self` in
        // self.references_in_expression.
        #[allow(clippy::map_entry)]
        if !self.references_per_identity.contains_key(&identity.id) {
            let mut result = BTreeSet::new();
            for e in identity.children() {
                result.extend(self.references_in_expression(e));
            }
            self.references_per_identity
                .insert(identity.id, result.into_iter().collect_vec());
        }
        self.references_per_identity[&identity.id].clone()
    }

    /// Returns all references to witness column in the expression, including indirect
    /// references through intermediate columns.
    fn references_in_expression(
        &mut self,
        expression: &Expression<T>,
    ) -> Vec<AlgebraicReferenceThin> {
        let mut references = BTreeSet::new();
        for e in expression.all_children() {
            match e {
                Expression::Reference(r) => match r.poly_id.ptype {
                    PolynomialType::Constant => {}
                    PolynomialType::Committed => {
                        references.insert(r.into());
                    }
                    PolynomialType::Intermediate => references
                        .extend(self.references_in_intermediate(&r.into()).iter().cloned()),
                },
                Expression::PublicReference(_) | Expression::Challenge(_) => {
                    // TODO we need to introduce a variable type for those.
                }
                Expression::Number(_)
                | Expression::BinaryOperation(..)
                | Expression::UnaryOperation(..) => {}
            }
        }
        references.into_iter().collect()
    }

    fn references_in_intermediate(
        &mut self,
        intermediate: &AlgebraicReferenceThin,
    ) -> &Vec<AlgebraicReferenceThin> {
        if !self.intermediate_cache.contains_key(intermediate) {
            let definition = &self.fixed_data.intermediate_definitions[intermediate];
            let references = self.references_in_expression(definition);
            self.intermediate_cache
                .insert(intermediate.clone(), references.clone());
        }
        &self.intermediate_cache[intermediate]
    }
}
