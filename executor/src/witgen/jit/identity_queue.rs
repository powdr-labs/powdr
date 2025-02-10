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

use super::{
    prover_function_heuristics::ProverFunction,
    variable::Variable,
    witgen_inference::{Assignment, VariableOrValue},
};

/// Keeps track of identities that still need to be processed and
/// updates this list based on the occurrence of updated variables
/// in identities.
#[derive(Clone)]
pub struct IdentityQueue<'a, T: FieldElement> {
    queue: BTreeSet<QueueItem<'a, T>>,
    occurrences: Rc<HashMap<Variable, Vec<QueueItem<'a, T>>>>,
}

impl<'a, T: FieldElement> IdentityQueue<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        identities: &[(&'a Identity<T>, i32)],
        assignments: &[Assignment<'a, T>],
        prover_functions: &[(ProverFunction<'a, T>, i32)],
    ) -> Self {
        let queue: BTreeSet<_> = identities
            .iter()
            .map(|(id, row)| QueueItem::Identity(id, *row))
            .chain(assignments.iter().map(|a| QueueItem::Assignment(a.clone())))
            .chain(
                prover_functions
                    .iter()
                    .map(|(p, row)| QueueItem::ProverFunction(p.clone(), *row)),
            )
            .collect();
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

    pub fn variables_updated(
        &mut self,
        variables: impl IntoIterator<Item = Variable>,
        skip_item: Option<QueueItem<'a, T>>,
    ) {
        self.queue.extend(
            variables
                .into_iter()
                .flat_map(|var| self.occurrences.get(&var))
                .flatten()
                .filter(|item| match &skip_item {
                    Some(it) => *item != it,
                    None => true,
                })
                .cloned(),
        )
    }
}

#[derive(Clone)]
pub enum QueueItem<'a, T: FieldElement> {
    Identity(&'a Identity<T>, i32),
    Assignment(Assignment<'a, T>),
    ProverFunction(ProverFunction<'a, T>, i32),
}

/// Sorts identities by row and then by ID, preceded by assignments.
impl<T: FieldElement> Ord for QueueItem<'_, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (QueueItem::Identity(id1, row1), QueueItem::Identity(id2, row2)) => {
                (row1, id1.id()).cmp(&(row2, id2.id()))
            }
            (QueueItem::Assignment(a1), QueueItem::Assignment(a2)) => a1.cmp(a2),
            (QueueItem::ProverFunction(p1, row1), QueueItem::ProverFunction(p2, row2)) => {
                (row1, p1.index).cmp(&(row2, p2.index))
            }
            (QueueItem::Assignment(..), _) => std::cmp::Ordering::Less,
            (QueueItem::Identity(..), QueueItem::Assignment(..)) => std::cmp::Ordering::Greater,
            (QueueItem::Identity(..), QueueItem::ProverFunction(..)) => std::cmp::Ordering::Less,
            (QueueItem::ProverFunction(..), _) => std::cmp::Ordering::Greater,
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
    pub fn references(&mut self, item: &QueueItem<'a, T>) -> Vec<Variable> {
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
                                })
                            }),
                        ),
                ),
                Identity::Connect(..) => Box::new(std::iter::empty()),
            },
            QueueItem::Assignment(a) => {
                let vars_in_rhs = match &a.rhs {
                    VariableOrValue::Variable(v) => Some(v.clone()),
                    VariableOrValue::Value(_) => None,
                };
                Box::new(
                    self.variables_in_expression(a.lhs, a.row_offset)
                        .into_iter()
                        .chain(vars_in_rhs),
                )
            }
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

    fn variables_in_expression(&mut self, expression: &Expression<T>, row: i32) -> Vec<Variable> {
        self.references_in_expression(expression)
            .iter()
            .map(|r| {
                let name = self.fixed_data.column_name(&r.poly_id).to_string();
                Variable::from_reference(&r.with_name(name), row)
            })
            .collect()
    }

    /// Turns AlgebraicReferenceThin to Variable, by including the row offset.
    fn reference_to_variable(&self, reference: &AlgebraicReferenceThin, row: i32) -> Variable {
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
