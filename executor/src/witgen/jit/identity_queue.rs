use std::{
    collections::{BTreeSet, HashMap},
    rc::Rc,
};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{AlgebraicExpression as Expression, AlgebraicReferenceThin, PolynomialType},
    parsed::visitor::{AllChildren, Children},
};
use powdr_number::FieldElement;

use crate::witgen::{
    data_structures::identity::Identity, jit::variable::MachineCallVariable, FixedData,
};

use super::{
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
    ) -> Self {
        let queue: BTreeSet<_> = identities
            .iter()
            .map(|(id, row)| QueueItem::Identity(id, *row))
            .chain(assignments.iter().map(|a| QueueItem::Assignment(a.clone())))
            .collect();
        let occurrences = compute_occurrences_map(fixed_data, &queue).into();
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
}

/// Sorts identities by row and then by ID, preceded by assignments.
impl<T: FieldElement> Ord for QueueItem<'_, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (QueueItem::Identity(id1, row1), QueueItem::Identity(id2, row2)) => {
                (row1, id1.id()).cmp(&(row2, id2.id()))
            }
            (QueueItem::Assignment(a1), QueueItem::Assignment(a2)) => a1.cmp(a2),
            (QueueItem::Identity(_, _), QueueItem::Assignment(_)) => std::cmp::Ordering::Greater,
            (QueueItem::Assignment(_), QueueItem::Identity(_, _)) => std::cmp::Ordering::Less,
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

/// Computes a map from each variable to the queue items it occurs in.
fn compute_occurrences_map<'b, 'a: 'b, T: FieldElement>(
    fixed_data: &'a FixedData<'a, T>,
    items: &BTreeSet<QueueItem<'a, T>>,
) -> HashMap<Variable, Vec<QueueItem<'a, T>>> {
    let mut intermediate_cache = HashMap::new();

    // Compute references only once per identity.
    let mut references_per_identity = HashMap::new();
    for id in items
        .iter()
        .filter_map(|item| match item {
            QueueItem::Identity(id, _) => Some(id),
            _ => None,
        })
        .unique_by(|id| id.id())
    {
        references_per_identity.insert(
            id.id(),
            references_in_identity(id, fixed_data, &mut intermediate_cache),
        );
    }

    items
        .iter()
        .flat_map(|item| {
            let variables = match item {
                QueueItem::Identity(id, row) => {
                    let mut variables = references_per_identity[&id.id()]
                        .iter()
                        .map(|r| {
                            let name = fixed_data.column_name(&r.poly_id).to_string();
                            Variable::from_reference(&r.with_name(name), *row)
                        })
                        .collect_vec();
                    if let Identity::BusSend(bus_send) = id {
                        variables.extend((0..bus_send.selected_payload.expressions.len()).map(
                            |index| {
                                Variable::MachineCallParam(MachineCallVariable {
                                    identity_id: id.id(),
                                    row_offset: *row,
                                    index,
                                })
                            },
                        ));
                    };
                    variables
                }
                QueueItem::Assignment(a) => {
                    variables_in_assignment(a, fixed_data, &mut intermediate_cache)
                }
            };
            variables.into_iter().map(move |v| (v, item.clone()))
        })
        .into_group_map()
}

/// Returns all references to witness column in the identity.
fn references_in_identity<T: FieldElement>(
    identity: &Identity<T>,
    fixed_data: &FixedData<T>,
    intermediate_cache: &mut HashMap<AlgebraicReferenceThin, Vec<AlgebraicReferenceThin>>,
) -> Vec<AlgebraicReferenceThin> {
    let mut result = BTreeSet::new();

    match identity {
        Identity::BusSend(bus_send) => result.extend(references_in_expression(
            &bus_send.selected_payload.selector,
            fixed_data,
            intermediate_cache,
        )),
        _ => {
            for e in identity.children() {
                result.extend(references_in_expression(e, fixed_data, intermediate_cache));
            }
        }
    }

    result.into_iter().collect()
}

/// Recursively resolves references in intermediate column definitions.
fn references_in_intermediate<T: FieldElement>(
    fixed_data: &FixedData<T>,
    intermediate: &AlgebraicReferenceThin,
    intermediate_cache: &mut HashMap<AlgebraicReferenceThin, Vec<AlgebraicReferenceThin>>,
) -> Vec<AlgebraicReferenceThin> {
    if let Some(references) = intermediate_cache.get(intermediate) {
        return references.clone();
    }
    let references = references_in_expression(
        &fixed_data.intermediate_definitions[intermediate],
        fixed_data,
        intermediate_cache,
    )
    .collect_vec();
    intermediate_cache.insert(intermediate.clone(), references.clone());
    references
}

/// Returns all references to witness or intermediate column in the expression.
fn references_in_expression<'a, T: FieldElement>(
    expression: &'a Expression<T>,
    fixed_data: &'a FixedData<T>,
    intermediate_cache: &'a mut HashMap<AlgebraicReferenceThin, Vec<AlgebraicReferenceThin>>,
) -> impl Iterator<Item = AlgebraicReferenceThin> + 'a {
    expression
        .all_children()
        .flat_map(
            move |e| -> Box<dyn Iterator<Item = AlgebraicReferenceThin> + 'a> {
                match e {
                    Expression::Reference(r) => match r.poly_id.ptype {
                        PolynomialType::Constant => Box::new(std::iter::empty()),
                        PolynomialType::Committed => Box::new(std::iter::once(r.into())),
                        PolynomialType::Intermediate => Box::new(
                            references_in_intermediate(fixed_data, &r.into(), intermediate_cache)
                                .into_iter(),
                        ),
                    },
                    Expression::PublicReference(_) | Expression::Challenge(_) => {
                        // TODO we need to introduce a variable type for those.
                        Box::new(std::iter::empty())
                    }
                    _ => Box::new(std::iter::empty()),
                }
            },
        )
        .unique()
}

/// Returns a vector of all variables that occur in the assignment.
fn variables_in_assignment<'a, T: FieldElement>(
    assignment: &Assignment<'a, T>,
    fixed_data: &'a FixedData<'a, T>,
    intermediate_cache: &mut HashMap<AlgebraicReferenceThin, Vec<AlgebraicReferenceThin>>,
) -> Vec<Variable> {
    let rhs_var = match &assignment.rhs {
        VariableOrValue::Variable(v) => Some(v.clone()),
        VariableOrValue::Value(_) => None,
    };
    references_in_expression(assignment.lhs, fixed_data, intermediate_cache)
        .map(|r| {
            let name = fixed_data.column_name(&r.poly_id).to_string();
            Variable::from_reference(&r.with_name(name), assignment.row_offset)
        })
        .chain(rhs_var)
        .collect()
}
