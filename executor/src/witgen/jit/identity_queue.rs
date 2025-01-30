use std::{
    collections::{BTreeSet, HashMap},
    rc::Rc,
};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{
        AlgebraicExpression as Expression, AlgebraicReference, AlgebraicReferenceThin,
        PolynomialType,
    },
    parsed::visitor::{AllChildren, Children},
};
use powdr_number::FieldElement;

use crate::witgen::{data_structures::identity::Identity, FixedData};

use super::variable::Variable;

/// Keeps track of identities that still need to be processed and
/// updates this list based on the occurrence of updated variables
/// in identities.
#[derive(Clone)]
pub struct IdentityQueue<'a, T: FieldElement> {
    queue: BTreeSet<QueueItem<'a, T>>,
    occurrences: Rc<HashMap<Variable, Vec<QueueItem<'a, T>>>>,
}

impl<'a, T: FieldElement> IdentityQueue<'a, T> {
    pub fn new(fixed_data: &'a FixedData<'a, T>, identities: &[(&'a Identity<T>, i32)]) -> Self {
        let occurrences = compute_occurrences_map(fixed_data, identities).into();
        Self {
            queue: identities
                .iter()
                .map(|(id, row)| QueueItem(id, *row))
                .collect(),
            occurrences,
        }
    }

    /// Returns the next identity to be processed and its row and
    /// removes it from the queue.
    pub fn next(&mut self) -> Option<(&'a Identity<T>, i32)> {
        self.queue.pop_first().map(|QueueItem(id, row)| (id, row))
    }

    pub fn variables_updated(
        &mut self,
        variables: impl IntoIterator<Item = Variable>,
        skip_identity: Option<(&'a Identity<T>, i32)>,
    ) {
        self.queue.extend(
            variables
                .into_iter()
                .flat_map(|var| self.occurrences.get(&var))
                .flatten()
                .filter(|QueueItem(id, row)| match skip_identity {
                    Some((id2, row2)) => (id.id(), *row) != (id2.id(), row2),
                    None => true,
                }),
        )
    }
}

/// Sorts identities by row and then by ID.
#[derive(Clone, Copy)]
struct QueueItem<'a, T>(&'a Identity<T>, i32);

impl<T> QueueItem<'_, T> {
    fn key(&self) -> (i32, u64) {
        let QueueItem(id, row) = self;
        (*row, id.id())
    }
}

impl<T> Ord for QueueItem<'_, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.key().cmp(&other.key())
    }
}

impl<T> PartialOrd for QueueItem<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> PartialEq for QueueItem<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key()
    }
}

impl<T> Eq for QueueItem<'_, T> {}

/// Computes a map from each variable to the identity-row-offset pairs it occurs in.
fn compute_occurrences_map<'a, T: FieldElement>(
    fixed_data: &'a FixedData<'a, T>,
    identities: &[(&'a Identity<T>, i32)],
) -> HashMap<Variable, Vec<QueueItem<'a, T>>> {
    let mut references_per_identity = HashMap::new();
    let mut intermediate_cache = HashMap::new();
    for id in identities.iter().map(|(id, _)| *id).unique_by(|id| id.id()) {
        references_per_identity.insert(
            id,
            references_in_identity(id, fixed_data, &mut intermediate_cache),
        );
    }
    identities
        .iter()
        .flat_map(|(id, row)| {
            references_per_identity[id].iter().map(move |reference| {
                let name = fixed_data.column_name(&reference.poly_id).to_string();
                let fat_ref = AlgebraicReference {
                    name,
                    poly_id: reference.poly_id,
                    next: reference.next,
                };
                let var = Variable::from_reference(&fat_ref, *row);
                (var, QueueItem(*id, *row))
            })
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
    for e in identity.children() {
        result.extend(references_in_expression(e, fixed_data, intermediate_cache));
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
