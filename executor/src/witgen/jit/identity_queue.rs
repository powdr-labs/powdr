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

/// Constructs a `ProcessingQueue` for pairs of identities and rows.
pub fn identity_queue<'a, T: FieldElement>(
    fixed_data: &'a FixedData<'a, T>,
    identities: &[(&'a Identity<T>, i32)],
) -> ProcessingQueue<IdentityQueueItem<'a, T>> {
    ProcessingQueue::new(
        identities.iter().cloned(),
        compute_occurrences_map(fixed_data, identities),
    )
}

/// Keeps track of identities or assignments that still need to be processed and
/// updates this list based on the occurrence of updated variables
/// in identities and assignments.
#[derive(Clone)]
pub struct ProcessingQueue<Item: QueueItem> {
    queue: BTreeSet<Item>,
    occurrences: Rc<HashMap<Variable, Vec<Item>>>,
}

impl<Item: QueueItem> ProcessingQueue<Item> {
    pub fn new(
        queue: impl IntoIterator<Item = Item::OuterType>,
        occurrences: impl IntoIterator<Item = (Variable, Item::OuterType)>,
    ) -> Self {
        Self {
            queue: queue.into_iter().map(Item::from_outer_type).collect(),
            occurrences: Rc::new(
                occurrences
                    .into_iter()
                    .map(|(v, i)| (v, Item::from_outer_type(i)))
                    .into_group_map(),
            ),
        }
    }

    /// Returns the next identity or assignment to be processed and its row and
    /// removes it from the queue.
    pub fn next(&mut self) -> Option<Item::OuterType> {
        self.queue.pop_first().map(|i| i.to_outer_type())
    }

    pub fn variables_updated(
        &mut self,
        variables: impl IntoIterator<Item = Variable>,
        skip_identity: Option<Item::OuterType>,
    ) {
        let skip_item = skip_identity.map(Item::from_outer_type);
        self.queue.extend(
            variables
                .into_iter()
                .flat_map(|var| self.occurrences.get(&var))
                .flatten()
                .filter(|&item| match &skip_item {
                    Some(skip_item) => item != skip_item,
                    None => true,
                })
                .cloned(),
        )
    }
}

pub trait QueueItem: Clone + Ord + PartialEq {
    type OuterType;
    fn to_outer_type(&self) -> Self::OuterType;
    fn from_outer_type(outer: Self::OuterType) -> Self;
}

/// Sorts identities by row and then by ID.
#[derive(Clone, Copy)]
pub struct IdentityQueueItem<'a, T>(&'a Identity<T>, i32);

impl<'a, T: Clone> QueueItem for IdentityQueueItem<'a, T> {
    type OuterType = (&'a Identity<T>, i32);

    fn to_outer_type(&self) -> Self::OuterType {
        (self.0, self.1)
    }

    fn from_outer_type(outer: Self::OuterType) -> Self {
        IdentityQueueItem(outer.0, outer.1)
    }
}

impl<T> IdentityQueueItem<'_, T> {
    fn key(&self) -> (i32, u64) {
        let IdentityQueueItem(id, row) = self;
        (*row, id.id())
    }
}

impl<T> Ord for IdentityQueueItem<'_, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.key().cmp(&other.key())
    }
}

impl<T> PartialOrd for IdentityQueueItem<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> PartialEq for IdentityQueueItem<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key()
    }
}

impl<T> Eq for IdentityQueueItem<'_, T> {}

/// Computes a map from each variable to the identity-row-offset pairs it occurs in.
fn compute_occurrences_map<'a, T: FieldElement>(
    fixed_data: &'a FixedData<'a, T>,
    identities: &[(&'a Identity<T>, i32)],
) -> Vec<(Variable, (&'a Identity<T>, i32))> {
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
                (var, (*id, *row))
            })
        })
        .collect()
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
