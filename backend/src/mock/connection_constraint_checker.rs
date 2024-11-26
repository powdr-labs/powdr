use std::collections::BTreeMap;
use std::collections::HashSet;
use std::fmt;
use std::hash::Hash;
use std::ops::ControlFlow;

use multiset::HashMultiSet;
use powdr_ast::analyzed::AlgebraicExpression;
use powdr_ast::analyzed::Analyzed;
use powdr_ast::analyzed::{
    Identity, LookupIdentity, PermutationIdentity, PhantomLookupIdentity,
    PhantomPermutationIdentity, SelectedExpressions,
};
use powdr_ast::parsed::visitor::ExpressionVisitable;
use powdr_ast::parsed::visitor::VisitOrder;
use powdr_backend_utils::referenced_namespaces_algebraic_expression;
use powdr_executor::witgen::AffineExpression;
use powdr_executor::witgen::ExpressionEvaluator;
use powdr_number::FieldElement;
use rayon::iter::IntoParallelIterator;
use rayon::iter::ParallelIterator;

use super::evaluator::Variables;
use super::machine::Machine;

pub enum ConnectionKind {
    Lookup,
    Permutation,
}

/// A connection between two machines.
pub struct Connection<F> {
    identity: Identity<F>,
    pub left: SelectedExpressions<F>,
    pub right: SelectedExpressions<F>,
    /// For [ConnectionKind::Permutation], rows of `left` are a permutation of rows of `right`. For [ConnectionKind::Lookup], all rows in `left` are in `right`.
    pub kind: ConnectionKind,
}

impl<F: FieldElement> Connection<F> {
    /// Extracts all connections from the global PIL.
    pub fn get_all(
        global_pil: &Analyzed<F>,
        machine_to_pil: &BTreeMap<String, Analyzed<F>>,
    ) -> Vec<Self> {
        global_pil
            .identities
            .iter()
            .filter_map(|identity| Connection::try_new(identity, global_pil, machine_to_pil).ok())
            .collect()
    }

    fn try_new(
        identity: &Identity<F>,
        global_pil: &Analyzed<F>,
        machine_to_pil: &BTreeMap<String, Analyzed<F>>,
    ) -> Result<Self, ()> {
        let (left, right, kind) = match identity {
            Identity::Polynomial(_) => Err(()),
            Identity::Connect(_) => unimplemented!(),
            Identity::Lookup(LookupIdentity { left, right, .. })
            | Identity::PhantomLookup(PhantomLookupIdentity { left, right, .. }) => {
                Ok((left.clone(), right.clone(), ConnectionKind::Lookup))
            }
            Identity::Permutation(PermutationIdentity { left, right, .. })
            | Identity::PhantomPermutation(PhantomPermutationIdentity { left, right, .. }) => {
                Ok((left.clone(), right.clone(), ConnectionKind::Permutation))
            }
        }?;

        // This connection is not localized yet: Its expression's PolyIDs point to the global PIL, not the local PIL.
        let connection_global = Self {
            identity: identity.clone(),
            left,
            right,
            kind,
        };
        let caller = connection_global.caller();
        let left = connection_global.localize(
            &connection_global.left,
            global_pil,
            &machine_to_pil[&caller],
        );
        let callee = connection_global.callee();
        let right = connection_global.localize(
            &connection_global.right,
            global_pil,
            &machine_to_pil[&callee],
        );

        Ok(Self {
            left,
            right,
            ..connection_global
        })
    }

    /// Translates PolyIDs pointing to columns in the global PIL to PolyIDs pointing to columns in the local PIL.
    fn localize(
        &self,
        selected_expressions: &SelectedExpressions<F>,
        global_pil: &Analyzed<F>,
        local_pil: &Analyzed<F>,
    ) -> SelectedExpressions<F> {
        // Build a map (local ID) -> (global ID).
        let name_to_id_local = local_pil.name_to_poly_id();
        let id_map = global_pil
            .name_to_poly_id()
            .into_iter()
            .filter_map(|(name, source_id)| {
                name_to_id_local
                    .get(&name)
                    .map(|target_id| (source_id, *target_id))
            })
            .collect::<BTreeMap<_, _>>();

        // Translate all polynomial references.
        let mut localized = selected_expressions.clone();
        localized.visit_expressions_mut(
            &mut |expr| {
                if let AlgebraicExpression::Reference(reference) = expr {
                    reference.poly_id = id_map[&reference.poly_id];
                }
                ControlFlow::Continue::<()>(())
            },
            VisitOrder::Pre,
        );

        localized
    }
}

fn unique_referenced_namespaces<F: FieldElement>(
    selected_expressions: &SelectedExpressions<F>,
) -> String {
    let all_namespaces = referenced_namespaces_algebraic_expression(selected_expressions);
    assert_eq!(
        all_namespaces.len(),
        1,
        "Expected exactly one namespace, got: {all_namespaces:?}",
    );
    all_namespaces.into_iter().next().unwrap()
}

/// A connection between two machines.
impl<F: FieldElement> Connection<F> {
    pub fn caller(&self) -> String {
        unique_referenced_namespaces(&self.left)
    }

    pub fn callee(&self) -> String {
        unique_referenced_namespaces(&self.right)
    }
}

pub struct ConnectionConstraintChecker<'a, F: FieldElement> {
    pub connections: &'a [Connection<F>],
    pub machines: BTreeMap<String, Machine<'a, F>>,
}

impl<'a, F: FieldElement> ConnectionConstraintChecker<'a, F> {
    /// Checks all connections.
    pub fn check(&self) -> Result<(), FailingConnectionConstraints<'a, F>> {
        let errors = self
            .connections
            .iter()
            .filter_map(|connection| self.check_connection(connection).err())
            .collect::<Vec<_>>();

        (!errors.is_empty())
            .then(|| {
                let error = FailingConnectionConstraints {
                    connection_count: self.connections.len(),
                    errors,
                };
                log::error!("{}", error);
                Err(error)
            })
            .unwrap_or(Ok(()))
    }

    /// Checks a single connection.
    fn check_connection(
        &self,
        connection: &'a Connection<F>,
    ) -> Result<(), FailingConnectionConstraint<'a, F>> {
        let caller_set = self.selected_tuples(&connection.caller(), &connection.left);
        let callee_set = self.selected_tuples(&connection.callee(), &connection.right);

        match connection.kind {
            ConnectionKind::Lookup => {
                // Check if $caller \subseteq callee$.
                let caller_set = caller_set.into_iter().collect::<HashSet<_>>();
                let callee_set = callee_set.into_iter().collect::<HashSet<_>>();
                let not_in_callee = caller_set
                    .difference(&callee_set)
                    .cloned()
                    .collect::<Vec<_>>();
                if !not_in_callee.is_empty() {
                    Err(FailingConnectionConstraint {
                        connection,
                        not_in_callee,
                        not_in_caller: Vec::new(),
                    })
                } else {
                    Ok(())
                }
            }
            ConnectionKind::Permutation => {
                // Check if $caller = callee$ (as multi-set).
                let caller_set = caller_set.into_iter().collect::<HashMultiSet<_>>();
                let callee_set = callee_set.into_iter().collect::<HashMultiSet<_>>();
                let is_equal = caller_set == callee_set;

                // Find the tuples that are in one set, but not in the other.
                // Note that both `not_in_caller` and `not_in_callee` might actually be empty,
                // if `caller_set` and `callee_set` are equal as sets but not as multi-sets.
                let caller_set = caller_set.distinct_elements().collect::<HashSet<_>>();
                let callee_set = callee_set.distinct_elements().collect::<HashSet<_>>();
                let not_in_caller = callee_set.difference(&caller_set).collect::<Vec<_>>();
                let not_in_callee = caller_set.difference(&callee_set).collect::<Vec<_>>();

                if !is_equal {
                    Err(FailingConnectionConstraint {
                        connection,
                        not_in_caller: not_in_caller.into_iter().cloned().cloned().collect(),
                        not_in_callee: not_in_callee.into_iter().cloned().cloned().collect(),
                    })
                } else {
                    Ok(())
                }
            }
        }
    }

    /// Returns the set of all selected tuples for a given machine.
    fn selected_tuples(
        &self,
        machine_name: &str,
        selected_expressions: &SelectedExpressions<F>,
    ) -> Vec<Tuple<F>> {
        let machine = &self.machines[machine_name];

        (0..machine.size)
            .into_par_iter()
            .filter_map(|row| {
                let variables = Variables { machine, row };
                let mut evaluator =
                    ExpressionEvaluator::new(&variables, &machine.intermediate_definitions);
                let result = evaluator.evaluate(&selected_expressions.selector).unwrap();
                let result = match result {
                    AffineExpression::Constant(c) => c,
                    _ => unreachable!("Unexpected result: {:?}", result),
                };

                assert!(result.is_zero() || result.is_one(), "Non-binary selector");
                result.is_one().then(|| {
                    let values = selected_expressions
                        .expressions
                        .iter()
                        .map(|expression| {
                            let result = evaluator.evaluate(expression).unwrap();
                            match result {
                                AffineExpression::Constant(c) => c,
                                _ => unreachable!("Unexpected result: {:?}", result),
                            }
                        })
                        .collect::<Vec<_>>();
                    Tuple { values, row }
                })
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
/// A tuple of field elements.
pub struct Tuple<F> {
    /// The values of the tuple.
    values: Vec<F>,
    /// The row in the machine where this tuple is located.
    /// Note that this value is only informational and is *not* used for equality or hashing.
    row: usize,
}

impl<F: PartialEq> PartialEq for Tuple<F> {
    fn eq(&self, other: &Self) -> bool {
        self.values == other.values
    }
}

impl<F: PartialEq> Eq for Tuple<F> {}

impl<F: Hash> Hash for Tuple<F> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.values.hash(state);
    }
}

impl<F: fmt::Display> fmt::Display for Tuple<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let values_str = self
            .values
            .iter()
            .map(|value| value.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "Row {}: ({})", self.row, values_str)
    }
}

pub struct FailingConnectionConstraint<'a, F> {
    /// The connection that failed.
    connection: &'a Connection<F>,

    /// Tuples that are in the callee, but not in the caller.
    /// For [ConnectionKind::Lookup], this is irrelevant and we'll store an empty vector here.
    not_in_caller: Vec<Tuple<F>>,

    /// Tuples that are in the caller, but not in the callee.
    not_in_callee: Vec<Tuple<F>>,
}

const MAX_TUPLES: usize = 5;

/// Formats an error, where some tuples in <from_machine> are not in <to_machine>.
fn fmt_subset_error<F: fmt::Display>(
    f: &mut fmt::Formatter<'_>,
    machine1: &str,
    machine2: &str,
    not_in_machine2: &[Tuple<F>],
) -> fmt::Result {
    writeln!(
        f,
        "  The following tuples appear in {machine2}, but not in {machine1}:"
    )?;
    for tuple in not_in_machine2.iter().take(MAX_TUPLES) {
        writeln!(f, "    {tuple}")?;
    }
    if not_in_machine2.len() > MAX_TUPLES {
        writeln!(f, "    ...")?;
    }
    Ok(())
}

impl<F: FieldElement> fmt::Display for FailingConnectionConstraint<'_, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "Connection failed between {} and {}:",
            self.connection.caller(),
            self.connection.callee()
        )?;
        writeln!(f, "    {}", self.connection.identity)?;

        if !self.not_in_callee.is_empty() {
            fmt_subset_error(
                f,
                &self.connection.caller(),
                &self.connection.callee(),
                &self.not_in_callee,
            )?;
        }
        if !self.not_in_caller.is_empty() {
            fmt_subset_error(
                f,
                &self.connection.callee(),
                &self.connection.caller(),
                &self.not_in_caller,
            )?;
        }
        Ok(())
    }
}

const MAX_ERRORS: usize = 5;

pub struct FailingConnectionConstraints<'a, F> {
    connection_count: usize,
    errors: Vec<FailingConnectionConstraint<'a, F>>,
}

impl<F: FieldElement> fmt::Display for FailingConnectionConstraints<'_, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "Errors in {} / {} connections:",
            self.errors.len(),
            self.connection_count
        )?;
        for error in self.errors.iter().take(MAX_ERRORS) {
            writeln!(f, "{error}")?;
        }
        if self.errors.len() > MAX_ERRORS {
            writeln!(f, "... and {} more errors", self.errors.len() - MAX_ERRORS)?;
        }
        Ok(())
    }
}
