use std::collections::BTreeMap;
use std::collections::BTreeSet;

use powdr_ast::analyzed::{
    Identity, LookupIdentity, PermutationIdentity, PhantomLookupIdentity,
    PhantomPermutationIdentity, SelectedExpressions,
};
use powdr_backend_utils::referenced_namespaces_algebraic_expression;
use powdr_executor::witgen::AffineExpression;
use powdr_executor::witgen::ExpressionEvaluator;
use powdr_number::FieldElement;
use rayon::iter::IntoParallelIterator;
use rayon::iter::ParallelIterator;

use super::evaluator::{Machine, Variables};

pub enum ConnectionKind {
    Lookup,
    Permutation,
}

pub struct Connection<F> {
    pub left: SelectedExpressions<F>,
    pub right: SelectedExpressions<F>,
    /// For [ConnectionKind::Permutation], rows of `left` are a permutation of rows of `right`. For [ConnectionKind::Lookup], all rows in `left` are in `right`.
    pub kind: ConnectionKind,
}

impl<F: Clone> TryFrom<&Identity<F>> for Connection<F> {
    type Error = ();

    fn try_from(identity: &Identity<F>) -> Result<Self, Self::Error> {
        match identity {
            Identity::Polynomial(_) => Err(()),
            Identity::Connect(_) => unimplemented!(),
            Identity::Lookup(LookupIdentity { left, right, .. })
            | Identity::PhantomLookup(PhantomLookupIdentity { left, right, .. }) => Ok(Self {
                left: left.clone(),
                right: right.clone(),
                kind: ConnectionKind::Lookup,
            }),
            Identity::Permutation(PermutationIdentity { left, right, .. })
            | Identity::PhantomPermutation(PhantomPermutationIdentity { left, right, .. }) => {
                Ok(Self {
                    left: left.clone(),
                    right: right.clone(),
                    kind: ConnectionKind::Permutation,
                })
            }
        }
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

impl<F: FieldElement> Connection<F> {
    pub fn caller(&self) -> String {
        unique_referenced_namespaces(&self.left)
    }

    pub fn callee(&self) -> String {
        unique_referenced_namespaces(&self.right)
    }
}

pub struct ConnectionConstraintChecker<'a, F: FieldElement> {
    // TODO: I think the connections here have different PolyIDs from the ones in the machines, because of re-parsing...
    pub connections: &'a [Connection<F>],
    pub machines: BTreeMap<&'a str, Machine<'a, F>>,
}

impl<'a, F: FieldElement> ConnectionConstraintChecker<'a, F> {
    pub fn check(&self) {
        for connection in self.connections {
            self.check_connection(connection);
        }
    }

    fn check_connection(&self, connection: &Connection<F>) {
        log::info!(
            "Checking connection: {} -> {}",
            connection.caller(),
            connection.callee()
        );
        let caller_set = self.selected_tuples(&connection.caller(), &connection.left);
        let callee_set = self.selected_tuples(&connection.callee(), &connection.right);

        match connection.kind {
            ConnectionKind::Lookup => {
                for tuple in caller_set {
                    assert!(
                        callee_set.contains(&tuple),
                        "Lookup failed: {tuple:?} not found in callee",
                    );
                }
            }
            ConnectionKind::Permutation => {
                assert_eq!(
                    caller_set.len(),
                    callee_set.len(),
                    "Permutation failed: caller and callee have different sizes"
                );
                for tuple in caller_set {
                    assert!(
                        callee_set.contains(&tuple),
                        "Permutation failed: {tuple:?} not found in callee",
                    );
                }
            }
        }
    }

    fn selected_tuples(
        &self,
        machine_name: &str,
        selected_expressions: &SelectedExpressions<F>,
    ) -> BTreeSet<Vec<F>> {
        let machine = &self.machines[&machine_name];
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
                    selected_expressions
                        .expressions
                        .iter()
                        .map(|expression| {
                            let result = evaluator.evaluate(expression).unwrap();
                            match result {
                                AffineExpression::Constant(c) => c,
                                _ => unreachable!("Unexpected result: {:?}", result),
                            }
                        })
                        .collect::<Vec<_>>()
                })
            })
            .collect()
    }
}
