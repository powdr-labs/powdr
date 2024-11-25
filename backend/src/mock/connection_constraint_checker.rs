use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::ops::ControlFlow;

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
        let connection_global = Self { left, right, kind };
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
            kind: connection_global.kind,
        })
    }

    /// Translates PolyIDs pointing to columns in the global PIL to PolyIDs pointing to columns in the local PIL.
    fn localize(
        &self,
        selected_expressions: &SelectedExpressions<F>,
        global_pil: &Analyzed<F>,
        local_pil: &Analyzed<F>,
    ) -> SelectedExpressions<F> {
        let name_to_id_local = local_pil.name_to_poly_id();
        let id_map = global_pil
            .name_to_poly_id()
            .into_iter()
            .map(|(name, poly_id)| (poly_id, *name_to_id_local.get(&name).unwrap()))
            .collect::<BTreeMap<_, _>>();

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
