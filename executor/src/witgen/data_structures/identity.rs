use core::fmt;
use std::{collections::BTreeMap, ops::RangeFrom};

use powdr_ast::{
    analyzed::{
        AlgebraicExpression, AlgebraicUnaryOperator, ConnectIdentity, Identity as AnalyzedIdentity,
        LookupIdentity, PermutationIdentity, PhantomLookupIdentity, PhantomPermutationIdentity,
        PolynomialIdentity, SelectedExpressions,
    },
    parsed::visitor::Children,
};
use powdr_number::FieldElement;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct BusSend<T> {
    /// The ID is globally unique among identities.
    pub id: u64,
    /// The ID of the bus interaction, used to find a matching receive.
    pub interaction_id: AlgebraicExpression<T>,
    /// The tuple sent to the bus, with a selector.
    pub selected_tuple: SelectedExpressions<T>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct BusReceive<T> {
    /// The ID is globally unique among identities.
    pub id: u64,
    /// The ID of the bus interaction, to be matched with a send.
    pub interaction_id: T,
    /// The tuple sent to the bus, with a selector.
    /// The selector is an expression that might or might not reference
    /// the multiplicity column, but it should always evaluate to a
    /// binary value. If it is zero, the multiplicity must be zero as well.
    /// For example, it could also be binary fixed column, indicating
    /// where the multiplicity can be non-zero.
    pub selected_tuple: SelectedExpressions<T>,
    /// The multiplicity with which the tuple is sent to the bus.
    /// None if this interaction comes from the RHS of a native lookup,
    /// as native lookup arguments don't require the multiplicity to be
    /// committed.
    /// Note that this is the absolute multiplicity, i.e., for bus receives,
    /// the negation has been removed.
    pub multiplicity: Option<AlgebraicExpression<T>>,
}

impl<T: FieldElement> BusSend<T> {
    /// Tries to find the matching receive, which should succeed iff. the send has
    /// a static interaction ID.
    pub fn try_match_static<'a>(
        &self,
        receives: &'a BTreeMap<T, BusReceive<T>>,
    ) -> Option<&'a BusReceive<T>> {
        Some(&receives[&self.interaction_id()?])
    }

    /// Returns the interaction ID if it is a number.
    /// Sends and receives can be matched by matching the interaction ID.
    pub fn interaction_id(&self) -> Option<T> {
        match &self.interaction_id {
            AlgebraicExpression::Number(id) => Some(*id),
            _ => None,
        }
    }
}

impl<T: FieldElement> BusReceive<T> {
    /// Returns whether the receive has an unconstrained multiplicity.
    /// If it does not, this typically means that the multiplicity is binary constraint.
    /// Unconstrained receives are equivalent to the RHS of a lookup;
    /// binary-constrained receives are equivalent to the RHS of a permutation.
    pub fn is_unconstrained(&self) -> bool {
        // TODO: This always works in practice, but we should properly check the
        // range constraints on the multiplicity column.
        self.multiplicity.as_ref() != Some(&self.selected_tuple.selector)
    }
}

impl<T> Children<AlgebraicExpression<T>> for BusSend<T> {
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut AlgebraicExpression<T>> + '_> {
        self.selected_tuple.children_mut()
    }
    fn children(&self) -> Box<dyn Iterator<Item = &AlgebraicExpression<T>> + '_> {
        self.selected_tuple.children()
    }
}

impl<T> Children<AlgebraicExpression<T>> for BusReceive<T> {
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut AlgebraicExpression<T>> + '_> {
        Box::new(
            self.selected_tuple
                .children_mut()
                .chain(self.multiplicity.iter_mut()),
        )
    }
    fn children(&self) -> Box<dyn Iterator<Item = &AlgebraicExpression<T>> + '_> {
        Box::new(
            self.selected_tuple
                .children()
                .chain(self.multiplicity.iter()),
        )
    }
}

impl<T: fmt::Display + fmt::Debug> fmt::Display for BusSend<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "BusSend(tuple={}, interaction_id={})",
            self.selected_tuple, self.interaction_id
        )
    }
}

impl<T: fmt::Display + fmt::Debug> fmt::Display for BusReceive<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let multiplicity = match &self.multiplicity {
            Some(m) => m.to_string(),
            None => "None".to_string(),
        };
        write!(
            f,
            "BusReceive(tuple={}, multiplicity={}, interaction_id={})",
            self.selected_tuple, multiplicity, self.interaction_id
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, derive_more::Display)]
/// A constraint in the constraint system.
/// It is similar to [powdr_ast::analyzed::Identity], but abstracts
/// over (phantom) lookups, permutations, and bus interactions.
/// Instead, they are represented as a (pair of) bus send and bus receive.
pub enum Identity<T> {
    Polynomial(PolynomialIdentity<T>),
    Connect(ConnectIdentity<T>),
    BusSend(BusSend<T>),
    BusReceive(BusReceive<T>),
}

impl<T> Children<AlgebraicExpression<T>> for Identity<T> {
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut AlgebraicExpression<T>> + '_> {
        match self {
            Identity::Polynomial(i) => i.children_mut(),
            Identity::Connect(i) => i.children_mut(),
            Identity::BusSend(i) => i.children_mut(),
            Identity::BusReceive(i) => i.children_mut(),
        }
    }

    fn children(&self) -> Box<dyn Iterator<Item = &AlgebraicExpression<T>> + '_> {
        match self {
            Identity::Polynomial(i) => i.children(),
            Identity::Connect(i) => i.children(),
            Identity::BusSend(i) => i.children(),
            Identity::BusReceive(i) => i.children(),
        }
    }
}

impl<T> Identity<T> {
    pub fn contains_next_ref(&self) -> bool {
        // TODO: This does not check the definitions of intermediate polynomials!
        self.children().any(|e| e.contains_next_ref())
    }

    pub fn id(&self) -> u64 {
        match self {
            Identity::Polynomial(i) => i.id,
            Identity::Connect(i) => i.id,
            Identity::BusSend(i) => i.id,
            Identity::BusReceive(i) => i.id,
        }
    }
}

/// Converts a list of [powdr_ast::analyzed::Identity] into a list of [Identity].
/// Polynomial and connect identities remain unchanged, phantom bus interactions
/// are converted to either a bus send or bus receive, and permutations and lookups
/// are converted to a pair of bus send and bus receive.
pub fn convert<T: FieldElement>(identities: &[AnalyzedIdentity<T>]) -> Vec<Identity<T>> {
    // (Phantom) lookups / permutations are converted to a pair of bus send and bus receive,
    // so we need to allocate new IDs for the new receives,
    let max_id = identities.iter().map(|id| id.id()).max().unwrap_or(0);
    let mut id_counter = (max_id + 1)..;

    identities
        .iter()
        .flat_map(|identity| convert_identity(&mut id_counter, identity))
        .collect()
}

fn convert_identity<T: FieldElement>(
    id_counter: &mut RangeFrom<u64>,
    identity: &AnalyzedIdentity<T>,
) -> Vec<Identity<T>> {
    match identity {
        AnalyzedIdentity::Polynomial(identity) => {
            vec![Identity::Polynomial(identity.clone())]
        }
        AnalyzedIdentity::Connect(identity) => {
            vec![Identity::Connect(identity.clone())]
        }
        AnalyzedIdentity::PhantomBusInteraction(identity) => {
            // Detect receives by having a unary minus in the multiplicity
            // TODO: We should instead analyze the range constraints of the
            // multiplicity expression.
            let (is_receive, multiplicity) = match &identity.multiplicity {
                AlgebraicExpression::UnaryOperation(op) => {
                    // There is only one unary operation
                    assert_eq!(op.op, AlgebraicUnaryOperator::Minus);
                    (true, (*op.expr).clone())
                }
                _ => (false, identity.multiplicity.clone()),
            };
            // By convention, we assume that the first tuple entry is the interaction ID.
            // TODO: Instead, we should have a separate field in the phantom bus interaction type.
            let interaction_id = match identity.tuple.0[0] {
                AlgebraicExpression::Number(id) => id,
                // TODO: Relax this for sends when implementing dynamic sends
                _ => panic!("Expected first tuple entry to be a static ID"),
            };
            // Remove the interaction ID from the list of expressions.
            let expressions = identity.tuple.0.iter().skip(1).cloned().collect();
            let selected_tuple = SelectedExpressions {
                selector: identity.latch.clone(),
                expressions,
            };
            let identity = match is_receive {
                true => Identity::BusReceive(BusReceive {
                    id: identity.id,
                    interaction_id,
                    multiplicity: Some(multiplicity),
                    selected_tuple,
                }),
                false => {
                    assert_eq!(multiplicity, identity.latch);
                    Identity::BusSend(BusSend {
                        id: identity.id,
                        interaction_id: AlgebraicExpression::Number(interaction_id),
                        selected_tuple,
                    })
                }
            };
            vec![identity]
        }
        // Permutations have a RHS multiplicity that is equal to the RHS latch.
        AnalyzedIdentity::Permutation(PermutationIdentity {
            id, left, right, ..
        })
        | AnalyzedIdentity::PhantomPermutation(PhantomPermutationIdentity {
            id,
            left,
            right,
            ..
        }) => bus_interaction_pair(*id, id_counter, left, right, Some(right.selector.clone())),
        // Native lookups do not have a multiplicity.
        AnalyzedIdentity::Lookup(LookupIdentity {
            id, left, right, ..
        }) => bus_interaction_pair(*id, id_counter, left, right, None),
        AnalyzedIdentity::PhantomLookup(PhantomLookupIdentity {
            id,
            left,
            right,
            multiplicity,
            ..
        }) => bus_interaction_pair(*id, id_counter, left, right, Some(multiplicity.clone())),
    }
}

fn bus_interaction_pair<T: FieldElement>(
    id: u64,
    id_counter: &mut RangeFrom<u64>,
    left: &SelectedExpressions<T>,
    right: &SelectedExpressions<T>,
    rhs_multiplicity: Option<AlgebraicExpression<T>>,
) -> Vec<Identity<T>> {
    // +1 because we want to be sure it is non-zero
    let interaction_id: T = (id + 1).into();
    vec![
        Identity::BusSend(BusSend {
            id,
            interaction_id: AlgebraicExpression::Number(interaction_id),
            selected_tuple: left.clone(),
        }),
        Identity::BusReceive(BusReceive {
            id: id_counter.next().unwrap(),
            multiplicity: rhs_multiplicity,
            interaction_id,
            selected_tuple: right.clone(),
        }),
    ]
}

#[cfg(test)]
mod test {
    use powdr_number::GoldilocksField;

    use crate::witgen::data_structures::identity::Identity;

    use super::convert;

    fn assert_correct_bus_interaction_pair(
        constraint: &str,
        expected_send: &str,
        expected_receive: &str,
        expected_is_receive_unconstrained: bool,
    ) {
        let pil_source = format!(
            r"
namespace main(4);
    col fixed right_latch = [0, 1]*;
    col witness right_selector, left_latch, a, b, multiplicities;
    {constraint}
    
    // Selectors should be binary
    (left_latch - 1) * left_latch = 0;
    (right_selector - 1) * right_selector = 0;

    // Some other bus interactions to potentially confuse the matching
    right_latch $ [a] in right_latch $ [a];
    left_latch $ [b] in left_latch $ [b];
"
        );
        let analyzed = powdr_pil_analyzer::analyze_string::<GoldilocksField>(&pil_source).unwrap();
        let identities = convert(&analyzed.identities);
        let receives = identities
            .iter()
            .filter_map(|id| match id {
                Identity::BusReceive(r) => Some((r.interaction_id, r.clone())),
                _ => None,
            })
            .collect();

        match (&identities[0], &identities[1]) {
            (Identity::BusSend(send), Identity::BusReceive(receive)) => {
                assert_eq!(&send.to_string(), expected_send);
                assert_eq!(&receive.to_string(), expected_receive);
                assert_eq!(send.try_match_static(&receives).unwrap(), receive);
                assert_eq!(
                    receive.is_unconstrained(),
                    expected_is_receive_unconstrained
                )
            }
            _ => panic!(
                "Expected one receive and one send, but got:\n{}\n{}",
                identities[0], identities[1]
            ),
        }
    }

    #[test]
    fn native_lookup() {
        assert_correct_bus_interaction_pair(
            "left_latch $ [a] in right_latch $ [b];",
            "BusSend(tuple=main::left_latch $ [main::a], interaction_id=1)",
            "BusReceive(tuple=main::right_latch $ [main::b], multiplicity=None, interaction_id=1)",
            true,
        );
    }

    #[test]
    fn phantom_lookup() {
        assert_correct_bus_interaction_pair(
            "Constr::PhantomLookup((Option::Some(left_latch), Option::Some(right_latch)), [(a, b)], multiplicities);",
            "BusSend(tuple=main::left_latch $ [main::a], interaction_id=1)",
            "BusReceive(tuple=main::right_latch $ [main::b], multiplicity=main::multiplicities, interaction_id=1)",
            true,
        );
    }

    #[test]
    fn lookup_bus_interactions() {
        // This is what would currently be generated by the following asm program:
        // use std::protocols::lookup_via_bus::lookup_send;
        // use std::protocols::lookup_via_bus::lookup_receive;
        // machine Main with degree: 2048 {
        //     col fixed right_latch = [0, 1]*;
        //     col witness left_latch, a, b, multiplicity;
        //     lookup_send(42, left_latch $ [a] in right_latch $ [b]);
        //     lookup_receive(42, left_latch $ [a] in right_latch $ [b], right_latch);
        // }
        assert_correct_bus_interaction_pair(
            r"Constr::PhantomBusInteraction(main::left_latch, [42, main::a], main::left_latch);
              Constr::PhantomBusInteraction(-main::multiplicities, [42, main::b], main::right_latch);",
            "BusSend(tuple=main::left_latch $ [main::a], interaction_id=42)",
            "BusReceive(tuple=main::right_latch $ [main::b], multiplicity=main::multiplicities, interaction_id=42)",
            true,
        );
    }

    #[test]
    fn native_permutation() {
        assert_correct_bus_interaction_pair(
            "left_latch $ [a] is (right_latch * right_selector) $ [b];",
            "BusSend(tuple=main::left_latch $ [main::a], interaction_id=1)",
            "BusReceive(tuple=main::right_latch * main::right_selector $ [main::b], multiplicity=main::right_latch * main::right_selector, interaction_id=1)",
            false,
        );
    }

    #[test]
    fn phantom_permutation() {
        assert_correct_bus_interaction_pair(
            "Constr::PhantomPermutation((Option::Some(left_latch), Option::Some(right_latch * right_selector)), [(a, b)]);",
            "BusSend(tuple=main::left_latch $ [main::a], interaction_id=1)",
            "BusReceive(tuple=main::right_latch * main::right_selector $ [main::b], multiplicity=main::right_latch * main::right_selector, interaction_id=1)",
            false,
        );
    }

    #[test]
    fn permutation_bus_interactions() {
        // This is what would currently be generated by the following asm program:
        // use std::protocols::permutation_via_bus::permutation_send;
        // use std::protocols::permutation_via_bus::permutation_receive;
        // machine Main with degree: 2048 {
        //     col fixed right_latch = [0, 1]*;
        //     col witness left_latch, a, b, multiplicity;
        //     permutation_send(42, (left_latch * left_selector) $ [a] is right_latch $ [b]);
        //     permutation_receive(42, (left_latch * left_selector) $ [a] is right_latch $ [b]);
        // }
        assert_correct_bus_interaction_pair(
            r"Constr::PhantomBusInteraction(main::left_latch, [42, main::a], main::left_latch);
              Constr::PhantomBusInteraction(-(main::right_latch * main::right_selector), [42, main::b], main::right_latch * main::right_selector);",
            "BusSend(tuple=main::left_latch $ [main::a], interaction_id=42)",
            "BusReceive(tuple=main::right_latch * main::right_selector $ [main::b], multiplicity=main::right_latch * main::right_selector, interaction_id=42)",
            false,
        );
    }
}
