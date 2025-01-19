use core::fmt;
use std::{iter::once, ops::RangeFrom};

use itertools::Itertools;
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
pub struct BusInteraction<T> {
    /// The ID is globally unique among identities.
    pub id: u64,
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

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct BusSend<T>(pub BusInteraction<T>);

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct BusReceive<T>(pub BusInteraction<T>);

impl<T: FieldElement> BusInteraction<T> {
    /// Returns the first tuple entry if it is a number.
    /// We assume this to identify the connection, i.e., sends and receives
    /// can be matched by matching the interaction ID.
    fn interaction_id(&self) -> Option<T> {
        match &self.selected_tuple.expressions[0] {
            AlgebraicExpression::Number(id) => Some(*id),
            _ => None,
        }
    }
}

impl<T: FieldElement> BusSend<T> {
    /// Tries to find the matching send, by linearly searching through all receives
    /// and checking if there is exactly one match.
    pub fn try_match_static<'a>(&self, receives: &'a [BusReceive<T>]) -> Option<&'a BusReceive<T>> {
        let id = self.0.interaction_id()?;

        receives
            .iter()
            .filter(|receive| receive.0.interaction_id() == Some(id))
            .exactly_one()
            .ok()
    }
}

impl<T: FieldElement> BusReceive<T> {
    pub fn is_unconstrained(&self) -> bool {
        // TODO: This is a hack (but should work if it was originally a lookup / permutation)
        self.0.multiplicity.as_ref() != Some(&self.0.selected_tuple.selector)
    }
}

impl<T> Children<AlgebraicExpression<T>> for BusInteraction<T> {
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

impl<T: fmt::Display + fmt::Debug> fmt::Display for BusInteraction<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let multiplicity = match &self.multiplicity {
            Some(m) => m.to_string(),
            None => "None".to_string(),
        };
        write!(
            f,
            "tuple={}, multiplicity={}",
            self.selected_tuple, multiplicity
        )
    }
}

impl<T: fmt::Display + fmt::Debug> fmt::Display for BusSend<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BusSend({})", self.0)
    }
}

impl<T: fmt::Display + fmt::Debug> fmt::Display for BusReceive<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BusReceive({})", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, derive_more::Display)]
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
            Identity::BusSend(i) => i.0.children_mut(),
            Identity::BusReceive(i) => i.0.children_mut(),
        }
    }

    fn children(&self) -> Box<dyn Iterator<Item = &AlgebraicExpression<T>> + '_> {
        match self {
            Identity::Polynomial(i) => i.children(),
            Identity::Connect(i) => i.children(),
            Identity::BusSend(i) => i.0.children(),
            Identity::BusReceive(i) => i.0.children(),
        }
    }
}

impl<T> Identity<T> {
    pub fn contains_next_ref(&self) -> bool {
        self.children().any(|e| e.contains_next_ref())
    }

    pub fn id(&self) -> u64 {
        match self {
            Identity::Polynomial(i) => i.id,
            Identity::Connect(i) => i.id,
            Identity::BusSend(i) => i.0.id,
            Identity::BusReceive(i) => i.0.id,
        }
    }
}

/// Converts a list of [powdr_ast::analyzed::Identity] into a list of [Identity].
/// Polynomial and connect identities remain unchanged, phantom bus interactions
/// are converted to either a bus send or bus receive, and permutations and lookups
/// are converted to a pair of bus send and bus receive.
pub fn convert<T: FieldElement>(identities: &[AnalyzedIdentity<T>]) -> Vec<Identity<T>> {
    let mut id_counter = 1..;

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
            vec![Identity::Polynomial(PolynomialIdentity {
                id: id_counter.next().unwrap(),
                ..identity.clone()
            })]
        }
        AnalyzedIdentity::Connect(identity) => {
            vec![Identity::Connect(ConnectIdentity {
                id: id_counter.next().unwrap(),
                ..identity.clone()
            })]
        }
        AnalyzedIdentity::PhantomBusInteraction(identity) => {
            let (is_receive, multiplicity) = match &identity.multiplicity {
                AlgebraicExpression::UnaryOperation(op) => {
                    // There is only one unary operation
                    assert_eq!(op.op, AlgebraicUnaryOperator::Minus);
                    (true, (*op.expr).clone())
                }
                _ => (false, identity.multiplicity.clone()),
            };
            match identity.tuple.0[0] {
                AlgebraicExpression::Number(_) => {}
                _ => panic!("Expected first tuple entry to be a static ID"),
            };
            let bus_interaction = BusInteraction {
                id: id_counter.next().unwrap(),
                multiplicity: Some(multiplicity),
                selected_tuple: SelectedExpressions {
                    selector: identity.latch.clone(),
                    expressions: identity.tuple.0.clone(),
                },
            };
            let identity = match is_receive {
                true => Identity::BusReceive(BusReceive(bus_interaction)),
                false => Identity::BusSend(BusSend(bus_interaction)),
            };
            vec![identity]
        }
        AnalyzedIdentity::Permutation(PermutationIdentity { left, right, .. })
        | AnalyzedIdentity::PhantomPermutation(PhantomPermutationIdentity {
            left, right, ..
        }) => bus_interaction_pair(id_counter, left, right, Some(right.selector.clone())),
        AnalyzedIdentity::Lookup(LookupIdentity { left, right, .. }) => {
            bus_interaction_pair(id_counter, left, right, None)
        }
        AnalyzedIdentity::PhantomLookup(PhantomLookupIdentity {
            left,
            right,
            multiplicity,
            ..
        }) => bus_interaction_pair(id_counter, left, right, Some(multiplicity.clone())),
    }
}

fn bus_interaction_pair<T: FieldElement>(
    id_counter: &mut RangeFrom<u64>,
    left: &SelectedExpressions<T>,
    right: &SelectedExpressions<T>,
    rhs_multiplicity: Option<AlgebraicExpression<T>>,
) -> Vec<Identity<T>> {
    let id_left = id_counter.next().unwrap();
    let id_right = id_counter.next().unwrap();
    vec![
        Identity::BusSend(BusSend(BusInteraction {
            id: id_left,
            multiplicity: Some(left.selector.clone()),
            selected_tuple: insert_id(left, id_left),
        })),
        Identity::BusReceive(BusReceive(BusInteraction {
            id: id_right,
            multiplicity: rhs_multiplicity,
            selected_tuple: insert_id(right, id_left),
        })),
    ]
}

fn insert_id<T: FieldElement>(
    selected_expressions: &SelectedExpressions<T>,
    id: u64,
) -> SelectedExpressions<T> {
    SelectedExpressions {
        selector: selected_expressions.selector.clone(),
        expressions: once(AlgebraicExpression::Number(id.into()))
            .chain(selected_expressions.expressions.clone())
            .collect(),
    }
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
                Identity::BusReceive(r) => Some(r.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();

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
            "BusSend(tuple=main::left_latch $ [5, main::a], multiplicity=main::left_latch)",
            "BusReceive(tuple=main::right_latch $ [5, main::b], multiplicity=None)",
            true,
        );
    }

    #[test]
    fn phantom_lookup() {
        assert_correct_bus_interaction_pair(
            "Constr::PhantomLookup((Option::Some(left_latch), Option::Some(right_latch)), [(a, b)], multiplicities);",
            "BusSend(tuple=main::left_latch $ [5, main::a], multiplicity=main::left_latch)",
            "BusReceive(tuple=main::right_latch $ [5, main::b], multiplicity=main::multiplicities)",
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
            "BusSend(tuple=main::left_latch $ [42, main::a], multiplicity=main::left_latch)",
            "BusReceive(tuple=main::right_latch $ [42, main::b], multiplicity=main::multiplicities)",
            true,
        );
    }

    #[test]
    fn native_permutation() {
        assert_correct_bus_interaction_pair(
            "left_latch $ [a] is (right_latch * right_selector) $ [b];",
            "BusSend(tuple=main::left_latch $ [5, main::a], multiplicity=main::left_latch)",
            "BusReceive(tuple=main::right_latch * main::right_selector $ [5, main::b], multiplicity=main::right_latch * main::right_selector)",
            false,
        );
    }

    #[test]
    fn phantom_permutation() {
        assert_correct_bus_interaction_pair(
            "Constr::PhantomPermutation((Option::Some(left_latch), Option::Some(right_latch * right_selector)), [(a, b)]);",
            "BusSend(tuple=main::left_latch $ [5, main::a], multiplicity=main::left_latch)",
            "BusReceive(tuple=main::right_latch * main::right_selector $ [5, main::b], multiplicity=main::right_latch * main::right_selector)",
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
            "BusSend(tuple=main::left_latch $ [42, main::a], multiplicity=main::left_latch)",
            "BusReceive(tuple=main::right_latch * main::right_selector $ [42, main::b], multiplicity=main::right_latch * main::right_selector)",
            false,
        );
    }
}
