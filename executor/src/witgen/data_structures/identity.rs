use core::fmt;

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
pub struct BusInteractionIdentity<T> {
    // The ID is globally unique among identities.
    pub id: u64,
    pub multiplicity: Option<AlgebraicExpression<T>>,
    pub selected_tuple: SelectedExpressions<T>,
}

impl<T: FieldElement> BusInteractionIdentity<T> {
    pub fn try_match_static<'a>(
        &self,
        receives: &'a [BusInteractionIdentity<T>],
    ) -> Option<&'a BusInteractionIdentity<T>> {
        let id = self.interaction_id()?;

        let mut matching_receive = None;
        for other in receives {
            if other.interaction_id() == Some(id) {
                match matching_receive {
                    None => {
                        matching_receive = Some(other);
                    }
                    // Multiple matches
                    Some(_) => {
                        return None;
                    }
                }
            }
        }

        matching_receive
    }

    fn interaction_id(&self) -> Option<T> {
        match &self.selected_tuple.expressions[0] {
            AlgebraicExpression::Number(id) => Some(*id),
            _ => None,
        }
    }

    pub fn is_unconstrained(&self) -> bool {
        // TODO: This is a hack (but should work if it was originally a lookup / permutation)
        self.multiplicity.as_ref() == Some(&self.selected_tuple.selector)
    }
}

impl<T> Children<AlgebraicExpression<T>> for BusInteractionIdentity<T> {
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

impl<T: fmt::Display + fmt::Debug> fmt::Display for BusInteractionIdentity<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "BusInteractionIdentity(id={}, multiplicity={:?}, tuple={})",
            self.id, self.multiplicity, self.selected_tuple
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, derive_more::Display)]
pub enum Identity<T> {
    Polynomial(PolynomialIdentity<T>),
    Connect(ConnectIdentity<T>),
    BusSend(BusInteractionIdentity<T>),
    BusReceive(BusInteractionIdentity<T>),
}

impl<T> Children<AlgebraicExpression<T>> for Identity<T> {
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut AlgebraicExpression<T>> + '_> {
        match self {
            Identity::Polynomial(i) => i.children_mut(),
            Identity::Connect(i) => i.children_mut(),
            Identity::BusSend(i) | Identity::BusReceive(i) => i.children_mut(),
        }
    }

    fn children(&self) -> Box<dyn Iterator<Item = &AlgebraicExpression<T>> + '_> {
        match self {
            Identity::Polynomial(i) => i.children(),
            Identity::Connect(i) => i.children(),
            Identity::BusSend(i) | Identity::BusReceive(i) => i.children(),
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
            Identity::BusSend(i) | Identity::BusReceive(i) => i.id,
        }
    }
}

pub fn convert<T: Clone>(identities: &[AnalyzedIdentity<T>]) -> Vec<Identity<T>> {
    let max_poly_id = identities
        .iter()
        .map(|i| match i {
            AnalyzedIdentity::Polynomial(identity) => identity.id,
            AnalyzedIdentity::Connect(identity) => identity.id,
            // Replaced anyway, so we don't bother with the ID.
            AnalyzedIdentity::Lookup(_)
            | AnalyzedIdentity::PhantomLookup(_)
            | AnalyzedIdentity::Permutation(_)
            | AnalyzedIdentity::PhantomPermutation(_)
            | AnalyzedIdentity::PhantomBusInteraction(_) => 0,
        })
        .max()
        .unwrap_or(0);

    let mut id_counter = max_poly_id + 1;

    identities
        .iter()
        .flat_map(|i| match i {
            AnalyzedIdentity::Polynomial(identity) => {
                vec![Identity::Polynomial(identity.clone())].into_iter()
            }
            AnalyzedIdentity::Connect(identity) => {
                vec![Identity::Connect(identity.clone())].into_iter()
            }
            AnalyzedIdentity::PhantomBusInteraction(identity) => {
                let id = id_counter;
                id_counter += 1;
                let (is_receive, multiplicity) = match &identity.multiplicity {
                    AlgebraicExpression::UnaryOperation(op) => {
                        // There is only one unary operation
                        assert_eq!(op.op, AlgebraicUnaryOperator::Minus);
                        (true, (*op.expr).clone())
                    }
                    _ => (false, identity.multiplicity.clone()),
                };
                let bus_interaction = BusInteractionIdentity {
                    id,
                    multiplicity: Some(multiplicity),
                    selected_tuple: SelectedExpressions {
                        selector: identity.latch.clone(),
                        expressions: identity.tuple.0.clone(),
                    },
                };
                let identity = match is_receive {
                    true => Identity::BusReceive(bus_interaction),
                    false => Identity::BusSend(bus_interaction),
                };
                vec![identity].into_iter()
            }
            AnalyzedIdentity::Permutation(PermutationIdentity { left, right, .. })
            | AnalyzedIdentity::PhantomPermutation(PhantomPermutationIdentity {
                left,
                right,
                ..
            }) => {
                let id_left = id_counter;
                let id_right = id_counter + 1;
                id_counter += 2;
                vec![
                    Identity::BusSend(BusInteractionIdentity {
                        id: id_left,
                        multiplicity: Some(left.selector.clone()),
                        selected_tuple: left.clone(),
                    }),
                    Identity::BusReceive(BusInteractionIdentity {
                        id: id_right,
                        multiplicity: Some(right.selector.clone()),
                        selected_tuple: right.clone(),
                    }),
                ]
                .into_iter()
            }
            AnalyzedIdentity::Lookup(LookupIdentity { left, right, .. }) => {
                let id_left = id_counter;
                let id_right = id_counter + 1;
                id_counter += 2;
                vec![
                    Identity::BusSend(BusInteractionIdentity {
                        id: id_left,
                        multiplicity: None,
                        selected_tuple: left.clone(),
                    }),
                    Identity::BusReceive(BusInteractionIdentity {
                        id: id_right,
                        multiplicity: None,
                        selected_tuple: right.clone(),
                    }),
                ]
                .into_iter()
            }
            AnalyzedIdentity::PhantomLookup(PhantomLookupIdentity {
                left,
                right,
                multiplicity,
                ..
            }) => {
                let id_left = id_counter;
                let id_right = id_counter + 1;
                id_counter += 2;
                vec![
                    Identity::BusSend(BusInteractionIdentity {
                        id: id_left,
                        multiplicity: Some(multiplicity.clone()),
                        selected_tuple: left.clone(),
                    }),
                    Identity::BusReceive(BusInteractionIdentity {
                        id: id_right,
                        multiplicity: Some(multiplicity.clone()),
                        selected_tuple: right.clone(),
                    }),
                ]
                .into_iter()
            }
        })
        .collect()
}
