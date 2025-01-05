use std::collections::{BTreeMap, BTreeSet};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{AlgebraicExpression, Analyzed, Identity, PhantomBusInteractionIdentity},
    parsed::visitor::Children,
};
use powdr_executor_utils::expression_evaluator::{ExpressionEvaluator, OwnedGlobalValues};
use powdr_number::FieldElement;

use super::machine::Machine;

pub struct BusChecker<'a, F> {
    connections: &'a [BusConnection<F>],
    machines: &'a BTreeMap<String, Machine<'a, F>>,
    global_values: OwnedGlobalValues<F>,
}

type Error = String;

/// A group of bus interactions which share the same id.
pub struct BusConnection<F> {
    pub id: F,
    pub interactions: BTreeSet<PhantomBusInteractionIdentity<F>>,
}

impl<F: FieldElement> BusConnection<F> {
    /// Extracts all bus connections from the global PIL.
    /// Assumption: Bus interactions have a unique ID as their first element.
    pub fn get_all(
        global_pil: &Analyzed<F>,
        _: &BTreeMap<String, Analyzed<F>>,
    ) -> Vec<Self> {
        global_pil
            .identities
            .iter()
            .filter_map(|identity| {
                if let Identity::PhantomBusInteraction(identity) = identity {
                    Some(identity)
                } else {
                    None
                }
            })
            .into_group_map_by(|bus_interaction| {
                if let AlgebraicExpression::Number(interaction_id) =
                    bus_interaction.tuple.children().next().unwrap()
                {
                    interaction_id
                } else {
                    panic!("Expected a number in the first child of a bus interaction")
                }
            })
            .into_iter()
            .map(|(id, interactions)| {
                // These interactions are guaranteed to have the same ID
                // We will check that they sum to zero
                BusConnection {
                    id: *id,
                    interactions: interactions.into_iter().cloned().collect(),
                }
            })
            .collect()
    }
}

impl<'a, F: FieldElement> BusChecker<'a, F> {
    pub fn new(
        connections: &'a [BusConnection<F>],
        machines: &'a BTreeMap<String, Machine<'a, F>>,
        challenges: &'a BTreeMap<u64, F>,
    ) -> Self {
        let global_values = OwnedGlobalValues {
            // TODO: Support publics.
            public_values: BTreeMap::new(),
            challenge_values: challenges.clone(),
        };
        Self {
            connections,
            machines,
            global_values,
        }
    }

    pub fn check(&self) -> Result<(), Error> {
        for bus_connection in self.connections {
            self.check_connection(bus_connection)?;
        }
        Ok(())
    }

    fn check_connection(&self, bus_connection: &BusConnection<F>) -> Result<(), Error> {
        // For this id, build the set of all sends and receives. Sends have positive multiplicity, receives negative.
        let rows = self.machines.iter().flat_map(|(_, machine)| {
            (0..machine.size).map(|row_id| {
                (
                    &machine.intermediate_definitions,
                    machine.trace_values.row(row_id),
                )
            })
        });
        let interactions: BTreeMap<Vec<F>, (usize, usize)> = rows.fold(
            Default::default(),
            |mut counts, (intermediate_definitions, row)| {
                let mut evaluator =
                    ExpressionEvaluator::new(row, &self.global_values, intermediate_definitions);

                // execute the bus interaction on this row
                let new_counts: Vec<(Vec<F>, (usize, usize))> = bus_connection
                    .interactions
                    .iter()
                    .map(|bus_interaction| {
                        let multiplicity = evaluator
                            .evaluate(&bus_interaction.multiplicity)
                            .try_into_i32()
                            .unwrap();
                        // we interpret the multiplicity as a send if it is positive, and a receive if it is negative
                        let is_send = multiplicity.is_positive();
                        let tuple = bus_interaction
                            .tuple
                            .children()
                            .map(|e| evaluator.evaluate(e))
                            .collect();
                        (
                            tuple,
                            if is_send {
                                (multiplicity as usize, 0)
                            } else {
                                (0, -multiplicity as usize)
                            },
                        )
                    })
                    .collect();

                for (tuple, (send, receive)) in new_counts {
                    counts
                        .entry(tuple)
                        .and_modify(|(s, r)| {
                            *s += send;
                            *r += receive;
                        })
                        .or_insert((send, receive));
                }
                counts
            },
        );

        for (tuple, (send, receive)) in interactions {
            if send != receive {
                return Err(format!(
                    "Bus connection with id {:?} failed: send {} != receive {} for tuple {:?}",
                    bus_connection.id, send, receive, tuple
                ));
            }
        }

        Ok(())
    }
}
