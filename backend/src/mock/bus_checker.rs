use std::collections::BTreeMap;

use powdr_ast::{
    analyzed::{Analyzed, Identity, PhantomBusInteractionIdentity},
    parsed::visitor::Children,
};
use powdr_executor_utils::expression_evaluator::{ExpressionEvaluator, OwnedGlobalValues};
use powdr_number::FieldElement;

use super::{localize, machine::Machine, unique_referenced_namespaces};

pub struct BusChecker<'a, F> {
    connections: &'a [BusConnection<F>],
    machines: &'a BTreeMap<String, Machine<'a, F>>,
    global_values: OwnedGlobalValues<F>,
}

type Error = Vec<String>;

pub struct BusConnection<F> {
    pub machine: String,
    pub interaction: PhantomBusInteractionIdentity<F>,
}

impl<F: FieldElement> BusConnection<F> {
    /// Extracts all bus connections from the global PIL.
    pub fn get_all(
        global_pil: &Analyzed<F>,
        machine_to_pil: &BTreeMap<String, Analyzed<F>>,
    ) -> Vec<Self> {
        global_pil
            .identities
            .iter()
            .filter_map(|identity| {
                // TODO: turn other relevant identities into bus connections
                if let Identity::PhantomBusInteraction(identity) = identity {
                    Some(identity.clone())
                } else {
                    None
                }
            })
            .map(|interaction| {
                // Localize the interaction assuming a single namespace is accessed. TODO: This may break due to the latch.
                let machine = unique_referenced_namespaces(&interaction).unwrap();
                let interaction = localize(interaction, global_pil, &machine_to_pil[&machine]);
                BusConnection {
                    machine,
                    interaction,
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
        // For each row
        let interactions: BTreeMap<Vec<F>, (usize, usize)> = self
            .machines
            .iter()
            .flat_map(|(name, machine)| {
                (0..machine.size).flat_map(|row_id| {
                    // create an evaluator for this row
                    let mut evaluator = ExpressionEvaluator::new(
                        machine.trace_values.row(row_id),
                        &self.global_values,
                        &machine.intermediate_definitions,
                    );

                    // for all connections of this machine
                    self.connections
                        .iter()
                        .filter(|bus_connection| bus_connection.machine == *name)
                        .map(move |bus_connection| {
                            let bus_interaction = &bus_connection.interaction;

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

                            let (send, receive) = if is_send {
                                (multiplicity as usize, 0)
                            } else {
                                (0, -multiplicity as usize)
                            };

                            (tuple, (send, receive))
                        })
                })
            })
            .fold(
                Default::default(),
                |mut counts, (tuple, (send, receive))| {
                    // update the counts
                    counts
                        .entry(tuple)
                        .and_modify(|(s, r)| {
                            *s += send;
                            *r += receive;
                        })
                        .or_insert((send, receive));

                    counts
                },
            );

        let mut errors = vec![];

        for (tuple, (send, receive)) in interactions {
            if send != receive {
                // we interpret the first element of the tuple as the id of the bus interaction
                let id = &tuple[0];
                let values = &tuple[1..];

                let error = format!(
                    "Bus connection with id {id} failed: send {send} != receive {receive} for tuple {values:?}",
                );

                log::error!("{}", error);

                errors.push(error);
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(())
    }
}
