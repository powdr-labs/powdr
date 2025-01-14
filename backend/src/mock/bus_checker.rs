use std::{cmp::Ordering, collections::BTreeMap, fmt};

use powdr_ast::{
    analyzed::{Analyzed, Identity, PhantomBusInteractionIdentity},
    parsed::visitor::Children,
};
use powdr_executor_utils::expression_evaluator::ExpressionEvaluator;
use powdr_number::FieldElement;

use super::{localize, machine::Machine, unique_referenced_namespaces};

pub struct BusChecker<'a, F> {
    connections: &'a [BusConnection<F>],
    machines: &'a BTreeMap<String, Machine<'a, F>>,
}

pub struct Error<F> {
    tuple: Vec<F>,
    sends: BTreeMap<BusConnection<F>, usize>,
    receives: BTreeMap<BusConnection<F>, usize>,
}

impl<F: fmt::Display> fmt::Display for Error<F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "Bus interaction {} failed for tuple {}:",
            self.tuple[0],
            self.tuple
                .iter()
                .skip(1)
                .map(|v| v.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        for (
            BusConnection {
                machine,
                interaction,
            },
            count,
        ) in &self.sends
        {
            writeln!(f, "  - sent {count} times",)?;
            writeln!(f, "    by `{interaction}`",)?;
            writeln!(f, "    in {machine}",)?;
        }
        for (
            BusConnection {
                machine,
                interaction,
            },
            count,
        ) in &self.receives
        {
            writeln!(f, "  - received {count} times",)?;
            writeln!(f, "    by `{interaction}`",)?;
            writeln!(f, "    in {machine}",)?;
        }
        Ok(())
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone)]
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
    ) -> Self {
        Self {
            connections,
            machines,
        }
    }

    pub fn check(&self) -> Result<(), Vec<Error<F>>> {
        type BusState<'a, F> = BTreeMap<
            Vec<F>,
            (
                BTreeMap<&'a BusConnection<F>, usize>,
                BTreeMap<&'a BusConnection<F>, usize>,
            ),
        >;

        let bus_state: BusState<F> = self
            .machines
            .iter()
            .flat_map(|(name, machine)| {
                (0..machine.size).flat_map(|row_id| {
                    // create an evaluator for this row
                    let mut evaluator = ExpressionEvaluator::new(
                        machine.values.row(row_id),
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

                            let tuple = bus_interaction
                                .tuple
                                .children()
                                .map(|e| evaluator.evaluate(e))
                                .collect();

                            (bus_connection, tuple, multiplicity)
                        })
                })
            })
            .fold(
                Default::default(),
                |mut counts, (bus_connection, tuple, multiplicity)| {
                    // update the counts
                    let (s, r) = counts.entry(tuple).or_default();

                    let abs = multiplicity.unsigned_abs() as usize;

                    match multiplicity.cmp(&0) {
                        // if the multiplicity is zero, we don't need to do anything
                        Ordering::Equal => {}
                        // if the multiplicity is positive, we send
                        Ordering::Greater => {
                            s.entry(bus_connection)
                                .and_modify(|sends| *sends += abs)
                                .or_insert(abs);
                        }
                        // if the multiplicity is negative, we receive
                        Ordering::Less => {
                            r.entry(bus_connection)
                                .and_modify(|receives| *receives += abs)
                                .or_insert(abs);
                        }
                    }

                    counts
                },
            );

        let mut errors = vec![];

        for (tuple, (sends, receives)) in bus_state {
            let send_count = sends.values().sum::<usize>();
            let receive_count = receives.values().sum::<usize>();
            if send_count != receive_count {
                let error = Error {
                    tuple,
                    sends: sends.into_iter().map(|(k, v)| (k.clone(), v)).collect(),
                    receives: receives.into_iter().map(|(k, v)| (k.clone(), v)).collect(),
                };

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
