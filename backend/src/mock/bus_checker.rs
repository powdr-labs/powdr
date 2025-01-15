use std::{cmp::Ordering, collections::BTreeMap, fmt};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{Analyzed, Identity, PhantomBusInteractionIdentity},
    parsed::visitor::Children,
};
use powdr_executor_utils::expression_evaluator::ExpressionEvaluator;
use powdr_number::FieldElement;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use super::{localize, machine::Machine, unique_referenced_namespaces};

pub struct BusChecker<'a, F> {
    connections: &'a [BusInteraction<F>],
    machines: &'a BTreeMap<String, Machine<'a, F>>,
}

pub struct Error<F> {
    tuple: Vec<F>,
    sends: BTreeMap<BusInteraction<F>, usize>,
    receives: BTreeMap<BusInteraction<F>, usize>,
}

impl<F: fmt::Display> fmt::Display for Error<F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "Bus interaction {} failed for tuple {}:",
            self.tuple[0],
            self.tuple.iter().skip(1).map(|v| v.to_string()).join(", ")
        )?;
        for (
            BusInteraction {
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
            BusInteraction {
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
pub struct BusInteraction<F> {
    pub machine: String,
    pub interaction: PhantomBusInteractionIdentity<F>,
}

impl<F: FieldElement> BusInteraction<F> {
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
                // Localize the interaction assuming a single namespace is accessed.
                let machine = unique_referenced_namespaces(&interaction).unwrap();
                let interaction = localize(interaction, global_pil, &machine_to_pil[&machine]);
                BusInteraction {
                    machine,
                    interaction,
                }
            })
            .collect()
    }
}

impl<'a, F: FieldElement> BusChecker<'a, F> {
    pub fn new(
        connections: &'a [BusInteraction<F>],
        machines: &'a BTreeMap<String, Machine<'a, F>>,
    ) -> Self {
        Self {
            connections,
            machines,
        }
    }

    pub fn check(&self) -> Result<(), Vec<Error<F>>> {
        #[derive(Default)]
        struct TupleState<'a, F> {
            sends: BTreeMap<&'a BusInteraction<F>, usize>,
            receives: BTreeMap<&'a BusInteraction<F>, usize>,
        }

        type BusState<'a, F> = BTreeMap<Vec<F>, TupleState<'a, F>>;

        let bus_state: BusState<F> = self
            .machines
            .into_par_iter()
            .flat_map(|(name, machine)| {
                (0..machine.size).into_par_iter().flat_map(|row_id| {
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
                        .collect::<Vec<_>>()
                })
            })
            // fold the interactions from a row into a state
            .fold(
                BusState::default,
                |mut state, (bus_connection, tuple, multiplicity)| {
                    let abs = multiplicity.unsigned_abs() as usize;

                    match multiplicity.cmp(&0) {
                        // if the multiplicity is zero, we don't need to do anything
                        Ordering::Equal => {}
                        // if the multiplicity is positive, we send
                        Ordering::Greater => {
                            let TupleState { sends, .. } = state.entry(tuple).or_default();
                            sends
                                .entry(bus_connection)
                                .and_modify(|sends| *sends += abs)
                                .or_insert(abs);
                        }
                        // if the multiplicity is negative, we receive
                        Ordering::Less => {
                            let TupleState { receives, .. } = state.entry(tuple).or_default();
                            receives
                                .entry(bus_connection)
                                .and_modify(|receives| *receives += abs)
                                .or_insert(abs);
                        }
                    }

                    state
                },
            )
            // combine all the states to one
            .reduce(
                BusState::default,
                |mut a, b| {
                    for (tuple, TupleState { sends, receives }) in b {
                        let TupleState {
                            sends: sends_a,
                            receives: receives_a,
                        } = a.entry(tuple).or_default();

                        for (bus_connection, count) in sends {
                            sends_a
                                .entry(bus_connection)
                                .and_modify(|sends| *sends += count)
                                .or_insert(count);
                        }

                        for (bus_connection, count) in receives {
                            receives_a
                                .entry(bus_connection)
                                .and_modify(|receives| *receives += count)
                                .or_insert(count);
                        }
                    }

                    a
                },
            );

        let mut errors = vec![];

        for (tuple, TupleState { sends, receives }) in bus_state {
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
