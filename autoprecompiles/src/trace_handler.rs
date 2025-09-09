use itertools::Itertools;
use rayon::prelude::*;
use std::collections::HashMap;
use std::ops::{Add, Mul, Neg, Sub};

use crate::adapter::Adapter;
use crate::expression::RowEvaluator;
use crate::Apc;
use crate::InstructionHandler;
use crate::SymbolicBusInteraction;

/// Returns data needed for constructing the APC trace.
pub struct TraceHandlerData<'a, F> {
    /// The dummy trace values for each instruction.
    pub dummy_values: Vec<Vec<&'a [F]>>,
    /// The mapping from dummy trace index to APC index for each instruction.
    pub dummy_trace_index_to_apc_index_by_instruction: Vec<HashMap<usize, usize>>,
}

pub trait TraceHandler<A: Adapter> {
    /// Returns reference to the dummy instruction handler
    fn instruction_handler(&self) -> &A::InstructionHandler;

    /// Returns the number of APC calls, which is also the number of rows in the APC trace
    fn apc_call_count(&self) -> usize;

    /// Returns a mapping from air_id to the dummy trace
    fn air_id_to_dummy_trace(&self) -> &HashMap<A::AirId, Trace<A::Field>>;

    /// Returns the data needed for constructing the APC trace, namely the dummy traces and the mapping from dummy trace index to APC index for each instruction
    fn data<'a>(&'a self, apc: &Apc<A::Field, A::Instruction>) -> TraceHandlerData<'a, A::Field> {
        let air_id_to_dummy_trace = self.air_id_to_dummy_trace();

        // Returns a vector with the same length as original instructions
        let original_instruction_air_ids = apc
            .instructions()
            .iter()
            .map(|instruction| {
                self.instruction_handler()
                    .get_instruction_air_id(instruction)
            })
            .collect::<Vec<_>>();

        let air_id_occurrences = original_instruction_air_ids.iter().counts();

        let apc_poly_id_to_index: HashMap<u64, usize> = apc
            .machine
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect();

        let original_instruction_table_offsets = original_instruction_air_ids
            .iter()
            .scan(
                HashMap::default(),
                |counts: &mut HashMap<&A::AirId, usize>, air_id| {
                    let count = counts.entry(air_id).or_default();
                    let current_count = *count;
                    *count += 1;
                    Some(current_count)
                },
            )
            .collect::<Vec<_>>();

        let dummy_trace_index_to_apc_index_by_instruction = apc
            .subs
            .iter()
            .map(|subs| {
                let mut dummy_trace_index_to_apc_index = HashMap::new();
                for (dummy_index, poly_id) in subs.iter().enumerate() {
                    if let Some(apc_index) = apc_poly_id_to_index.get(poly_id) {
                        dummy_trace_index_to_apc_index.insert(dummy_index, *apc_index);
                    }
                }
                dummy_trace_index_to_apc_index
            })
            .collect::<Vec<_>>();

        let dummy_values = (0..self.apc_call_count())
            .into_par_iter()
            .map(|trace_row| {
                original_instruction_air_ids
                    .iter()
                    .zip_eq(original_instruction_table_offsets.iter())
                    .map(|(air_id, dummy_table_offset)| {
                        let Trace { values, width } = air_id_to_dummy_trace.get(air_id).unwrap();
                        let occurrences_per_record = air_id_occurrences.get(air_id).unwrap();
                        let start =
                            (trace_row * occurrences_per_record + dummy_table_offset) * width;
                        let end = start + width;
                        &values[start..end]
                    })
                    .collect_vec()
            })
            .collect();

        TraceHandlerData {
            dummy_values,
            dummy_trace_index_to_apc_index_by_instruction,
        }
    }
}

pub struct Trace<F> {
    pub values: Vec<F>,
    pub width: usize,
}

impl<F> Trace<F> {
    pub fn new(values: Vec<F>, width: usize) -> Self {
        Self { values, width }
    }
}

pub struct InteractionEvaluator<
    'a,
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy,
> {
    pub row_evaluator: RowEvaluator<'a, F>,
}

impl<'a, F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy>
    InteractionEvaluator<'a, F>
{
    pub fn new(row_evaluator: RowEvaluator<'a, F>) -> Self {
        Self { row_evaluator }
    }

    pub fn evaluate_bus_interactions(
        &self,
        bus_interactions: &Vec<&SymbolicBusInteraction<F>>,
        filter_by: impl Fn(&SymbolicBusInteraction<F>) -> bool,
    ) -> Vec<ConcreteBusInteraction<F>> {
        bus_interactions
            .iter()
            .filter(|&bus_interaction| filter_by(bus_interaction))
            .map(|bus_interaction| {
                let mult = self.row_evaluator.eval_expr(&bus_interaction.mult);
                let args = bus_interaction
                    .args
                    .iter()
                    .map(|arg| self.row_evaluator.eval_expr(arg))
                    .collect_vec();
                ConcreteBusInteraction {
                    id: bus_interaction.id,
                    mult,
                    args,
                }
            })
            .collect()
    }
}

pub struct ConcreteBusInteraction<F> {
    pub id: u64,
    pub mult: F,
    pub args: Vec<F>,
}
