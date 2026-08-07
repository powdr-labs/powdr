use std::collections::{BTreeMap, HashMap};

use itertools::Itertools;
use openvm_instructions::VmOpcode;
use openvm_stark_backend::p3_maybe_rayon::prelude::{IntoParallelIterator, ParallelIterator};
use powdr_autoprecompiles::trace_handler::{
    resolve_computation_method, DummyCoord, OriginalRowReference, ResolvedMethod, TraceTrait,
};
use serde::{Deserialize, Serialize};

use crate::isa::{IsaApc, OpenVmISA};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct SubstitutionMeta {
    pub(crate) original_poly_index: usize,
    pub(crate) apc_poly_id: u64,
    pub(crate) apc_index: Option<usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct InstructionMeta {
    pub(crate) air_name: String,
    pub(crate) occurrence_per_call: usize,
    pub(crate) table_offset: usize,
    pub(crate) substitutions: Vec<SubstitutionMeta>,
}

/// Backend-agnostic trace-generation metadata, derived once from an APC.
#[derive(Clone, Serialize, Deserialize)]
pub struct ApcTraceGenMeta {
    pub(crate) apc_poly_id_to_index: BTreeMap<u64, usize>,
    pub(crate) instructions: Vec<InstructionMeta>,
}

#[derive(Debug, Clone)]
pub(crate) struct CpuInstructionMeta {
    pub(crate) copy_pairs: Vec<(usize, usize)>,
}

#[derive(Clone)]
pub struct CpuTraceGenMeta<F> {
    pub(crate) instructions: Vec<CpuInstructionMeta>,
    pub(crate) columns_to_compute: Vec<(usize, ResolvedMethod<F>)>,
}

#[cfg(feature = "cuda")]
#[derive(Debug, Clone)]
pub(crate) struct GpuAirMeta {
    pub(crate) air_name: String,
    pub(crate) instruction_indices: Vec<usize>,
}

#[cfg(feature = "cuda")]
#[derive(Clone)]
pub struct GpuTraceGenMeta {
    pub(crate) airs: Vec<GpuAirMeta>,
}

impl ApcTraceGenMeta {
    pub(crate) fn new<F, ISA: OpenVmISA>(
        apc: &IsaApc<F, ISA>,
        opcode_to_air: &HashMap<VmOpcode, String>,
    ) -> Self {
        let apc_poly_id_to_index = apc
            .machine()
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect::<BTreeMap<_, _>>();

        let instructions_with_subs = apc
            .instructions()
            .zip_eq(apc.subs().iter())
            .filter(|(_, subs)| !subs.is_empty())
            .map(|(instruction, subs)| (opcode_to_air[&instruction.inner.opcode].clone(), subs))
            .collect::<Vec<_>>();

        let air_name_occurrences = instructions_with_subs
            .iter()
            .map(|(air_name, _)| air_name.clone())
            .counts();

        let mut air_name_counts = HashMap::new();
        let mut instructions = Vec::with_capacity(instructions_with_subs.len());

        for (air_name, substitutions) in instructions_with_subs.iter() {
            let count = air_name_counts.entry(air_name.clone()).or_default();
            let table_offset = *count;
            *count += 1;

            let substitutions = substitutions
                .iter()
                .map(|substitution| SubstitutionMeta {
                    original_poly_index: substitution.original_poly_index,
                    apc_poly_id: substitution.apc_poly_id,
                    apc_index: apc_poly_id_to_index.get(&substitution.apc_poly_id).copied(),
                })
                .collect();

            instructions.push(InstructionMeta {
                air_name: air_name.clone(),
                occurrence_per_call: air_name_occurrences[air_name],
                table_offset,
                substitutions,
            });
        }

        Self {
            apc_poly_id_to_index,
            instructions,
        }
    }

    pub(crate) fn width(&self) -> usize {
        self.apc_poly_id_to_index.len()
    }

    pub(crate) fn dummy_values<'a, F, M>(
        &self,
        air_name_to_dummy_trace: &'a HashMap<String, M>,
        apc_call_count: usize,
    ) -> Vec<Vec<OriginalRowReference<'a, M::Values>>>
    where
        F: Send + Sync,
        M: TraceTrait<F>,
    {
        (0..apc_call_count)
            .into_par_iter()
            .map(|trace_row| {
                self.instructions
                    .iter()
                    .map(|instruction| {
                        let trace = air_name_to_dummy_trace.get(&instruction.air_name).unwrap();
                        let width = trace.width();
                        let start = (trace_row * instruction.occurrence_per_call
                            + instruction.table_offset)
                            * width;

                        OriginalRowReference {
                            data: trace.values(),
                            start,
                            length: width,
                        }
                    })
                    .collect()
            })
            .collect()
    }
}

impl<F: Clone> CpuTraceGenMeta<F> {
    pub(crate) fn new<ISA: OpenVmISA>(meta: &ApcTraceGenMeta, apc: &IsaApc<F, ISA>) -> Self {
        let instructions = meta
            .instructions
            .iter()
            .map(|instruction| CpuInstructionMeta {
                copy_pairs: instruction
                    .substitutions
                    .iter()
                    .filter_map(|substitution| {
                        substitution
                            .apc_index
                            .map(|apc_index| (substitution.original_poly_index, apc_index))
                    })
                    .collect(),
            })
            .collect();

        let apc_poly_id_to_dummy_index: BTreeMap<u64, DummyCoord> = meta
            .instructions
            .iter()
            .enumerate()
            .flat_map(|(instruction, cached)| {
                cached.substitutions.iter().map(move |substitution| {
                    (
                        substitution.apc_poly_id,
                        DummyCoord {
                            instruction,
                            index: substitution.original_poly_index,
                        },
                    )
                })
            })
            .collect();

        let columns_to_compute = apc
            .machine()
            .derived_columns
            .iter()
            .filter(|d| d.is_new)
            .map(|d| {
                (
                    meta.apc_poly_id_to_index[&d.variable.id],
                    resolve_computation_method(&d.computation_method, &apc_poly_id_to_dummy_index),
                )
            })
            .collect();

        Self {
            instructions,
            columns_to_compute,
        }
    }
}

#[cfg(feature = "cuda")]
impl GpuTraceGenMeta {
    pub(crate) fn new(meta: &ApcTraceGenMeta) -> Self {
        let mut air_index_by_name = HashMap::<String, usize>::new();
        let mut airs = Vec::<GpuAirMeta>::new();

        for (instruction_index, instruction) in meta.instructions.iter().enumerate() {
            let next_index = airs.len();
            let air_index = *air_index_by_name
                .entry(instruction.air_name.clone())
                .or_insert_with(|| {
                    airs.push(GpuAirMeta {
                        air_name: instruction.air_name.clone(),
                        instruction_indices: Vec::new(),
                    });
                    next_index
                });
            airs[air_index].instruction_indices.push(instruction_index);
        }

        Self { airs }
    }
}
