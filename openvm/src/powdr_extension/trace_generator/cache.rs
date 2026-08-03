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
pub(crate) struct CachedSubstitution {
    pub(crate) original_poly_index: usize,
    pub(crate) apc_poly_id: u64,
    pub(crate) apc_index: Option<usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct CachedInstruction {
    pub(crate) air_name: String,
    pub(crate) occurrence_per_call: usize,
    pub(crate) table_offset: usize,
    pub(crate) substitutions: Vec<CachedSubstitution>,
}

/// Shared APC metadata reused by backend-specific trace generators.
#[derive(Clone, Serialize, Deserialize)]
#[serde(bound(serialize = "F: Serialize", deserialize = "F: Deserialize<'de>"))]
pub struct CachedApc<F, ISA: OpenVmISA> {
    pub(crate) apc: IsaApc<F, ISA>,
    pub(crate) apc_poly_id_to_index: BTreeMap<u64, usize>,
    pub(crate) instructions: Vec<CachedInstruction>,
    pub(crate) apc_poly_id_to_dummy_index: BTreeMap<u64, DummyCoord>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct CachedInstructionCpu {
    pub(crate) copy_pairs: Vec<(usize, usize)>,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(bound(serialize = "F: Serialize", deserialize = "F: Deserialize<'de>"))]
pub struct CachedApcCpu<F> {
    pub(crate) instructions: Vec<CachedInstructionCpu>,
    pub(crate) columns_to_compute: Vec<(usize, ResolvedMethod<F>)>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct CachedGpuAir {
    pub(crate) air_name: String,
    pub(crate) instruction_indices: Vec<usize>,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct CachedApcGpu {
    pub(crate) airs: Vec<CachedGpuAir>,
}

impl<F: Clone, ISA: OpenVmISA> CachedApc<F, ISA> {
    pub(crate) fn new(apc: IsaApc<F, ISA>, opcode_to_air: &HashMap<VmOpcode, String>) -> Self {
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
        let mut apc_poly_id_to_dummy_index = BTreeMap::new();

        for (instruction_index, (air_name, substitutions)) in
            instructions_with_subs.iter().enumerate()
        {
            let count = air_name_counts.entry(air_name.clone()).or_default();
            let table_offset = *count;
            *count += 1;

            let substitutions = substitutions
                .iter()
                .map(|substitution| {
                    apc_poly_id_to_dummy_index.insert(
                        substitution.apc_poly_id,
                        DummyCoord {
                            instruction: instruction_index,
                            index: substitution.original_poly_index,
                        },
                    );

                    CachedSubstitution {
                        original_poly_index: substitution.original_poly_index,
                        apc_poly_id: substitution.apc_poly_id,
                        apc_index: apc_poly_id_to_index.get(&substitution.apc_poly_id).copied(),
                    }
                })
                .collect();

            instructions.push(CachedInstruction {
                air_name: air_name.clone(),
                occurrence_per_call: air_name_occurrences[air_name],
                table_offset,
                substitutions,
            });
        }

        Self {
            apc,
            apc_poly_id_to_index,
            instructions,
            apc_poly_id_to_dummy_index,
        }
    }

    pub(crate) fn width(&self) -> usize {
        self.apc_poly_id_to_index.len()
    }

    pub(crate) fn dummy_values<'a, M>(
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

impl<F: Clone> CachedApcCpu<F> {
    pub(crate) fn new<ISA: OpenVmISA>(apc: &CachedApc<F, ISA>) -> Self {
        let instructions = apc
            .instructions
            .iter()
            .map(|instruction| CachedInstructionCpu {
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

        let columns_to_compute = apc
            .apc
            .machine()
            .derived_columns
            .iter()
            .filter(|d| d.is_new)
            .map(|d| {
                (
                    apc.apc_poly_id_to_index[&d.variable.id],
                    resolve_computation_method(
                        &d.computation_method,
                        &apc.apc_poly_id_to_dummy_index,
                    ),
                )
            })
            .collect();

        Self {
            instructions,
            columns_to_compute,
        }
    }
}

impl CachedApcGpu {
    pub(crate) fn new<F, ISA: OpenVmISA>(apc: &CachedApc<F, ISA>) -> Self {
        let mut air_index_by_name = HashMap::<String, usize>::new();
        let mut airs = Vec::<CachedGpuAir>::new();

        for (instruction_index, instruction) in apc.instructions.iter().enumerate() {
            let next_index = airs.len();
            let air_index = *air_index_by_name
                .entry(instruction.air_name.clone())
                .or_insert_with(|| {
                    airs.push(CachedGpuAir {
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
