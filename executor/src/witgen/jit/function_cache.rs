use std::{collections::HashMap, hash::Hash};

use bit_vec::BitVec;
use itertools::Itertools;
use powdr_number::{FieldElement, KnownField};

use crate::witgen::{
    data_structures::finalizable_data::{ColumnLayout, CompactDataRef},
    jit::{effect::format_code, processor::ProcessorResult},
    machines::{
        profiling::{record_end, record_start},
        LookupCell, MachineParts,
    },
    range_constraints::RangeConstraint,
    EvalError, FixedData, MutableState, QueryCallback,
};

use super::{
    block_machine_processor::BlockMachineProcessor,
    compiler::{compile_effects, WitgenFunction},
    variable::Variable,
    witgen_inference::CanProcessCall,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct CacheKey<T: FieldElement> {
    identity_id: u64,
    /// If `Some((index, value))`, then this function is used only if the
    /// `index`th argument is set to `value`.
    known_concrete: Option<(usize, T)>,
    known_args: BitVec,
}

pub struct FunctionCache<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    /// The processor that generates the JIT code
    processor: BlockMachineProcessor<'a, T>,
    /// The cache of JIT functions and the returned range constraints.
    /// If the entry is None, we attempted to generate the function but failed.
    witgen_functions: HashMap<CacheKey<T>, Option<CacheEntry<T>>>,
    column_layout: ColumnLayout,
    block_size: usize,
    machine_name: String,
    parts: MachineParts<'a, T>,
}

pub struct CacheEntry<T: FieldElement> {
    pub function: WitgenFunction<T>,
    pub range_constraints: Vec<RangeConstraint<T>>,
}

impl<'a, T: FieldElement> FunctionCache<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        parts: MachineParts<'a, T>,
        block_size: usize,
        latch_row: usize,
        metadata: ColumnLayout,
        machine_name: String,
    ) -> Self {
        let processor =
            BlockMachineProcessor::new(fixed_data, parts.clone(), block_size, latch_row);

        FunctionCache {
            fixed_data,
            processor,
            column_layout: metadata,
            witgen_functions: HashMap::new(),
            block_size,
            machine_name,
            parts,
        }
    }

    /// Compiles the JIT function for the given identity and known arguments, and a potentially
    /// fully known argument.
    /// Returns the function and the output range constraints if the function was successfully compiled.
    pub fn compile_cached(
        &mut self,
        can_process: impl CanProcessCall<T>,
        identity_id: u64,
        known_args: &BitVec,
        known_concrete: Option<(usize, T)>,
    ) -> &Option<CacheEntry<T>> {
        // First try the generic version, then the specific.
        let mut key = CacheKey {
            identity_id,
            known_args: known_args.clone(),
            known_concrete: None,
        };

        if self.ensure_cache(can_process.clone(), &key).is_none() && known_concrete.is_some() {
            key = CacheKey {
                identity_id,
                known_args: known_args.clone(),
                known_concrete,
            };
            self.ensure_cache(can_process.clone(), &key);
        }
        self.ensure_cache(can_process, &key)
    }

    fn ensure_cache(
        &mut self,
        can_process: impl CanProcessCall<T>,
        cache_key: &CacheKey<T>,
    ) -> &Option<CacheEntry<T>> {
        if !self.witgen_functions.contains_key(cache_key) {
            record_start("Auto-witgen code derivation");
            let f = match T::known_field() {
                // Currently, we only support the Goldilocks fields
                Some(KnownField::GoldilocksField) => {
                    self.compile_witgen_function(can_process, cache_key)
                }
                _ => None,
            };
            assert!(self.witgen_functions.insert(cache_key.clone(), f).is_none());
            record_end("Auto-witgen code derivation");
        }
        self.witgen_functions.get(cache_key).unwrap()
    }

    fn compile_witgen_function(
        &self,
        can_process: impl CanProcessCall<T>,
        cache_key: &CacheKey<T>,
    ) -> Option<CacheEntry<T>> {
        log::debug!(
            "Compiling JIT function for\n  Machine: {}\n  Connection: {}\n   Inputs: {:?}{}",
            self.machine_name,
            self.parts.connections[&cache_key.identity_id],
            cache_key.known_args,
            cache_key
                .known_concrete
                .map(|(i, v)| format!("\n   Input {i} = {v}"))
                .unwrap_or_default()
        );

        let (
            ProcessorResult {
                code,
                range_constraints,
            },
            prover_functions,
        ) = self
            .processor
            .generate_code(
                can_process,
                cache_key.identity_id,
                &cache_key.known_args,
                cache_key.known_concrete,
            )
            .map_err(|e| {
                // These errors can be pretty verbose and are quite common currently.
                log::debug!(
                    "=> Error generating JIT code: {}\n...",
                    e.to_string().lines().take(5).join("\n")
                );
            })
            .ok()?;

        log::debug!("=> Success!");
        let out_of_bounds_vars = code
            .iter()
            .flat_map(|effect| effect.referenced_variables())
            .filter_map(|var| match var {
                Variable::WitnessCell(cell) => Some(cell),
                _ => None,
            })
            .filter(|cell| cell.row_offset < -1 || cell.row_offset >= self.block_size as i32)
            .collect_vec();
        if !out_of_bounds_vars.is_empty() {
            log::debug!("Code:\n{}", format_code(&code));
            panic!(
                "Expected JITed code to only reference cells in the block + the last row \
                of the previous block, i.e. rows -1 until (including) {}, but it does reference the following:\n{}",
                self.block_size - 1,
                out_of_bounds_vars.iter().format(", ")
            );
        }

        log::trace!("Generated code ({} steps)", code.len());
        let known_inputs = cache_key
            .known_args
            .iter()
            .enumerate()
            .filter_map(|(i, b)| if b { Some(Variable::Param(i)) } else { None })
            .collect::<Vec<_>>();

        log::trace!("Compiling effects...");
        let function = compile_effects(
            self.fixed_data.analyzed,
            self.column_layout.clone(),
            &known_inputs,
            &code,
            prover_functions,
        )
        .unwrap();
        log::trace!("Compilation done.");

        Some(CacheEntry {
            function,
            range_constraints,
        })
    }

    pub fn process_lookup_direct<'c, 'd, Q: QueryCallback<T>>(
        &self,
        mutable_state: &MutableState<'a, T, Q>,
        connection_id: u64,
        values: &mut [LookupCell<'c, T>],
        data: CompactDataRef<'d, T>,
        known_concrete: Option<(usize, T)>,
    ) -> Result<bool, EvalError<T>> {
        let known_args = values
            .iter()
            .map(|cell| cell.is_input())
            .collect::<BitVec>();

        let cache_key = CacheKey {
            identity_id: connection_id,
            known_args: known_args.clone(),
            known_concrete,
        };

        self.witgen_functions
            .get(&cache_key)
            .or_else(|| {
                self.witgen_functions.get(&CacheKey {
                    identity_id: connection_id,
                    known_args: known_args.clone(),
                    known_concrete: None,
                })
            })
            .expect("Need to call compile_cached() first!")
            .as_ref()
            .expect("compile_cached() returned false!")
            .function
            .call(self.fixed_data, mutable_state, values, data);

        Ok(true)
    }
}
