use std::{collections::HashMap, hash::Hash};

use bit_vec::BitVec;
use itertools::Itertools;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_number::{FieldElement, KnownField};

use crate::witgen::{
    data_structures::finalizable_data::{ColumnLayout, CompactDataRef},
    jit::{effect::format_code, processor::ProcessorResult},
    machines::{
        profiling::{record_end, record_start},
        LookupCell, MachineParts,
    },
    EvalError, FixedData, MutableState, QueryCallback,
};

use super::{
    block_machine_processor::BlockMachineProcessor,
    compiler::{compile_effects, CompiledFunction},
    effect::{Effect, ProverFunctionCall},
    interpreter::EffectsInterpreter,
    prover_function_heuristics::{ProverFunction, ProverFunctionComputation},
    variable::Variable,
    witgen_inference::CanProcessCall,
};

/// Inferred witness generation routines that are larger than
/// this number of "statements" will use the interpreter instead of the compiler
/// due to the large compilation ressources required.
const MAX_COMPILED_CODE_SIZE: usize = 1000;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct CacheKey<T: FieldElement> {
    bus_id: T,
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
    witgen_functions: HashMap<CacheKey<T>, Option<CacheEntry<'a, T>>>,
    column_layout: ColumnLayout,
    block_size: usize,
    machine_name: String,
    parts: MachineParts<'a, T>,
}

enum WitgenFunction<'a, T: FieldElement> {
    Compiled(CompiledFunction<T>),
    Interpreted(EffectsInterpreter<'a, T>),
}

impl<T: FieldElement> WitgenFunction<'_, T> {
    /// Call the witgen function to fill the data and "known" tables
    /// given a slice of parameters.
    /// The `row_offset` is the index inside `data` of the row considered to be "row zero".
    /// This function always succeeds (unless it panics).
    pub fn call<Q: QueryCallback<T>>(
        &self,
        fixed_data: &FixedData<'_, T>,
        mutable_state: &MutableState<'_, T, Q>,
        params: &mut [LookupCell<T>],
        data: CompactDataRef<'_, T>,
    ) {
        match self {
            WitgenFunction::Compiled(compiled_function) => {
                compiled_function.call(fixed_data, mutable_state, params, data);
            }
            WitgenFunction::Interpreted(interpreter) => {
                interpreter.call::<Q>(fixed_data, mutable_state, params, data)
            }
        }
    }
}

pub struct CacheEntry<'a, T: FieldElement> {
    function: WitgenFunction<'a, T>,
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
        bus_id: T,
        known_args: &BitVec,
        known_concrete: Option<(usize, T)>,
    ) -> &Option<CacheEntry<T>> {
        // First try the generic version, then the specific.
        let mut key = CacheKey {
            bus_id,
            known_args: known_args.clone(),
            known_concrete: None,
        };

        if self.ensure_cache(can_process.clone(), &key).is_none() && known_concrete.is_some() {
            key = CacheKey {
                bus_id,
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
            let compiled = self
                .derive_witgen_function(can_process, cache_key)
                .and_then(|(result, prover_functions)| {
                    self.compile_witgen_function(result, prover_functions, cache_key)
                });
            assert!(self
                .witgen_functions
                .insert(cache_key.clone(), compiled)
                .is_none());

            record_end("Auto-witgen code derivation");
        }
        self.witgen_functions.get(cache_key).unwrap()
    }

    fn derive_witgen_function(
        &self,
        can_process: impl CanProcessCall<T>,
        cache_key: &CacheKey<T>,
    ) -> Option<(ProcessorResult<T>, Vec<ProverFunction<'a, T>>)> {
        log::debug!(
            "Compiling JIT function for\n  Machine: {}\n  Connection: {}\n   Inputs: {:?}{}",
            self.machine_name,
            self.parts.bus_receives[&cache_key.bus_id],
            cache_key.known_args,
            cache_key
                .known_concrete
                .map(|(i, v)| format!("\n   Input {i} = {v}"))
                .unwrap_or_default()
        );

        let (processor_result, prover_functions) = self
            .processor
            .generate_code(
                can_process,
                cache_key.bus_id,
                &cache_key.known_args,
                cache_key.known_concrete,
            )
            .map_err(|e| {
                // These errors can be pretty verbose and are quite common currently.
                log::debug!("=> Error generating JIT code:\n{e}",);
            })
            .ok()?;

        log::debug!("=> Success!");
        let out_of_bounds_vars = processor_result
            .code
            .iter()
            .flat_map(|effect| effect.referenced_variables())
            .filter_map(|var| match var {
                Variable::WitnessCell(cell) => Some(cell),
                _ => None,
            })
            .filter(|cell| cell.row_offset < -1 || cell.row_offset >= self.block_size as i32)
            .collect_vec();
        if !out_of_bounds_vars.is_empty() {
            log::debug!("Code:\n{}", format_code(&processor_result.code));
            panic!(
                "Expected JITed code to only reference cells in the block + the last row \
                of the previous block, i.e. rows -1 until (including) {}, but it does reference the following:\n{}",
                self.block_size - 1,
                out_of_bounds_vars.iter().format(", ")
            );
        }

        log::trace!("Generated code ({} steps)", processor_result.code.len());
        Some((processor_result, prover_functions))
    }

    fn compile_witgen_function(
        &self,
        result: ProcessorResult<T>,
        prover_functions: Vec<ProverFunction<'a, T>>,
        cache_key: &CacheKey<T>,
    ) -> Option<CacheEntry<'a, T>> {
        let known_inputs = cache_key
            .known_args
            .iter()
            .enumerate()
            .filter_map(|(i, b)| if b { Some(Variable::Param(i)) } else { None })
            .collect::<Vec<_>>();

        // Use the compiler for goldilocks with at most MAX_COMPILED_CODE_SIZE statements and
        // the interpreter otherwise.
        let interpreted = !matches!(T::known_field(), Some(KnownField::GoldilocksField))
            || code_size(&result.code) > MAX_COMPILED_CODE_SIZE;

        if interpreted && has_input_output_prover_function_call(&prover_functions, &result.code) {
            // TODO we still need to implement this.
            log::debug!("Interpreter does not yet implement input/output prover functions.");
            return None;
        }

        let function = if interpreted {
            log::trace!("Building effects interpreter...");
            WitgenFunction::Interpreted(EffectsInterpreter::new(
                &known_inputs,
                &result.code,
                prover_functions,
            ))
        } else {
            log::trace!("Compiling effects...");
            WitgenFunction::Compiled(
                compile_effects(
                    self.fixed_data.analyzed,
                    self.column_layout.clone(),
                    &known_inputs,
                    &result.code,
                    prover_functions,
                )
                .unwrap(),
            )
        };
        log::trace!("Compilation done.");

        Some(CacheEntry {
            function,
            range_constraints: result.range_constraints,
        })
    }

    pub fn process_lookup_direct<'c, 'd, Q: QueryCallback<T>>(
        &self,
        mutable_state: &MutableState<'a, T, Q>,
        bus_id: T,
        values: &mut [LookupCell<'c, T>],
        data: CompactDataRef<'d, T>,
        known_concrete: Option<(usize, T)>,
    ) -> Result<bool, EvalError<T>> {
        let known_args = values
            .iter()
            .map(|cell| cell.is_input())
            .collect::<BitVec>();

        let cache_key = CacheKey {
            bus_id,
            known_args: known_args.clone(),
            known_concrete,
        };

        self.witgen_functions
            .get(&cache_key)
            .or_else(|| {
                self.witgen_functions.get(&CacheKey {
                    bus_id,
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

/// Returns the elements in the code and thus a rough estimate of the number of steps
fn code_size<T: FieldElement>(code: &[Effect<T, Variable>]) -> usize {
    code.iter()
        .map(|effect| match effect {
            Effect::Assignment(..)
            | Effect::BitDecomposition(..)
            | Effect::Assertion(..)
            | Effect::MachineCall(..)
            | Effect::ProverFunctionCall(..) => 1,
            Effect::RangeConstraint(..) => unreachable!(),
            Effect::Branch(_, first, second) => code_size(first) + code_size(second) + 1,
        })
        .sum()
}

/// Returns true if there is any input/output prover function call in the code.
fn has_input_output_prover_function_call<'a, T: FieldElement>(
    prover_functions: &[ProverFunction<'a, T>],
    code: impl IntoIterator<Item = &'a Effect<T, Variable>> + 'a,
) -> bool {
    code.into_iter().any(|effect| match effect {
        Effect::ProverFunctionCall(ProverFunctionCall { function_index, .. }) => {
            match prover_functions[*function_index].computation {
                ProverFunctionComputation::ProvideIfUnknown(..)
                | ProverFunctionComputation::ComputeFrom(..) => false,
                ProverFunctionComputation::HandleQueryInputOutput(..) => true,
            }
        }
        Effect::Branch(_, if_branch, else_branch) => has_input_output_prover_function_call(
            prover_functions,
            if_branch.iter().chain(else_branch),
        ),
        Effect::Assignment(..)
        | Effect::BitDecomposition(..)
        | Effect::RangeConstraint(..)
        | Effect::Assertion(..)
        | Effect::MachineCall(..) => false,
    })
}
