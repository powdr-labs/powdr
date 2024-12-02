use std::{
    collections::HashMap,
    ffi::c_void,
    sync::{Arc, RwLock},
};

use bit_vec::BitVec;
use itertools::Itertools;
use powdr_number::FieldElement;

use crate::witgen::{
    data_structures::finalizable_data::CompactDataRef,
    jit::witgen_inference::WitgenInference,
    machines::{LookupCell, MachineParts},
    util::try_to_simple_poly,
    EvalError, FixedData, MutableState, QueryCallback,
};

use super::cell::Cell;

pub struct JitProcessor<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    machine_name: String,
    parts: MachineParts<'a, T>,
    block_size: usize,
    latch_row: usize,
    // TODO also cache negative results?
    witgen_functions: RwLock<HashMap<Option<(u64, BitVec)>, (WitgenFunction, Vec<Cell>)>>,
}

impl<'a, T: FieldElement> JitProcessor<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        machine_name: String,
        parts: MachineParts<'a, T>,
        block_size: usize,
        latch_row: usize,
    ) -> Self {
        JitProcessor {
            fixed_data,
            machine_name,
            parts,
            block_size,
            latch_row,
            witgen_functions: RwLock::new(HashMap::new()),
        }
    }

    pub fn can_answer_lookup(&self, identity_id: u64, known_inputs: BitVec) -> bool {
        self.can_handle(Some((identity_id, known_inputs)))
    }

    pub fn can_run(&self) -> bool {
        // TODO we need which columns are already known on the current row.
        // For these, we have to create new values on the next row.
        // for all others, we have to create values on the current row.
        //
        // These columns are determined eihter through a connection or through evaluating
        // the last row once.
        //
        // TODO although this is also kind of wrong: The columns known on the first row
        // might be different from the ones that can be transferred on every row.
        // So two-step approach:
        // 1. mark every columns known on some row (no particular row).
        // Check which columns can be computed in the next now.
        // Now only mark those known and check that the rest can be computed fully.
        // But this is essentially just a condition that we can always go from a full row to a full next row.
        // How do we ensur ethat the first row is full to begin with?
        // -> of course! Either use the interpreter or run the algorithm on a specific row.
        //

        // So we call `can_hanndle` with "all columns known on the latch row".
        self.can_handle(None)
    }

    fn can_handle(&self, key: Option<(u64, BitVec)>) -> bool {
        if T::BITS > 64 {
            return false;
        }

        if self.witgen_functions.read().unwrap().contains_key(&key) {
            return true;
        }

        let (known_input_cols, right) = if let Some((identity_id, known_inputs)) = &key {
            // TODO what if the same column is mentioned multiple times on the RHS of the connection?
            let right = self.parts.connections[&identity_id].right;
            let Some(known_input_cols) = known_inputs
                .iter()
                .zip(&right.expressions)
                .filter(|(known, _)| *known)
                .map(|(_, e)| try_to_simple_poly(e))
                .collect::<Option<Vec<_>>>()
            else {
                return false;
            };
            log::debug!(
                "Trying to auto-generate witgen code for known inputs: {}",
                known_input_cols.iter().format(", ")
            );
            (
                known_input_cols
                    .into_iter()
                    .map(|p| p.poly_id)
                    .collect_vec(),
                Some(right),
            )
        } else {
            log::debug!(
                    "Trying to auto-generate witgen code for free-running machine without known inputs.",
                );
            // All columns are known on the latch row.
            (self.parts.witnesses.iter().cloned().collect_vec(), None)
        };

        let mut inference = WitgenInference::new(
            self.fixed_data,
            &self.parts,
            self.block_size,
            self.latch_row,
            known_input_cols,
            right,
        );
        if !inference.run() {
            return false;
        }
        log::info!(
            "Successfully generated witgen code machine {}.",
            self.machine_name
        );
        let (code, known_after) = inference.code_and_known_cells("witgen");
        log::trace!("Generated code:\n{code}");

        let lib_path = powdr_jit_compiler::compiler::call_cargo(&code)
            .map_err(|e| {
                log::error!("Failed to compile generated code: {}", e);
                false
            })
            .unwrap();

        let library = Arc::new(unsafe { libloading::Library::new(&lib_path.path).unwrap() });
        // TODO what happen if there is a conflict in function names? Should we
        // encode the ID and the known inputs?
        let witgen_fun = unsafe {
            library
                .get::<extern "C" fn(WitgenFunctionParams, *mut c_void, *const c_void)>(b"witgen")
        }
        .unwrap();

        self.witgen_functions.write().unwrap().insert(
            key,
            (
                WitgenFunction {
                    library: library.clone(),
                    witgen_function: *witgen_fun,
                },
                known_after,
            ),
        );
        true
    }

    pub fn process_lookup_direct<'b, 'c, 'd, Q: QueryCallback<T>>(
        &self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        connection_id: u64,
        values: &mut [LookupCell<'c, T>],
        // TODO maybe just a `*mut T` plus first_col would be best?
        mut data: CompactDataRef<'d, T>,
    ) -> Result<bool, EvalError<T>> {
        // Transfer inputs.
        let right = self.parts.connections[&connection_id].right;
        let mut known_inputs = BitVec::new();
        for (e, v) in right.expressions.iter().zip(values.iter()) {
            match v {
                LookupCell::Input(&v) => {
                    let col = try_to_simple_poly(e).unwrap();
                    data.set(self.latch_row as i32, col.poly_id.id as u32, v);
                    known_inputs.push(true);
                }
                LookupCell::Output(_) => {
                    // TODO
                    known_inputs.push(false);
                }
            }
        }

        //let start = std::time::Instant::now();

        let (witgen_fun, known_after) =
            &self.witgen_functions.read().unwrap()[&Some((connection_id, known_inputs))];
        witgen_fun.call(&mut data, self.latch_row, mutable_state, process_lookup);

        //let end = start.elapsed();
        //println!("Witgen took {}", end.as_nanos());

        // TODO maybe do this inside witgen_fun::call?
        // for Cell { id, row_offset, .. } in known_after {
        //     data.set_known(row_offset + self.latch_row as i32, *id as u32);
        // }

        // TODO shortcut this somehow
        for (e, v) in right.expressions.iter().zip(values) {
            match v {
                LookupCell::Input(_) => {}
                LookupCell::Output(c) => {
                    let col = try_to_simple_poly(e).unwrap();
                    **c = data.get(self.latch_row as i32, col.poly_id.id as u32);
                }
            }
        }

        Ok(true)
    }
}

fn process_lookup<'b, 'd, T: FieldElement, Q: QueryCallback<T>>(
    mutable_state: &'b mut MutableState<'_, 'b, T, Q>,
    identity_id: u64,
    values: &mut [LookupCell<'_, T>],
) -> bool {
    mutable_state
        .machines
        .call_direct(identity_id, values, mutable_state.query_callback)
        .unwrap()
}

struct WitgenFunction {
    #[allow(dead_code)]
    library: Arc<libloading::Library>,
    witgen_function: extern "C" fn(WitgenFunctionParams, *mut c_void, *const c_void),
}

#[repr(C)]
struct WitgenFunctionParams {
    data: *mut c_void,
    known: *mut u32,
    len: u64,
    row_offset: u64,
}

impl WitgenFunction {
    fn call<'a, 'b, 'c, 'd, T: FieldElement, Q: QueryCallback<T>>(
        &self,
        data: &mut CompactDataRef<T>,
        // TODO this applies a shift. Maybe we could do it much earlier?
        latch_row: usize,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        process_lookup: fn(
            &'b mut MutableState<'a, 'b, T, Q>,
            u64,
            &mut [LookupCell<'c, T>],
        ) -> bool,
    ) {
        let (data_slice, known_slice, row_offset) = data.direct_slice();
        let data_c_ptr = data_slice.as_mut_ptr() as *mut c_void;
        let len = data_slice.len() as u64;
        //        assert_eq!((len + 31) / 32, known_slice.len() as u64);
        let process_lookup_ptr = process_lookup as *const c_void;
        let mutable_state_ptr = mutable_state as *mut _ as *mut c_void;
        (self.witgen_function)(
            WitgenFunctionParams {
                data: data_c_ptr,
                known: known_slice.as_mut_ptr(),
                len,
                row_offset: row_offset as u64 + latch_row as u64,
            },
            mutable_state_ptr,
            process_lookup_ptr,
        );
    }
}
