use std::{collections::HashMap, ffi::c_void, sync::Arc};

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

pub struct JitProcessor<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    parts: MachineParts<'a, T>,
    block_size: usize,
    latch_row: usize,
    witgen_functions: HashMap<(u64, BitVec), WitgenFunction<T>>,
}

impl<'a, T: FieldElement> JitProcessor<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        parts: MachineParts<'a, T>,
        block_size: usize,
        latch_row: usize,
    ) -> Self {
        JitProcessor {
            fixed_data,
            parts,
            block_size,
            latch_row,
            witgen_functions: HashMap::new(),
        }
    }

    pub fn can_answer_lookup(&self, identity_id: u64, known_inputs: &BitVec) -> bool {
        // TODO cache the result

        // TODO what if the same column is mentioned multiple times on the RHS of the connection?

        let right = self.parts.connections[&identity_id].right;
        let Some(known_inputs) = known_inputs
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
            known_inputs.iter().format(", ")
        );

        let known_inputs = known_inputs.into_iter().map(|p| p.poly_id);

        let mut inference = WitgenInference::new(
            self.fixed_data,
            &self.parts,
            self.block_size,
            self.latch_row,
            known_inputs,
            right,
        );
        if !inference.run() {
            return false;
        }
        log::info!("Successfully generated witgen code for some machine.");
        let code = inference.code();
        log::trace!("Generated code:\n{code}");

        let lib_path = powdr_jit_compiler::compiler::call_cargo(&code).unwrap();
        true
    }

    pub fn process_lookup_direct<'b, 'c, 'd, Q: QueryCallback<T>>(
        &self,
        _mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        connection_id: u64,
        values: Vec<LookupCell<'c, T>>,
        // TODO maybe just a `*mut T` plus first_col would be best?
        mut data: CompactDataRef<'d, T>,
    ) -> Result<bool, EvalError<T>> {
        // Transfer inputs.
        let right = self.parts.connections[&connection_id].right;
        let mut known_inputs = BitVec::new();
        for (e, v) in right.expressions.iter().zip(&values) {
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

        self.witgen_functions[&(connection_id, known_inputs)].call(
            &mut data,
            Self::get_cell,
            Self::set_cell,
        );

        Ok(true)
    }

    extern "C" fn get_cell(data: *mut c_void, row: i32, col: u64) -> T {
        let data = unsafe { &*(data as *const CompactDataRef<T>) };
        data.get(row, col as u32)
    }

    extern "C" fn set_cell(data: *mut c_void, row: i32, col: u64, value: T) {
        let data = unsafe { &mut *(data as *mut CompactDataRef<T>) };
        data.set(row, col as u32, value);
    }
}

struct WitgenFunction<T> {
    #[allow(dead_code)]
    library: Arc<libloading::Library>,
    prover_function: extern "C" fn(
        *mut c_void,
        extern "C" fn(*mut c_void, i32, u64) -> T,
        extern "C" fn(*mut c_void, i32, u64, T),
    ),
}

impl<T: FieldElement> WitgenFunction<T> {
    fn call(
        &self,
        data: &mut CompactDataRef<T>,
        get_cell: extern "C" fn(*mut c_void, i32, u64) -> T,
        set_cell: extern "C" fn(*mut c_void, i32, u64, T),
    ) {
        let data_c_ptr = data as *mut _ as *mut c_void;
        (self.prover_function)(data_c_ptr, get_cell, set_cell);
    }
}
