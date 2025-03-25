use super::decompose_lower32;
use super::poseidon_gl;
use powdr_number::{FieldElement, LargeInt};
use rayon::iter::IndexedParallelIterator;
use rayon::iter::IntoParallelIterator;
use rayon::iter::ParallelIterator;

use std::collections::HashMap;

use num_derive::{FromPrimitive, ToPrimitive};

macro_rules! witness_cols {
    ($name:ident, $($col:ident),*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ToPrimitive, FromPrimitive)]
        #[allow(non_camel_case_types)]
        #[repr(usize)]
        enum $name {
            $($col,)*
        }

        impl $name {
            fn all() -> Vec<Self> {
                vec![
                    $(Self::$col,)*
                ]
            }

            fn name(&self) -> &'static str {
                match *self {
                    $(Self::$col => stringify!($col),)*
                }
            }
        }
    };
}

macro_rules! witness_cols_str {
    ($name:ident, $($col:ident = $str:literal),*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ToPrimitive, FromPrimitive)]
        #[allow(non_camel_case_types)]
        #[repr(usize)]
        enum $name {
            $($col,)*
        }

        impl $name {
            fn all() -> Vec<Self> {
                vec![
                    $(Self::$col,)*
                ]
            }

            fn name(&self) -> &'static str {
                match *self {
                    $(Self::$col => $str,)*
                }
            }
        }
    };
}

fn only_column_name(name: &str) -> &str {
    // look backwards the "::" and return only the part after it
    name.rfind("::").map(|i| &name[i + 2..]).unwrap_or(name)
}

/// Each submachine kind (i.e., binary, shift) must implement this trait
trait SubmachineKind: Send {
    /// Which of the witness columns are selectors, if any
    const SELECTORS: &'static str;
    /// Row block size
    const BLOCK_SIZE: u32;
    /// List of non-selector witness columns
    fn cols() -> Vec<String>;
    /// Add an operation to the submachine trace
    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        selector: Option<&str>,
        lookup_args: &[F; 4],
        // extra info provided by the executor
        extra: &[F],
    );
    /// Some machines need more than simply copying first block
    fn dummy_block_fix<F: FieldElement>(_trace: &mut SubmachineTrace<F>, _rows: u32) {}
}

/// Trait only used for the constructor
pub trait SubmachineBoxed<F: FieldElement> {
    fn new_boxed(namespace: &str, witness_cols: &[String]) -> Box<dyn Submachine<F>>;
}

impl<F: FieldElement, M: SubmachineKind + 'static> SubmachineBoxed<F> for M {
    fn new_boxed(namespace: &str, witness_cols: &[String]) -> Box<dyn Submachine<F>> {
        Box::new(SubmachineImpl::<F, M>::new(namespace, witness_cols))
    }
}

/// Submachine interface, implemented by SubmachineImpl.
/// Each specific submachine only needs to implement the SubmachineKind trait.
/// Trace is built by calling these methods.
/// It being a trait also allows us to put different submachines in the same hashmap.
pub trait Submachine<F: FieldElement>: Send {
    /// submachine namespace
    fn namespace(&self) -> &str;
    /// current number of rows
    fn len(&self) -> u32;
    /// add a new operation to the trace
    fn add_operation(&mut self, selector: Option<&str>, lookup_args: &[F; 4], extra: &[F]);
    /// finish the trace, padding to the given degree and returning the machine columns.
    /// Ideally we'd take `self` here, but this is called from a `dyn Trait`...
    fn finish(&mut self, degree: u32) -> Vec<(String, Vec<F>)>;
}

/// Concrete implementation of the Submachine trait
struct SubmachineImpl<F: FieldElement, M: SubmachineKind> {
    trace: SubmachineTrace<F>,
    m: std::marker::PhantomData<M>,
    finished: bool,
}

impl<F: FieldElement, M: SubmachineKind> SubmachineImpl<F, M> {
    pub fn new(namespace: &str, witness_cols: &[String]) -> Self {
        // filter machine columns
        let prefix = format!("{namespace}::");
        let witness_cols: Vec<_> = witness_cols
            .iter()
            .filter(|c| c.starts_with(namespace))
            .map(|c| c.strip_prefix(&prefix).unwrap().to_string())
            .collect();
        let selectors = witness_cols
            .iter()
            .map(|c| c.to_string())
            .filter(|c| c.starts_with(&format!("{}[", M::SELECTORS)))
            .collect();
        let cols = M::cols();
        if witness_cols.is_empty() {
            log::info!("namespace {namespace} has no witness columns in the optimized pil");
        }
        SubmachineImpl {
            trace: SubmachineTrace::new(namespace, cols, selectors),
            m: std::marker::PhantomData,
            finished: false,
        }
    }
}

impl<F: FieldElement, M: SubmachineKind> Submachine<F> for SubmachineImpl<F, M> {
    fn namespace(&self) -> &str {
        self.trace.namespace.as_str()
    }

    fn len(&self) -> u32 {
        self.trace.len()
    }

    fn add_operation(&mut self, selector: Option<&str>, lookup_args: &[F; 4], extra: &[F]) {
        M::add_operation(&mut self.trace, selector, lookup_args, extra);
    }

    fn finish(&mut self, degree: u32) -> Vec<(String, Vec<F>)> {
        assert!(self.len() <= degree);
        assert!(!self.finished, "submachine finish called twice");
        self.finished = true;
        self.trace.final_row_override();
        while self.len() < degree {
            let prev_len = self.len();
            self.trace.push_dummy_block(degree, M::BLOCK_SIZE);
            let dummy_size = self.len() - prev_len;
            M::dummy_block_fix(&mut self.trace, dummy_size);
        }
        self.trace
            .take_cols()
            .into_iter()
            .map(|(k, v)| (format!("{}::{}", self.trace.namespace, k), v))
            .collect()
    }
}

/// Holds the submachine trace as a list of columns and a last row override
struct SubmachineTrace<F: FieldElement> {
    namespace: String,
    // non-selector columns
    cols: Vec<String>,
    // values for cols, stored as rows in a flat vec for memory locality
    values: Vec<F>,
    // selectors stored separately as they are dynamic (depend on the program)
    selectors: HashMap<String, Vec<F>>,
    // the trace is circular, so for the first block, we can only set the
    // previous row after the whole trace is built
    last_row_overrides: HashMap<usize, Option<F>>,
}

impl<F: FieldElement> SubmachineTrace<F> {
    fn new(namespace: &str, cols: Vec<String>, selectors: Vec<String>) -> Self {
        SubmachineTrace {
            namespace: namespace.to_string(),
            last_row_overrides: Default::default(),
            cols,
            values: vec![],
            selectors: selectors.iter().map(|n| (n.to_string(), vec![])).collect(),
        }
    }

    fn len(&self) -> u32 {
        self.values.len() as u32 / self.cols.len() as u32
    }

    /// set the value of a column in all rows of the current block
    fn set_current_block(&mut self, size: u32, col: usize, value: F) {
        for i in 0..size as usize {
            let row = self.len() as usize - i - 1;
            let idx = row * self.cols.len() + col;
            self.values[idx] = value;
        }
    }

    /// set the value of a column in the current row
    fn set_current_row(&mut self, col: usize, value: F) {
        let row = self.len() as usize - 1;
        let idx = row * self.cols.len() + col;
        self.values[idx] = value;
    }

    /// set the value of a selector in all rows of the current block
    fn set_current_block_selector(&mut self, size: u32, sel: &str, value: F) {
        for i in 0..size {
            let idx = self.len() - i - 1;
            *self
                .selectors
                .get_mut(sel)
                .unwrap_or_else(|| panic!("{} has no column {sel}", self.namespace))
                .get_mut(idx as usize)
                .unwrap() = value;
        }
    }

    /// set the value of a selector in the current row
    fn set_current_row_selector(&mut self, sel: &str, value: F) {
        *self
            .selectors
            .get_mut(sel)
            .unwrap_or_else(|| panic!("{} has no selector {sel}", self.namespace))
            .last_mut()
            .unwrap() = value;
    }

    /// set the value of a column in the last row of the complete trace
    fn set_final_row(&mut self, col: usize, value: F) {
        self.last_row_overrides.insert(col, Some(value));
    }

    /// apply saved updates to the last row of the trace
    fn final_row_override(&mut self) {
        for (col, value) in self.last_row_overrides.iter() {
            let row = self.len() as usize - 1;
            let idx = row * self.cols.len() + col;
            if let Some(value) = value {
                self.values[idx] = *value;
            }
        }
    }

    /// add new row of zeroes to the trce
    fn push_row(&mut self) {
        self.selectors.values_mut().for_each(|v| v.push(0.into()));
        self.values
            .extend(std::iter::repeat(F::from(0)).take(self.cols.len()));
    }

    /// Push a dummy block to the trace.
    /// A dummy block is a copy of the first block, with the final row updates applied to it, and selectors set to 0.
    fn push_dummy_block(&mut self, machine_max_degree: u32, size: u32) {
        for i in 0..size as usize {
            if self.len() == machine_max_degree {
                break;
            }
            self.push_row();
            let last_row = self.len() as usize - 1;
            let last_row_start_idx = last_row * self.cols.len();
            // split_at_mut the last row
            let (start, last_row) = self.values.split_at_mut(last_row_start_idx);
            // copy row i to the last row
            last_row.copy_from_slice(&start[self.cols.len() * i..self.cols.len() * (i + 1)]);
        }
        self.final_row_override();
    }

    /// consume the trace, returning the columns
    fn take_cols(&mut self) -> Vec<(String, Vec<F>)> {
        let selectors = std::mem::take(&mut self.selectors);
        let rows = self.len();
        let row_len = self.cols.len();
        let cols = std::mem::take(&mut self.cols);
        let values = std::mem::take(&mut self.values);
        cols.into_par_iter()
            .enumerate()
            .map(move |(idx, name)| {
                let mut col_values = Vec::with_capacity(rows as usize);
                for row in 0..rows as usize {
                    let val = values[row * row_len + idx];
                    col_values.push(val);
                }
                (name.clone(), col_values)
            })
            .chain(selectors)
            .collect()
    }
}

pub struct BinaryMachine;
witness_cols! {BinaryCols, A_byte0, A_byte1, A_byte2, A_byte3, B_byte0, B_byte1, B_byte2, B_byte3, C_byte0, C_byte1, C_byte2, C_byte3, A, B, C, operation_id}

impl SubmachineKind for BinaryMachine {
    const SELECTORS: &'static str = "sel";
    const BLOCK_SIZE: u32 = 1;

    fn cols() -> Vec<String> {
        BinaryCols::all()
            .iter()
            .map(|c| c.name().to_string())
            .collect()
    }

    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        selector: Option<&str>,
        lookup_args: &[F; 4],
        extra: &[F],
    ) {
        assert!(extra.is_empty());
        let selector = only_column_name(selector.unwrap());
        let [op_id, a, b, c] = lookup_args[..] else {
            panic!();
        };

        // decompose A
        let (a1, a2, a3, a4, _sign) =
            decompose_lower32(a.to_integer().try_into_u32().unwrap().into());
        // decompose B
        let (b1, b2, b3, b4, _sign) =
            decompose_lower32(b.to_integer().try_into_u32().unwrap().into());
        // decompose C
        let (c1, c2, c3, c4, _sign) =
            decompose_lower32(c.to_integer().try_into_u32().unwrap().into());

        // 1 row for each binary operation
        trace.push_row();
        trace.set_current_row(BinaryCols::A_byte0 as usize, (a1 as u32).into());
        trace.set_current_row(BinaryCols::A_byte1 as usize, (a2 as u32).into());
        trace.set_current_row(BinaryCols::A_byte2 as usize, (a3 as u32).into());
        trace.set_current_row(BinaryCols::A_byte3 as usize, (a4 as u32).into());
        trace.set_current_row(BinaryCols::B_byte0 as usize, (b1 as u32).into());
        trace.set_current_row(BinaryCols::B_byte1 as usize, (b2 as u32).into());
        trace.set_current_row(BinaryCols::B_byte2 as usize, (b3 as u32).into());
        trace.set_current_row(BinaryCols::B_byte3 as usize, (b4 as u32).into());
        trace.set_current_row(BinaryCols::C_byte0 as usize, (c1 as u32).into());
        trace.set_current_row(BinaryCols::C_byte1 as usize, (c2 as u32).into());
        trace.set_current_row(BinaryCols::C_byte2 as usize, (c3 as u32).into());
        trace.set_current_row(BinaryCols::C_byte3 as usize, (c4 as u32).into());
        trace.set_current_row(BinaryCols::A as usize, a);
        trace.set_current_row(BinaryCols::B as usize, b);
        trace.set_current_row(BinaryCols::C as usize, c);
        trace.set_current_row(BinaryCols::operation_id as usize, op_id);
        // latch row: set selector
        trace.set_current_row_selector(selector, 1.into());
    }
}

pub struct ShiftMachine;
witness_cols! {ShiftCols, A_byte, B_next, C_part, A, B, C, operation_id, operation_id_next}

impl SubmachineKind for ShiftMachine {
    const SELECTORS: &'static str = "sel";
    const BLOCK_SIZE: u32 = 4;

    fn cols() -> Vec<String> {
        ShiftCols::all()
            .iter()
            .map(|c| c.name().to_string())
            .collect()
    }

    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        selector: Option<&str>,
        lookup_args: &[F; 4],
        extra: &[F],
    ) {
        assert!(extra.is_empty());
        let selector = only_column_name(selector.unwrap());
        let [op_id, a, b, c] = lookup_args[..] else {
            panic!();
        };

        let mut shl = 0;
        let mut shr = 0;
        match op_id {
            id if id.is_zero() => {
                shl = b.to_integer().try_into_u32().unwrap();
            }
            id if id.is_one() => {
                shr = b.to_integer().try_into_u32().unwrap();
            }
            _ => unreachable!(),
        };

        // decompose A
        let (b1, b2, b3, b4, _sign) =
            decompose_lower32(a.to_integer().try_into_u32().unwrap().into());

        // set last row of the previous block
        if trace.len() > 0 {
            trace.set_current_row(ShiftCols::A_byte as usize, (b1 as u32).into());
            trace.set_current_row(
                ShiftCols::C_part as usize,
                (((b1 as u32) << shl) >> shr).into(),
            );
            trace.set_current_row(ShiftCols::operation_id_next as usize, op_id);
            trace.set_current_row(ShiftCols::B_next as usize, b);
        } else {
            trace.set_final_row(ShiftCols::A_byte as usize, (b1 as u32).into());
            trace.set_final_row(
                ShiftCols::C_part as usize,
                (((b1 as u32) << shl) >> shr).into(),
            );
            trace.set_final_row(ShiftCols::operation_id_next as usize, op_id);
            trace.set_final_row(ShiftCols::B_next as usize, b);
        }

        // 4 rows for each shift operation
        trace.push_row();
        trace.set_current_row(ShiftCols::operation_id as usize, op_id);
        trace.set_current_row(ShiftCols::operation_id_next as usize, op_id);
        trace.set_current_row(ShiftCols::A_byte as usize, (b2 as u32).into());
        let c_part_factor = (b2 as u32) << 8;
        let c_part = ((c_part_factor << shl) >> shr).into();
        trace.set_current_row(ShiftCols::C_part as usize, c_part);
        let a_row = a.to_integer().try_into_u32().unwrap() & 0xff;
        trace.set_current_row(ShiftCols::A as usize, a_row.into());
        trace.set_current_row(ShiftCols::B as usize, b);
        trace.set_current_row(ShiftCols::B_next as usize, b);
        trace.set_current_row(ShiftCols::C as usize, ((a_row << shl) >> shr).into());
        //
        trace.push_row();
        trace.set_current_row(ShiftCols::operation_id as usize, op_id);
        trace.set_current_row(ShiftCols::operation_id_next as usize, op_id);
        trace.set_current_row(ShiftCols::A_byte as usize, (b3 as u32).into());
        let c_part_factor = (b3 as u32) << 16;
        let c_part = ((c_part_factor << shl) >> shr).into();
        trace.set_current_row(ShiftCols::C_part as usize, c_part);
        let a_row = a.to_integer().try_into_u32().unwrap() & 0xffff;
        trace.set_current_row(ShiftCols::A as usize, a_row.into());
        trace.set_current_row(ShiftCols::B as usize, b);
        trace.set_current_row(ShiftCols::B_next as usize, b);
        trace.set_current_row(ShiftCols::C as usize, ((a_row << shl) >> shr).into());
        //
        trace.push_row();
        trace.set_current_row(ShiftCols::operation_id as usize, op_id);
        trace.set_current_row(ShiftCols::operation_id_next as usize, op_id);
        trace.set_current_row(ShiftCols::A_byte as usize, (b4 as u32).into());
        let c_part_factor = (b4 as u32) << 24;
        let c_part = ((c_part_factor << shl) >> shr).into();
        trace.set_current_row(ShiftCols::C_part as usize, c_part);
        let a_row = a.to_integer().try_into_u32().unwrap() & 0xffffff;
        trace.set_current_row(ShiftCols::A as usize, a_row.into());
        trace.set_current_row(ShiftCols::B as usize, b);
        trace.set_current_row(ShiftCols::B_next as usize, b);
        trace.set_current_row(ShiftCols::C as usize, ((a_row << shl) >> shr).into());
        // latch row
        trace.push_row();
        trace.set_current_row(ShiftCols::operation_id as usize, op_id);
        trace.set_current_row(ShiftCols::A as usize, a);
        trace.set_current_row(ShiftCols::B as usize, b);
        trace.set_current_row(ShiftCols::C as usize, c);
        trace.set_current_row_selector(selector, 1.into());
    }
}

pub struct SplitGlMachine;
witness_cols! {SplitGlCols, output_low, output_high, in_acc, bytes, lt, gt, was_lt}

impl SubmachineKind for SplitGlMachine {
    const SELECTORS: &'static str = "sel";
    const BLOCK_SIZE: u32 = 8;

    fn cols() -> Vec<String> {
        SplitGlCols::all()
            .iter()
            .map(|c| c.name().to_string())
            .collect()
    }

    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        selector: Option<&str>,
        lookup_args: &[F; 4],
        extra: &[F],
    ) {
        assert!(extra.is_empty());
        let selector = only_column_name(selector.unwrap());
        let [_input, output_lo, output_hi, _] = lookup_args[..] else {
            panic!();
        };

        let lo = output_lo.to_integer().try_into_u32().unwrap() as u64;
        let hi = output_hi.to_integer().try_into_u32().unwrap() as u64;

        fn hi_and_lo<F: FieldElement>(hi: u64, lo: u64) -> F {
            ((hi << 32) | lo).into()
        }

        let (b0, b1, b2, b3, _) = decompose_lower32(lo as i64);
        let (b4, b5, b6, b7, _) = decompose_lower32(hi as i64);

        const BYTES_MAX: [u32; 8] = [0, 0, 0, 0, 0xff, 0xff, 0xff, 0xff];
        let b = [
            b0 as u32, b1 as u32, b2 as u32, b3 as u32, b4 as u32, b5 as u32, b6 as u32, b7 as u32,
        ];
        let lt: Vec<_> = b
            .iter()
            .zip(BYTES_MAX.iter())
            .map(|(b, m)| if b < m { 1 } else { 0 })
            .collect();
        let gt: Vec<_> = b
            .iter()
            .zip(BYTES_MAX.iter())
            .map(|(b, m)| if b > m { 1 } else { 0 })
            .collect();
        let mut was_lt = [0u32; 8];
        was_lt[7] = lt[7];
        for i in (0..7).rev() {
            was_lt[i] = was_lt[i + 1] + lt[i] - was_lt[i + 1] * lt[i];
        }

        // set values in the last row of the previous block
        if trace.len() > 0 {
            trace.set_current_row(SplitGlCols::bytes as usize, b[0].into());
            trace.set_current_row(SplitGlCols::lt as usize, lt[0].into());
            trace.set_current_row(SplitGlCols::gt as usize, gt[0].into());
            trace.set_current_row(SplitGlCols::was_lt as usize, was_lt[0].into());
        } else {
            trace.set_final_row(SplitGlCols::bytes as usize, b[0].into());
            trace.set_final_row(SplitGlCols::lt as usize, lt[0].into());
            trace.set_final_row(SplitGlCols::gt as usize, gt[0].into());
            trace.set_final_row(SplitGlCols::was_lt as usize, was_lt[0].into());
        }

        // split_gl needs 8 rows:
        // 0
        trace.push_row();
        trace.set_current_row(SplitGlCols::output_low as usize, (lo & 0xff).into());
        trace.set_current_row(SplitGlCols::output_high as usize, 0.into());
        trace.set_current_row(SplitGlCols::in_acc as usize, (lo & 0xff).into());
        trace.set_current_row(SplitGlCols::bytes as usize, b[1].into());
        trace.set_current_row(SplitGlCols::lt as usize, lt[1].into());
        trace.set_current_row(SplitGlCols::gt as usize, gt[1].into());
        trace.set_current_row(SplitGlCols::was_lt as usize, was_lt[1].into());

        // 1
        trace.push_row();
        trace.set_current_row(SplitGlCols::output_low as usize, (lo & 0xffff).into());
        trace.set_current_row(SplitGlCols::output_high as usize, 0.into());
        trace.set_current_row(SplitGlCols::in_acc as usize, (lo & 0xffff).into());
        trace.set_current_row(SplitGlCols::bytes as usize, b[2].into());
        trace.set_current_row(SplitGlCols::lt as usize, lt[2].into());
        trace.set_current_row(SplitGlCols::gt as usize, gt[2].into());
        trace.set_current_row(SplitGlCols::was_lt as usize, was_lt[2].into());

        // 2
        trace.push_row();
        trace.set_current_row(SplitGlCols::output_low as usize, (lo & 0xffffff).into());
        trace.set_current_row(SplitGlCols::output_high as usize, 0.into());
        trace.set_current_row(SplitGlCols::in_acc as usize, (lo & 0xffffff).into());
        trace.set_current_row(SplitGlCols::bytes as usize, b[3].into());
        trace.set_current_row(SplitGlCols::lt as usize, lt[3].into());
        trace.set_current_row(SplitGlCols::gt as usize, gt[3].into());
        trace.set_current_row(SplitGlCols::was_lt as usize, was_lt[3].into());

        // 3
        trace.push_row();
        trace.set_current_row(SplitGlCols::output_low as usize, lo.into());
        trace.set_current_row(SplitGlCols::output_high as usize, 0.into());
        trace.set_current_row(SplitGlCols::in_acc as usize, lo.into());
        trace.set_current_row(SplitGlCols::bytes as usize, b[4].into());
        trace.set_current_row(SplitGlCols::lt as usize, lt[4].into());
        trace.set_current_row(SplitGlCols::gt as usize, gt[4].into());
        trace.set_current_row(SplitGlCols::was_lt as usize, was_lt[4].into());

        // 4
        trace.push_row();
        trace.set_current_row(SplitGlCols::output_low as usize, lo.into());
        trace.set_current_row(SplitGlCols::output_high as usize, (hi & 0xff).into());
        trace.set_current_row(SplitGlCols::in_acc as usize, hi_and_lo(hi & 0xff, lo));
        trace.set_current_row(SplitGlCols::bytes as usize, b[5].into());
        trace.set_current_row(SplitGlCols::lt as usize, lt[5].into());
        trace.set_current_row(SplitGlCols::gt as usize, gt[5].into());
        trace.set_current_row(SplitGlCols::was_lt as usize, was_lt[5].into());

        // 5
        trace.push_row();
        trace.set_current_row(SplitGlCols::output_low as usize, lo.into());
        trace.set_current_row(SplitGlCols::output_high as usize, (hi & 0xffff).into());
        trace.set_current_row(SplitGlCols::in_acc as usize, hi_and_lo(hi & 0xffff, lo));
        trace.set_current_row(SplitGlCols::bytes as usize, b[6].into());
        trace.set_current_row(SplitGlCols::lt as usize, lt[6].into());
        trace.set_current_row(SplitGlCols::gt as usize, gt[6].into());
        trace.set_current_row(SplitGlCols::was_lt as usize, was_lt[6].into());

        // 6
        trace.push_row();
        trace.set_current_row(SplitGlCols::output_low as usize, (lo).into());
        trace.set_current_row(SplitGlCols::output_high as usize, (hi & 0xffffff).into());
        trace.set_current_row(SplitGlCols::in_acc as usize, hi_and_lo(hi & 0xffffff, lo));
        trace.set_current_row(SplitGlCols::bytes as usize, b[7].into());
        trace.set_current_row(SplitGlCols::lt as usize, lt[7].into());
        trace.set_current_row(SplitGlCols::gt as usize, gt[7].into());
        trace.set_current_row(SplitGlCols::was_lt as usize, was_lt[7].into());

        // 7: bytes/lt/gt/was_lt are set by the next row
        trace.push_row();
        trace.set_current_row_selector(selector, 1.into());
        trace.set_current_row(SplitGlCols::output_low as usize, lo.into());
        trace.set_current_row(SplitGlCols::output_high as usize, hi.into());
        trace.set_current_row(SplitGlCols::in_acc as usize, hi_and_lo(hi, lo));
    }
}

pub struct PublicsMachine;
witness_cols! {PublicsCols, value}

impl SubmachineKind for PublicsMachine {
    const SELECTORS: &'static str = "";
    const BLOCK_SIZE: u32 = 1;

    fn cols() -> Vec<String> {
        PublicsCols::all()
            .iter()
            .map(|c| c.name().to_string())
            .collect()
    }

    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        selector: Option<&str>,
        lookup_args: &[F; 4],
        extra: &[F],
    ) {
        assert!(selector.is_none());
        assert!(extra.is_empty());
        let [addr, value, _, _] = lookup_args[..] else {
            panic!();
        };
        assert!(
            addr.to_integer().try_into_u32().unwrap() < 8,
            "publics machine only supports 8 public values"
        );
        while addr.to_integer().try_into_u32().unwrap() >= trace.len() {
            trace.push_row();
        }
        let row = addr.to_integer().try_into_u32().unwrap() as usize;
        let idx = row * trace.cols.len();
        trace.values[idx] = value;
    }
}

pub struct PoseidonGlMachine;
witness_cols_str! {
    PoseidonGlCols,
    time_step = "time_step",
    do_mload = "do_mload",
    word_low = "word_low",
    word_high = "word_high",
    input_addr = "input_addr",
    output_addr = "output_addr",
    // input
    input_0 = "input[0]",
    input_1 = "input[1]",
    input_2 = "input[2]",
    input_3 = "input[3]",
    input_4 = "input[4]",
    input_5 = "input[5]",
    input_6 = "input[6]",
    input_7 = "input[7]",
    input_8 = "input[8]",
    input_9 = "input[9]",
    input_10 = "input[10]",
    input_11 = "input[11]",
    // output
    output_0 = "output[0]",
    output_1 = "output[1]",
    output_2 = "output[2]",
    output_3 = "output[3]",
    // state
    state_0 = "state[0]",
    state_1 = "state[1]",
    state_2 = "state[2]",
    state_3 = "state[3]",
    state_4 = "state[4]",
    state_5 = "state[5]",
    state_6 = "state[6]",
    state_7 = "state[7]",
    state_8 = "state[8]",
    state_9 = "state[9]",
    state_10 = "state[10]",
    state_11 = "state[11]",
    // x3
    x3_0 = "x3[0]",
    x3_1 = "x3[1]",
    x3_2 = "x3[2]",
    x3_3 = "x3[3]",
    x3_4 = "x3[4]",
    x3_5 = "x3[5]",
    x3_6 = "x3[6]",
    x3_7 = "x3[7]",
    x3_8 = "x3[8]",
    x3_9 = "x3[9]",
    x3_10 = "x3[10]",
    x3_11 = "x3[11]",
    // x7
    x7_0 = "x7[0]",
    x7_1 = "x7[1]",
    x7_2 = "x7[2]",
    x7_3 = "x7[3]",
    x7_4 = "x7[4]",
    x7_5 = "x7[5]",
    x7_6 = "x7[6]",
    x7_7 = "x7[7]",
    x7_8 = "x7[8]",
    x7_9 = "x7[9]",
    x7_10 = "x7[10]",
    x7_11 = "x7[11]"
}

impl SubmachineKind for PoseidonGlMachine {
    const SELECTORS: &'static str = "sel";
    const BLOCK_SIZE: u32 = 31; // full rounds + partial rounds + 1

    fn cols() -> Vec<String> {
        PoseidonGlCols::all()
            .iter()
            .map(|c| c.name().to_string())
            .collect()
    }

    #[allow(clippy::needless_range_loop)]
    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        selector: Option<&str>,
        lookup_args: &[F; 4],
        extra: &[F],
    ) {
        const STATE_SIZE: usize = 12;
        const OUTPUT_SIZE: usize = 4;

        let selector = only_column_name(selector.unwrap());
        let [input_addr, output_addr, time_step, _] = lookup_args[..] else {
            panic!();
        };

        let input = extra[0..STATE_SIZE].to_vec();
        let output = extra[STATE_SIZE..STATE_SIZE + OUTPUT_SIZE].to_vec();

        let mut state: Vec<F> = input.clone();

        for row in 0..(Self::BLOCK_SIZE - 1) as usize {
            trace.push_row();
            for i in 0..STATE_SIZE {
                trace.set_current_row(PoseidonGlCols::state_0 as usize + i, state[i]);
            }
            // memory read/write columns
            if row < STATE_SIZE {
                let v = input[row].to_integer().try_into_u64().unwrap();
                let hi = (v >> 32) as u32;
                let lo = (v & 0xffffffff) as u32;
                trace.set_current_row(PoseidonGlCols::do_mload as usize, 1.into());
                trace.set_current_row(PoseidonGlCols::word_low as usize, lo.into());
                trace.set_current_row(PoseidonGlCols::word_high as usize, hi.into());
            } else if row < STATE_SIZE + OUTPUT_SIZE {
                let v = output[row - STATE_SIZE]
                    .to_integer()
                    .try_into_u64()
                    .unwrap();
                let hi = (v >> 32) as u32;
                let lo = (v & 0xffffffff) as u32;
                trace.set_current_row(PoseidonGlCols::do_mload as usize, 0.into());
                trace.set_current_row(PoseidonGlCols::word_low as usize, lo.into());
                trace.set_current_row(PoseidonGlCols::word_high as usize, hi.into());
            } else {
                trace.set_current_row(PoseidonGlCols::do_mload as usize, 0.into());
                trace.set_current_row(PoseidonGlCols::word_low as usize, 0.into());
                trace.set_current_row(PoseidonGlCols::word_high as usize, 0.into());
            }
            // update state for the next row
            // S-Boxes
            let is_full = !(4..26).contains(&row);
            for i in 0..STATE_SIZE {
                let a = state[i] + F::from(poseidon_gl::ROUND_CONSTANTS[i][row]);
                let x3 = a.pow(3.into());
                let x7 = x3.pow(2.into()) * a;
                trace.set_current_row(PoseidonGlCols::x3_0 as usize + i, x3);
                trace.set_current_row(PoseidonGlCols::x7_0 as usize + i, x7);
                if i == 0 || is_full {
                    state[i] = x7;
                } else {
                    state[i] = a;
                }
            }
            // MDS multiplication
            let mds_input = state.clone();
            for (i, v) in state.iter_mut().enumerate() {
                let mut tmp = F::zero();
                for (j, &input) in mds_input.iter().enumerate() {
                    tmp += input * F::from(poseidon_gl::MDS_MATRIX[i][j]);
                }
                *v = tmp;
            }
        }

        trace.push_row();
        for i in 0..STATE_SIZE {
            // x3 and x7 are still constrained in output row...
            let a = state[i];
            let x3 = a.pow(3.into());
            let x7 = x3.pow(2.into()) * a;
            trace.set_current_row(PoseidonGlCols::x3_0 as usize + i, x3);
            trace.set_current_row(PoseidonGlCols::x7_0 as usize + i, x7);
            // set output
            trace.set_current_row(PoseidonGlCols::state_0 as usize + i, state[i]);
        }
        // these are the same in the whole block
        trace.set_current_block(
            Self::BLOCK_SIZE,
            PoseidonGlCols::time_step as usize,
            time_step,
        );
        trace.set_current_block(
            Self::BLOCK_SIZE,
            PoseidonGlCols::input_addr as usize,
            input_addr,
        );
        trace.set_current_block(
            Self::BLOCK_SIZE,
            PoseidonGlCols::output_addr as usize,
            output_addr,
        );
        // set selector
        trace.set_current_block_selector(Self::BLOCK_SIZE, selector, 1.into());
        for i in 0..STATE_SIZE {
            trace.set_current_block(
                Self::BLOCK_SIZE,
                PoseidonGlCols::input_0 as usize + i,
                input[i],
            );
        }
        for i in 0..OUTPUT_SIZE {
            trace.set_current_block(
                Self::BLOCK_SIZE,
                PoseidonGlCols::output_0 as usize + i,
                state[i],
            );
        }
    }

    fn dummy_block_fix<F: FieldElement>(trace: &mut SubmachineTrace<F>, rows: u32) {
        trace.set_current_block(rows, PoseidonGlCols::do_mload as usize, 0.into());
    }
}
