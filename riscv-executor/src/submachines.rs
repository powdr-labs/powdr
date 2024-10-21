use super::decompose_lower32;
use super::Elem;
use powdr_number::{FieldElement, LargeInt};

use std::collections::HashMap;

/// Each submachine kind (i.e., binary, shift) must implement this trait
trait SubmachineKind {
    /// Witness column names, without the machine name prefix
    const COLS: &'static [&'static str];
    /// Which of the witness columns are selectors, if any
    const SELECTORS: &'static [&'static str];
    /// Row block size
    const BLOCK_SIZE: u32;
    /// Add an operation to the submachine trace
    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        name: &str,
        args: &[(&str, Elem<F>)],
    );
}

/// Trait only used for the constructor
pub trait SubmachineBoxed<F: FieldElement> {
    fn new_boxed(name: &str) -> Box<dyn Submachine<F>>;
}

impl<F: FieldElement, M: SubmachineKind + 'static> SubmachineBoxed<F> for M {
    fn new_boxed(name: &str) -> Box<dyn Submachine<F>> {
        Box::new(SubmachineImpl::<F, M>::new(name))
    }
}

/// Submachine interface, implemented by SubmachineImpl.
/// Each specific submachine only need to implement the SubmachineKind trait.
/// Trace is built by calling these methods.
/// It being a trait also allows us to put different submachines in the same hashmap.
pub trait Submachine<F: FieldElement> {
    /// current number of rows
    fn len(&self) -> u32;
    /// add a new operation to the trace
    fn add_operation(&mut self, name: &str, args: &[(&str, Elem<F>)]);
    /// apply final row overrides (needed because the trace is circular)
    fn final_row_override(&mut self);
    /// push a dummy block to the trace
    fn push_dummy_block(&mut self);
    /// Consume the trace returning a list of columns.
    /// Should be called only once.
    fn take_cols(&mut self) -> Vec<(String, Vec<Elem<F>>)>;
}

/// Concrete implementation of the Submachine trait
struct SubmachineImpl<F: FieldElement, M: SubmachineKind> {
    name: String,
    trace: SubmachineTrace<F>,
    m: std::marker::PhantomData<M>,
}

impl<F: FieldElement, M: SubmachineKind> SubmachineImpl<F, M> {
    pub fn new(name: &str) -> Self {
        SubmachineImpl {
            name: name.to_string(),
            trace: SubmachineTrace::new(M::COLS),
            m: std::marker::PhantomData,
        }
    }
}

impl<F: FieldElement, M: SubmachineKind> Submachine<F> for SubmachineImpl<F, M> {
    fn len(&self) -> u32 {
        self.trace.len()
    }
    fn add_operation(&mut self, name: &str, args: &[(&str, Elem<F>)]) {
        M::add_operation(&mut self.trace, name, args);
    }

    fn final_row_override(&mut self) {
        self.trace.final_row_override()
    }

    fn push_dummy_block(&mut self) {
        self.trace.push_dummy_block(M::BLOCK_SIZE, M::SELECTORS);
    }

    fn take_cols(&mut self) -> Vec<(String, Vec<Elem<F>>)> {
        self.trace
            .take_cols()
            .map(|(k, v)| (format!("{}::{}", self.name, k), v))
            .collect()
    }
}

/// Holds the submachine trace as a list of columns and a last row override
struct SubmachineTrace<F: FieldElement> {
    cols: HashMap<String, Vec<Elem<F>>>,
    // the trace is circular, so for the first block, we can only set the
    // previous row after the whole trace is built
    last_row_overrides: HashMap<String, Option<Elem<F>>>,
}

impl<F: FieldElement> SubmachineTrace<F> {
    fn new(cols: &[&'static str]) -> Self {
        SubmachineTrace {
            cols: cols.iter().map(|n| (n.to_string(), vec![])).collect(),
            last_row_overrides: cols.iter().map(|n| (n.to_string(), None)).collect(),
        }
    }

    fn len(&self) -> u32 {
        self.cols
            .values()
            .next()
            .map(|v| v.len().try_into().unwrap())
            .unwrap_or(0)
    }

    /// set the value of a column in the current
    fn set_row(&mut self, col: &str, value: Elem<F>) {
        *self.cols.get_mut(col).unwrap().last_mut().unwrap() = value;
    }

    /// get the value of a column in the given row
    fn get_row_idx(&self, col: &str, idx: i64) -> Elem<F> {
        let idx: usize = if idx < 0 {
            (self.len() as i64 + idx) as usize
        } else {
            idx as usize
        };
        self.cols.get(col).unwrap()[idx]
    }

    /// set the value of a column in the last row of the complete trace
    fn set_final_row(&mut self, col: &str, value: Elem<F>) {
        *self.last_row_overrides.get_mut(col).unwrap() = Some(value);
    }

    /// apply saved updates to the last row of the trace
    fn final_row_override(&mut self) {
        for (col, value) in self.last_row_overrides.iter() {
            if let Some(value) = value {
                *self.cols.get_mut(col).unwrap().last_mut().unwrap() = *value;
            }
        }
    }

    /// add new row of zeroes to the trce
    fn push_row(&mut self) {
        self.cols
            .values_mut()
            .for_each(|v| v.push(Elem::Field(0.into())));
    }

    /// add a copy of the previous row to the trace
    fn push_row_copy(&mut self, idx: u32) {
        self.cols.values_mut().for_each(|v| v.push(v[idx as usize]));
    }

    /// Push a dummy block to the trace.
    /// A dummy block is a copy of the first block, with the final row updates applied to it.
    fn push_dummy_block(&mut self, size: u32, selectors: &'static [&'static str]) {
        for i in 0..size {
            self.push_row_copy(i);
            for sel in selectors {
                self.set_row(sel, 0.into());
            }
        }
        self.final_row_override();
    }

    /// consume the trace, returning the columns
    fn take_cols(&mut self) -> impl Iterator<Item = (String, Vec<Elem<F>>)> {
        std::mem::take(&mut self.cols).into_iter()
    }

    /// helper for debugging purposes only
    #[allow(dead_code)]
    fn print_row_idx(&self, idx: i64) {
        let idx: usize = if idx < 0 {
            (self.len() as i64 + idx) as usize
        } else {
            idx as usize
        };

        print!("Row {idx} - ");
        for (col, values) in &self.cols {
            let val = format!(
                "{col}: {:x}",
                values[idx].into_fe().to_integer().try_into_u64().unwrap()
            );
            print!("{val:>15}, ");
        }
        println!();
    }
}

pub struct BinaryMachine;

impl SubmachineKind for BinaryMachine {
    const COLS: &'static [&'static str] = &[
        "operation_id",
        "sel[0]",
        "sel[1]",
        "sel[2]",
        "A_byte",
        "B_byte",
        "C_byte",
        "A",
        "B",
        "C",
    ];
    const SELECTORS: &'static [&'static str] = &["sel[0]", "sel[1]", "sel[2]"];
    const BLOCK_SIZE: u32 = 4;

    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        op: &str,
        args: &[(&str, Elem<F>)],
    ) {
        let args: HashMap<_, _> = args.iter().cloned().collect();
        let a = args["A"];
        let b = args["B"];
        let c = args["C"];

        let sel;
        let op_id: Elem<F>;
        match op {
            "and" => {
                //
                op_id = 0.into();
                sel = "sel[0]";
            }
            "or" => {
                //
                op_id = 1.into();
                sel = "sel[1]";
            }
            "xor" => {
                //
                op_id = 2.into();
                sel = "sel[2]";
            }
            _ => unreachable!(),
        };

        // decompose A
        let (a1, a2, a3, a4, _sign) = decompose_lower32(a.u().into());
        // decompose B
        let (b1, b2, b3, b4, _sign) = decompose_lower32(b.u().into());
        // decompose C
        let (c1, c2, c3, c4, _sign) = decompose_lower32(c.u().into());

        // set last row of the previous block
        if trace.len() > 0 {
            trace.set_row("A_byte", (a1 as u32).into());
            trace.set_row("B_byte", (b1 as u32).into());
            trace.set_row("C_byte", (c1 as u32).into());
        } else {
            trace.set_final_row("A_byte", (a1 as u32).into());
            trace.set_final_row("B_byte", (b1 as u32).into());
            trace.set_final_row("C_byte", (c1 as u32).into());
        }

        // 4 rows for each binary operation
        trace.push_row();
        trace.set_row("operation_id", op_id);
        trace.set_row("A_byte", (a2 as u32).into());
        trace.set_row("B_byte", (b2 as u32).into());
        trace.set_row("C_byte", (c2 as u32).into());
        trace.set_row("A", (a.u() & 0xff).into());
        trace.set_row("B", (b.u() & 0xff).into());
        trace.set_row("C", (c.u() & 0xff).into());

        trace.push_row();
        trace.set_row("operation_id", op_id);
        trace.set_row("A_byte", (a3 as u32).into());
        trace.set_row("B_byte", (b3 as u32).into());
        trace.set_row("C_byte", (c3 as u32).into());
        trace.set_row("A", (a.u() & 0xffff).into());
        trace.set_row("B", (b.u() & 0xffff).into());
        trace.set_row("C", (c.u() & 0xffff).into());

        trace.push_row();
        trace.set_row("operation_id", op_id);
        trace.set_row("A_byte", (a4 as u32).into());
        trace.set_row("B_byte", (b4 as u32).into());
        trace.set_row("C_byte", (c4 as u32).into());
        trace.set_row("A", (a.u() & 0xffffff).into());
        trace.set_row("B", (b.u() & 0xffffff).into());
        trace.set_row("C", (c.u() & 0xffffff).into());

        trace.push_row();
        trace.set_row("operation_id", op_id);
        trace.set_row("A", a);
        trace.set_row("B", b);
        trace.set_row("C", c);
        // latch row: set selector
        trace.set_row(sel, 1.into());
    }
}

pub struct ShiftMachine;

impl SubmachineKind for ShiftMachine {
    const COLS: &'static [&'static str] = &[
        "operation_id",
        "sel[0]",
        "sel[1]",
        "A_byte",
        "C_part",
        "A",
        "B",
        "C",
    ];
    const SELECTORS: &'static [&'static str] = &["sel[0]", "sel[1]"];
    const BLOCK_SIZE: u32 = 4;

    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        op: &str,
        args: &[(&str, Elem<F>)],
    ) {
        let args: HashMap<_, _> = args.iter().cloned().collect();
        let a = args["A"];
        let b = args["B"];
        let c = args["C"];

        let sel;
        let op_id: Elem<F>;
        let mut shl = 0;
        let mut shr = 0;
        match op {
            "shl" => {
                op_id = 0.into();
                sel = "sel[0]";
                shl = b.u();
            }
            "shr" => {
                op_id = 1.into();
                sel = "sel[1]";
                shr = b.u();
            }
            _ => unreachable!(),
        };

        // decompose A
        let (b1, b2, b3, b4, _sign) = decompose_lower32(a.u().into());

        // set last row of the previous block
        if trace.len() > 0 {
            trace.set_row("A_byte", (b1 as u32).into());
            trace.set_row("C_part", (((b1 as u32) << shl) >> shr).into());
        } else {
            trace.set_final_row("A_byte", (b1 as u32).into());
            trace.set_final_row("C_part", (((b1 as u32) << shl) >> shr).into());
        }

        // 4 rows for each shift operation
        trace.push_row();
        trace.set_row("operation_id", op_id);
        trace.set_row("A_byte", (b2 as u32).into());
        let c_part_factor = (b2 as u32) << 8;
        let c_part = ((c_part_factor << shl) >> shr).into();
        trace.set_row("C_part", c_part);
        let a_row = a.u() & 0xff;
        trace.set_row("A", a_row.into());
        trace.set_row("B", b);
        trace.set_row("C", ((a_row << shl) >> shr).into());
        //
        trace.push_row();
        trace.set_row("operation_id", op_id);
        trace.set_row("A_byte", (b3 as u32).into());
        let c_part_factor = (b3 as u32) << 16;
        let c_part = ((c_part_factor << shl) >> shr).into();
        trace.set_row("C_part", c_part);
        let a_row = a.u() & 0xffff;
        trace.set_row("A", a_row.into());
        trace.set_row("B", b);
        trace.set_row("C", ((a_row << shl) >> shr).into());
        //
        trace.push_row();
        trace.set_row("operation_id", op_id);
        trace.set_row("A_byte", (b4 as u32).into());
        let c_part_factor = (b4 as u32) << 24;
        let c_part = ((c_part_factor << shl) >> shr).into();
        trace.set_row("C_part", c_part);
        let a_row = a.u() & 0xffffff;
        trace.set_row("A", a_row.into());
        trace.set_row("B", b);
        trace.set_row("C", ((a_row << shl) >> shr).into());
        // latch row
        trace.push_row();
        trace.set_row("operation_id", op_id);
        trace.set_row("A", a);
        trace.set_row("B", b);
        trace.set_row("C", c);
        trace.set_row(sel, 1.into());
    }
}

pub struct SplitGlMachine;

impl SubmachineKind for SplitGlMachine {
    const COLS: &'static [&'static str] = &[
        "sel[0]",
        "bytes",
        "in_acc",
        "output_low",
        "output_high",
        "lt",
        "gt",
        "was_lt",
    ];
    const SELECTORS: &'static [&'static str] = &["sel[0]"];
    const BLOCK_SIZE: u32 = 8;

    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        _op: &str,
        args: &[(&str, Elem<F>)],
    ) {
        let args: HashMap<_, _> = args.iter().cloned().collect();
        let lo = args["output_low"].u() as u64;
        let hi = args["output_high"].u() as u64;

        fn hi_and_lo<F: FieldElement>(hi: u64, lo: u64) -> Elem<F> {
            Elem::from_u64_as_fe((hi << 32) | lo)
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
            trace.set_row("bytes", b[0].into());
            trace.set_row("lt", lt[0].into());
            trace.set_row("gt", gt[0].into());
            trace.set_row("was_lt", was_lt[0].into());
        } else {
            trace.set_final_row("bytes", b[0].into());
            trace.set_final_row("lt", lt[0].into());
            trace.set_final_row("gt", gt[0].into());
            trace.set_final_row("was_lt", was_lt[0].into());
        }

        // split_gl needs 8 rows:
        // 0
        trace.push_row();
        trace.set_row("output_low", Elem::from_u64_as_fe(lo & 0xff));
        trace.set_row("output_high", 0.into());
        trace.set_row("in_acc", Elem::from_u64_as_fe(lo & 0xff));
        trace.set_row("bytes", b[1].into());
        trace.set_row("lt", lt[1].into());
        trace.set_row("gt", gt[1].into());
        trace.set_row("was_lt", was_lt[1].into());

        // 1
        trace.push_row();
        trace.set_row("output_low", Elem::from_u64_as_fe(lo & 0xffff));
        trace.set_row("output_high", 0.into());
        trace.set_row("in_acc", Elem::from_u64_as_fe(lo & 0xffff));
        trace.set_row("bytes", b[2].into());
        trace.set_row("lt", lt[2].into());
        trace.set_row("gt", gt[2].into());
        trace.set_row("was_lt", was_lt[2].into());

        // 2
        trace.push_row();
        trace.set_row("output_low", Elem::from_u64_as_fe(lo & 0xffffff));
        trace.set_row("output_high", 0.into());
        trace.set_row("in_acc", Elem::from_u64_as_fe(lo & 0xffffff));
        trace.set_row("bytes", b[3].into());
        trace.set_row("lt", lt[3].into());
        trace.set_row("gt", gt[3].into());
        trace.set_row("was_lt", was_lt[3].into());

        // 3
        trace.push_row();
        trace.set_row("output_low", Elem::from_u64_as_fe(lo));
        trace.set_row("output_high", 0.into());
        trace.set_row("in_acc", Elem::from_u64_as_fe(lo));
        trace.set_row("bytes", b[4].into());
        trace.set_row("lt", lt[4].into());
        trace.set_row("gt", gt[4].into());
        trace.set_row("was_lt", was_lt[4].into());

        // 4
        trace.push_row();
        trace.set_row("output_low", Elem::from_u64_as_fe(lo));
        trace.set_row("output_high", Elem::from_u64_as_fe(hi & 0xff));
        trace.set_row("in_acc", hi_and_lo(hi & 0xff, lo));
        trace.set_row("bytes", b[5].into());
        trace.set_row("lt", lt[5].into());
        trace.set_row("gt", gt[5].into());
        trace.set_row("was_lt", was_lt[5].into());

        // 5
        trace.push_row();
        trace.set_row("output_low", Elem::from_u64_as_fe(lo));
        trace.set_row("output_high", Elem::from_u64_as_fe(hi & 0xffff));
        trace.set_row("in_acc", hi_and_lo(hi & 0xffff, lo));
        trace.set_row("bytes", b[6].into());
        trace.set_row("lt", lt[6].into());
        trace.set_row("gt", gt[6].into());
        trace.set_row("was_lt", was_lt[6].into());

        // 6
        trace.push_row();
        trace.set_row("output_low", Elem::from_u64_as_fe(lo));
        trace.set_row("output_high", Elem::from_u64_as_fe(hi & 0xffffff));
        trace.set_row("in_acc", hi_and_lo(hi & 0xffffff, lo));
        trace.set_row("bytes", b[7].into());
        trace.set_row("lt", lt[7].into());
        trace.set_row("gt", gt[7].into());
        trace.set_row("was_lt", was_lt[7].into());

        // 7: bytes/lt/gt/was_lt are set by the next row
        trace.push_row();
        trace.set_row("sel[0]", 1.into());
        trace.set_row("output_low", Elem::from_u64_as_fe(lo));
        trace.set_row("output_high", Elem::from_u64_as_fe(hi));
        trace.set_row("in_acc", hi_and_lo(hi, lo));
    }
}
