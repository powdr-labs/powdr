use super::decompose_lower32;
use super::poseidon_gl;
use super::Elem;
use powdr_number::{FieldElement, LargeInt};

use std::collections::HashMap;

/// Each submachine kind (i.e., binary, shift) must implement this trait
trait SubmachineKind {
    /// Which of the witness columns are selectors, if any
    const SELECTORS: &'static str;
    /// Row block size
    const BLOCK_SIZE: u32;
    /// Add an operation to the submachine trace
    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        name: &str,
        args: &[Elem<F>],
        selector: Option<u32>,
        submachines: &[&mut dyn Submachine<F>],
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
pub trait Submachine<F: FieldElement> {
    /// submachine namespace
    fn namespace(&self) -> &str;
    /// current number of rows
    fn len(&self) -> u32;
    /// add a new operation to the trace
    fn add_operation(
        &mut self,
        name: &str,
        args: &[Elem<F>],
        selector: Option<u32>,
        submachines: &[&mut dyn Submachine<F>],
    );
    /// apply final row overrides (needed because the trace is circular)
    fn final_row_override(&mut self);
    /// push a dummy block to the trace
    fn push_dummy_block(&mut self, machine_max_degree: usize);
    /// Consume the trace returning a list of columns.
    /// Should be called only once.
    fn take_cols(&mut self) -> Vec<(String, Vec<Elem<F>>)>;
}

/// Concrete implementation of the Submachine trait
struct SubmachineImpl<F: FieldElement, M: SubmachineKind> {
    namespace: String,
    trace: SubmachineTrace<F>,
    m: std::marker::PhantomData<M>,
}

impl<F: FieldElement, M: SubmachineKind> SubmachineImpl<F, M> {
    pub fn new(namespace: &str, witness_cols: &[String]) -> Self {
        // filter machine columns
        let prefix = format!("{namespace}::");
        let witness_cols = witness_cols
            .iter()
            .filter(|c| c.starts_with(namespace))
            .map(|c| (c.strip_prefix(&prefix).unwrap().to_string(), vec![]))
            .collect();
        SubmachineImpl {
            namespace: namespace.to_string(),
            trace: SubmachineTrace::new(witness_cols),
            m: std::marker::PhantomData,
        }
    }
}

impl<F: FieldElement, M: SubmachineKind> Submachine<F> for SubmachineImpl<F, M> {
    fn namespace(&self) -> &str {
        self.namespace.as_str()
    }

    fn len(&self) -> u32 {
        self.trace.len()
    }

    fn add_operation(
        &mut self,
        name: &str,
        args: &[Elem<F>],
        selector: Option<u32>,
        submachines: &[&mut dyn Submachine<F>],
    ) {
        M::add_operation(&mut self.trace, name, args, selector, submachines);
    }

    fn final_row_override(&mut self) {
        self.trace.final_row_override()
    }

    fn push_dummy_block(&mut self, machine_max_degree: usize) {
        let prev_len = self.len();
        self.trace
            .push_dummy_block(machine_max_degree, M::BLOCK_SIZE, M::SELECTORS);
        let dummy_size = self.len() - prev_len;
        M::dummy_block_fix(&mut self.trace, dummy_size);
    }

    fn take_cols(&mut self) -> Vec<(String, Vec<Elem<F>>)> {
        self.trace
            .take_cols()
            .map(|(k, v)| (format!("{}::{}", self.namespace, k), v))
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
    fn new(cols: HashMap<String, Vec<Elem<F>>>) -> Self {
        assert!(!cols.is_empty(), "machine with no witness columns");
        SubmachineTrace {
            last_row_overrides: cols.keys().map(|n| (n.clone(), None)).collect(),
            cols,
        }
    }

    fn len(&self) -> u32 {
        self.cols.values().next().unwrap().len().try_into().unwrap()
    }

    /// set the value of a column in all rows of the current block
    fn set_current_block(&mut self, size: u32, col: &str, value: Elem<F>) {
        for i in 0..size {
            let idx = self.len() - i - 1;
            *self
                .cols
                .get_mut(col)
                .unwrap()
                .get_mut(idx as usize)
                .unwrap() = value;
        }
    }

    /// set the value of a column in the current
    fn set_current_row(&mut self, col: &str, value: Elem<F>) {
        *self.cols.get_mut(col).unwrap().last_mut().unwrap() = value;
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

    // /// add a copy of the given row to the trace
    // fn push_row_copy(&mut self, idx: u32) {
    //     self.cols.values_mut().for_each(|v| v.push(v[idx as usize]));
    // }

    /// Push a dummy block to the trace.
    /// A dummy block is a copy of the first block, with the final row updates applied to it, and selectors set to 0.
    fn push_dummy_block(&mut self, machine_max_degree: usize, size: u32, selectors: &'static str) {
        let selector_pat = format!("{selectors}[");

        for i in 0..size {
            if self.cols.values().next().unwrap().len() == machine_max_degree {
                break;
            }
            self.cols.iter_mut().for_each(|(col, values)| {
                if !col.starts_with(&selector_pat) {
                    values.push(values[i as usize])
                } else {
                    values.push(0.into())
                }
            });
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
    const SELECTORS: &'static str = "sel";
    const BLOCK_SIZE: u32 = 4;

    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        op: &str,
        args: &[Elem<F>],
        selector: Option<u32>,
        _submachines: &[&mut dyn Submachine<F>],
    ) {
        let [a, b, c] = args[..] else {
            panic!();
        };

        let op_id: Elem<F> = match op {
            "and" => 0,
            "or" => 1,
            "xor" => 2,
            _ => unreachable!(),
        }
        .into();

        // decompose A
        let (a1, a2, a3, a4, _sign) = decompose_lower32(a.u().into());
        // decompose B
        let (b1, b2, b3, b4, _sign) = decompose_lower32(b.u().into());
        // decompose C
        let (c1, c2, c3, c4, _sign) = decompose_lower32(c.u().into());

        // set last row of the previous block
        if trace.len() > 0 {
            trace.set_current_row("A_byte", (a1 as u32).into());
            trace.set_current_row("B_byte", (b1 as u32).into());
            trace.set_current_row("C_byte", (c1 as u32).into());
        } else {
            trace.set_final_row("A_byte", (a1 as u32).into());
            trace.set_final_row("B_byte", (b1 as u32).into());
            trace.set_final_row("C_byte", (c1 as u32).into());
        }

        // 4 rows for each binary operation
        trace.push_row();
        trace.set_current_row("operation_id", op_id);
        trace.set_current_row("A_byte", (a2 as u32).into());
        trace.set_current_row("B_byte", (b2 as u32).into());
        trace.set_current_row("C_byte", (c2 as u32).into());
        trace.set_current_row("A", (a.u() & 0xff).into());
        trace.set_current_row("B", (b.u() & 0xff).into());
        trace.set_current_row("C", (c.u() & 0xff).into());

        trace.push_row();
        trace.set_current_row("operation_id", op_id);
        trace.set_current_row("A_byte", (a3 as u32).into());
        trace.set_current_row("B_byte", (b3 as u32).into());
        trace.set_current_row("C_byte", (c3 as u32).into());
        trace.set_current_row("A", (a.u() & 0xffff).into());
        trace.set_current_row("B", (b.u() & 0xffff).into());
        trace.set_current_row("C", (c.u() & 0xffff).into());

        trace.push_row();
        trace.set_current_row("operation_id", op_id);
        trace.set_current_row("A_byte", (a4 as u32).into());
        trace.set_current_row("B_byte", (b4 as u32).into());
        trace.set_current_row("C_byte", (c4 as u32).into());
        trace.set_current_row("A", (a.u() & 0xffffff).into());
        trace.set_current_row("B", (b.u() & 0xffffff).into());
        trace.set_current_row("C", (c.u() & 0xffffff).into());

        trace.push_row();
        trace.set_current_row("operation_id", op_id);
        trace.set_current_row("A", a);
        trace.set_current_row("B", b);
        trace.set_current_row("C", c);
        // latch row: set selector
        trace.set_current_row(
            &format!("{}[{}]", Self::SELECTORS, selector.unwrap()),
            1.into(),
        );
    }
}

pub struct ShiftMachine;

impl SubmachineKind for ShiftMachine {
    const SELECTORS: &'static str = "sel";
    const BLOCK_SIZE: u32 = 4;

    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        op: &str,
        args: &[Elem<F>],
        selector: Option<u32>,
        _submachines: &[&mut dyn Submachine<F>],
    ) {
        let [a, b, c] = args[..] else {
            panic!();
        };

        let op_id: Elem<F>;
        let mut shl = 0;
        let mut shr = 0;
        match op {
            "shl" => {
                op_id = 0.into();
                shl = b.u();
            }
            "shr" => {
                op_id = 1.into();
                shr = b.u();
            }
            _ => unreachable!(),
        };

        // decompose A
        let (b1, b2, b3, b4, _sign) = decompose_lower32(a.u().into());

        // set last row of the previous block
        if trace.len() > 0 {
            trace.set_current_row("A_byte", (b1 as u32).into());
            trace.set_current_row("C_part", (((b1 as u32) << shl) >> shr).into());
        } else {
            trace.set_final_row("A_byte", (b1 as u32).into());
            trace.set_final_row("C_part", (((b1 as u32) << shl) >> shr).into());
        }

        // 4 rows for each shift operation
        trace.push_row();
        trace.set_current_row("operation_id", op_id);
        trace.set_current_row("A_byte", (b2 as u32).into());
        let c_part_factor = (b2 as u32) << 8;
        let c_part = ((c_part_factor << shl) >> shr).into();
        trace.set_current_row("C_part", c_part);
        let a_row = a.u() & 0xff;
        trace.set_current_row("A", a_row.into());
        trace.set_current_row("B", b);
        trace.set_current_row("C", ((a_row << shl) >> shr).into());
        //
        trace.push_row();
        trace.set_current_row("operation_id", op_id);
        trace.set_current_row("A_byte", (b3 as u32).into());
        let c_part_factor = (b3 as u32) << 16;
        let c_part = ((c_part_factor << shl) >> shr).into();
        trace.set_current_row("C_part", c_part);
        let a_row = a.u() & 0xffff;
        trace.set_current_row("A", a_row.into());
        trace.set_current_row("B", b);
        trace.set_current_row("C", ((a_row << shl) >> shr).into());
        //
        trace.push_row();
        trace.set_current_row("operation_id", op_id);
        trace.set_current_row("A_byte", (b4 as u32).into());
        let c_part_factor = (b4 as u32) << 24;
        let c_part = ((c_part_factor << shl) >> shr).into();
        trace.set_current_row("C_part", c_part);
        let a_row = a.u() & 0xffffff;
        trace.set_current_row("A", a_row.into());
        trace.set_current_row("B", b);
        trace.set_current_row("C", ((a_row << shl) >> shr).into());
        // latch row
        trace.push_row();
        trace.set_current_row("operation_id", op_id);
        trace.set_current_row("A", a);
        trace.set_current_row("B", b);
        trace.set_current_row("C", c);
        trace.set_current_row(
            &format!("{}[{}]", Self::SELECTORS, selector.unwrap()),
            1.into(),
        );
    }
}

pub struct SplitGlMachine;

impl SubmachineKind for SplitGlMachine {
    const SELECTORS: &'static str = "sel";
    const BLOCK_SIZE: u32 = 8;

    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        _op: &str,
        args: &[Elem<F>],
        selector: Option<u32>,
        _submachines: &[&mut dyn Submachine<F>],
    ) {
        let [output_hi, output_low] = args[..] else {
            panic!();
        };

        let lo = output_low.u() as u64;
        let hi = output_hi.u() as u64;

        fn hi_and_lo<F: FieldElement>(hi: u64, lo: u64) -> Elem<F> {
            Elem::from_u64_as_fe_unchecked((hi << 32) | lo)
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
            trace.set_current_row("bytes", b[0].into());
            trace.set_current_row("lt", lt[0].into());
            trace.set_current_row("gt", gt[0].into());
            trace.set_current_row("was_lt", was_lt[0].into());
        } else {
            trace.set_final_row("bytes", b[0].into());
            trace.set_final_row("lt", lt[0].into());
            trace.set_final_row("gt", gt[0].into());
            trace.set_final_row("was_lt", was_lt[0].into());
        }

        // split_gl needs 8 rows:
        // 0
        trace.push_row();
        trace.set_current_row("output_low", Elem::from_u64_as_fe_unchecked(lo & 0xff));
        trace.set_current_row("output_high", 0.into());
        trace.set_current_row("in_acc", Elem::from_u64_as_fe_unchecked(lo & 0xff));
        trace.set_current_row("bytes", b[1].into());
        trace.set_current_row("lt", lt[1].into());
        trace.set_current_row("gt", gt[1].into());
        trace.set_current_row("was_lt", was_lt[1].into());

        // 1
        trace.push_row();
        trace.set_current_row("output_low", Elem::from_u64_as_fe_unchecked(lo & 0xffff));
        trace.set_current_row("output_high", 0.into());
        trace.set_current_row("in_acc", Elem::from_u64_as_fe_unchecked(lo & 0xffff));
        trace.set_current_row("bytes", b[2].into());
        trace.set_current_row("lt", lt[2].into());
        trace.set_current_row("gt", gt[2].into());
        trace.set_current_row("was_lt", was_lt[2].into());

        // 2
        trace.push_row();
        trace.set_current_row("output_low", Elem::from_u64_as_fe_unchecked(lo & 0xffffff));
        trace.set_current_row("output_high", 0.into());
        trace.set_current_row("in_acc", Elem::from_u64_as_fe_unchecked(lo & 0xffffff));
        trace.set_current_row("bytes", b[3].into());
        trace.set_current_row("lt", lt[3].into());
        trace.set_current_row("gt", gt[3].into());
        trace.set_current_row("was_lt", was_lt[3].into());

        // 3
        trace.push_row();
        trace.set_current_row("output_low", Elem::from_u64_as_fe_unchecked(lo));
        trace.set_current_row("output_high", 0.into());
        trace.set_current_row("in_acc", Elem::from_u64_as_fe_unchecked(lo));
        trace.set_current_row("bytes", b[4].into());
        trace.set_current_row("lt", lt[4].into());
        trace.set_current_row("gt", gt[4].into());
        trace.set_current_row("was_lt", was_lt[4].into());

        // 4
        trace.push_row();
        trace.set_current_row("output_low", Elem::from_u64_as_fe_unchecked(lo));
        trace.set_current_row("output_high", Elem::from_u64_as_fe_unchecked(hi & 0xff));
        trace.set_current_row("in_acc", hi_and_lo(hi & 0xff, lo));
        trace.set_current_row("bytes", b[5].into());
        trace.set_current_row("lt", lt[5].into());
        trace.set_current_row("gt", gt[5].into());
        trace.set_current_row("was_lt", was_lt[5].into());

        // 5
        trace.push_row();
        trace.set_current_row("output_low", Elem::from_u64_as_fe_unchecked(lo));
        trace.set_current_row("output_high", Elem::from_u64_as_fe_unchecked(hi & 0xffff));
        trace.set_current_row("in_acc", hi_and_lo(hi & 0xffff, lo));
        trace.set_current_row("bytes", b[6].into());
        trace.set_current_row("lt", lt[6].into());
        trace.set_current_row("gt", gt[6].into());
        trace.set_current_row("was_lt", was_lt[6].into());

        // 6
        trace.push_row();
        trace.set_current_row("output_low", Elem::from_u64_as_fe_unchecked(lo));
        trace.set_current_row("output_high", Elem::from_u64_as_fe_unchecked(hi & 0xffffff));
        trace.set_current_row("in_acc", hi_and_lo(hi & 0xffffff, lo));
        trace.set_current_row("bytes", b[7].into());
        trace.set_current_row("lt", lt[7].into());
        trace.set_current_row("gt", gt[7].into());
        trace.set_current_row("was_lt", was_lt[7].into());

        // 7: bytes/lt/gt/was_lt are set by the next row
        trace.push_row();
        trace.set_current_row(
            &format!("{}[{}]", Self::SELECTORS, selector.unwrap()),
            1.into(),
        );
        trace.set_current_row("output_low", Elem::from_u64_as_fe_unchecked(lo));
        trace.set_current_row("output_high", Elem::from_u64_as_fe_unchecked(hi));
        trace.set_current_row("in_acc", hi_and_lo(hi, lo));
    }
}

pub struct PublicsMachine;
impl SubmachineKind for PublicsMachine {
    const SELECTORS: &'static str = "";
    const BLOCK_SIZE: u32 = 1;
    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        _op: &str,
        args: &[Elem<F>],
        selector: Option<u32>,
        _submachines: &[&mut dyn Submachine<F>],
    ) {
        assert!(selector.is_none());
        let [addr, value] = args[..] else {
            panic!();
        };
        assert!(
            addr.u() < 8,
            "publics machine only supports 8 public values"
        );
        while addr.u() >= trace.len() {
            trace.push_row();
        }
        *trace
            .cols
            .get_mut("value")
            .unwrap()
            .get_mut(addr.u() as usize)
            .unwrap() = value;
    }
}

pub struct PoseidonGlMachine;

impl SubmachineKind for PoseidonGlMachine {
    const SELECTORS: &'static str = "sel";
    const BLOCK_SIZE: u32 = 31; // full rounds + partial rounds + 1

    fn add_operation<F: FieldElement>(
        trace: &mut SubmachineTrace<F>,
        _op: &str,
        args: &[Elem<F>],
        selector: Option<u32>,
        _submachines: &[&mut dyn Submachine<F>],
    ) {
        const STATE_SIZE: usize = 12;
        const OUTPUT_SIZE: usize = 4;
        // const FULL_ROUNDS: usize = 8;
        // const PARTIAL_ROUNDS: usize = 22;

        const INPUT_COLS: [&str; STATE_SIZE] = [
            "input[0]",
            "input[1]",
            "input[2]",
            "input[3]",
            "input[4]",
            "input[5]",
            "input[6]",
            "input[7]",
            "input[8]",
            "input[9]",
            "input[10]",
            "input[11]",
        ];

        const OUTPUT_COLS: [&str; OUTPUT_SIZE] =
            ["output[0]", "output[1]", "output[2]", "output[3]"];

        const STATE_COLS: [&str; STATE_SIZE] = [
            "state[0]",
            "state[1]",
            "state[2]",
            "state[3]",
            "state[4]",
            "state[5]",
            "state[6]",
            "state[7]",
            "state[8]",
            "state[9]",
            "state[10]",
            "state[11]",
        ];

        const X3_COLS: [&str; STATE_SIZE] = [
            "x3[0]", "x3[1]", "x3[2]", "x3[3]", "x3[4]", "x3[5]", "x3[6]", "x3[7]", "x3[8]",
            "x3[9]", "x3[10]", "x3[11]",
        ];

        const X7_COLS: [&str; STATE_SIZE] = [
            "x7[0]", "x7[1]", "x7[2]", "x7[3]", "x7[4]", "x7[5]", "x7[6]", "x7[7]", "x7[8]",
            "x7[9]", "x7[10]", "x7[11]",
        ];

        let [input_addr, output_addr, time_step] = args[0..3] else {
            panic!()
        };
        let input = args[3..3 + STATE_SIZE].to_vec();
        let output = args[3 + STATE_SIZE..3 + STATE_SIZE + OUTPUT_SIZE].to_vec();

        let mut state: Vec<F> = input.iter().map(|e| e.into_fe()).collect();

        for row in 0..(Self::BLOCK_SIZE - 1) as usize {
            trace.push_row();
            for i in 0..STATE_SIZE {
                trace.set_current_row(STATE_COLS[i], Elem::Field(state[i]));
            }
            // memory read/write columns
            if row < STATE_SIZE {
                let v = input[row].f().to_integer().try_into_u64().unwrap();
                let hi = (v >> 32) as u32;
                let lo = (v & 0xffffffff) as u32;
                trace.set_current_row("do_mload", 1.into());
                trace.set_current_row("word_low", lo.into());
                trace.set_current_row("word_high", hi.into());
            } else if row < STATE_SIZE + OUTPUT_SIZE {
                let v = output[row - STATE_SIZE]
                    .f()
                    .to_integer()
                    .try_into_u64()
                    .unwrap();
                let hi = (v >> 32) as u32;
                let lo = (v & 0xffffffff) as u32;
                trace.set_current_row("do_mload", 0.into());
                trace.set_current_row("word_low", lo.into());
                trace.set_current_row("word_high", hi.into());
            } else {
                trace.set_current_row("do_mload", 0.into());
                trace.set_current_row("word_low", 0.into());
                trace.set_current_row("word_high", 0.into());
            }
            // update state for the next row
            // S-Boxes
            let is_full = !(4..26).contains(&row);
            for i in 0..STATE_SIZE {
                let a = state[i] + F::from(poseidon_gl::ROUND_CONSTANTS[i][row]);
                let x3 = a.pow(3.into());
                let x7 = x3.pow(2.into()) * a;
                trace.set_current_row(X3_COLS[i], Elem::Field(x3));
                trace.set_current_row(X7_COLS[i], Elem::Field(x7));
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
            trace.set_current_row(X3_COLS[i], Elem::Field(x3));
            trace.set_current_row(X7_COLS[i], Elem::Field(x7));
            // set output
            trace.set_current_row(STATE_COLS[i], Elem::Field(state[i]));
        }
        // these are the same in the whole block
        trace.set_current_block(Self::BLOCK_SIZE, "operation_id", 0.into());
        trace.set_current_block(Self::BLOCK_SIZE, "time_step", time_step);
        trace.set_current_block(Self::BLOCK_SIZE, "input_addr", input_addr);
        trace.set_current_block(Self::BLOCK_SIZE, "output_addr", output_addr);
        // set selector
        trace.set_current_block(
            Self::BLOCK_SIZE,
            &format!("{}[{}]", Self::SELECTORS, selector.unwrap()),
            1.into(),
        );
        for i in 0..STATE_SIZE {
            trace.set_current_block(Self::BLOCK_SIZE, INPUT_COLS[i], input[i]);
        }
        for i in 0..OUTPUT_SIZE {
            trace.set_current_block(Self::BLOCK_SIZE, OUTPUT_COLS[i], Elem::Field(state[i]));
        }
    }

    fn dummy_block_fix<F: FieldElement>(trace: &mut SubmachineTrace<F>, rows: u32) {
        trace.set_current_block(rows, "do_mload", 0.into());
    }
}
