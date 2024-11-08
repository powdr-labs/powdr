//! A specialized executor for our RISC-V assembly that can speedup witgen and
//! help with making partition decisions.
//!
//! WARNING: the general witness generation/execution code over the polynomial
//! constraints try to ensure the determinism of the instructions. If we bypass
//! much of witness generation using the present module, we lose the
//! non-determinism verification.
//!
//! TODO: perform determinism verification for each instruction independently
//! from execution.

use std::{
    collections::{BTreeMap, HashMap},
    fmt::{self, Display, Formatter},
    path::Path,
    sync::Arc,
};

use builder::TraceBuilder;

use itertools::Itertools;
use powdr_ast::{
    analyzed::Analyzed,
    asm_analysis::{AnalysisASMFile, CallableSymbol, FunctionStatement, LabelStatement, Machine},
    parsed::{
        asm::{parse_absolute_path, AssignmentRegister, DebugDirective},
        BinaryOperation, Expression, FunctionCall, Number, UnaryOperation,
    },
};
use powdr_executor::constant_evaluator::VariablySizedColumn;
use powdr_number::{write_polys_csv_file, FieldElement, LargeInt};
pub use profiler::ProfilerOptions;

pub mod arith;
mod poseidon2_gl;
pub mod poseidon_gl;
mod profiler;
mod submachines;
use submachines::*;
mod memory;
use memory::*;

use crate::profiler::Profiler;

/// Initial value of the PC.
///
/// To match the ZK proof witness, the PC must start after some offset used for
/// proof initialization.
///
/// TODO: get this value from some authoritative place
const PC_INITIAL_VAL: usize = 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Elem<F: FieldElement> {
    /// Only the ranges of i32 and u32 are actually valid for a Binary value.
    /// I.e., [-2**31, 2**32).
    Binary(i64),
    Field(F),
}

impl<F: FieldElement> Elem<F> {
    /// Try to interpret the value of a field as a binary, if it can be represented either as a
    /// u32 or a i32.
    pub fn try_from_fe_as_bin(value: &F) -> Option<Self> {
        if let Some(v) = value.to_integer().try_into_u32() {
            Some(Self::Binary(v as i64))
        } else {
            value.try_into_i32().map(|v| Self::Binary(v as i64))
        }
    }

    pub fn from_u32_as_fe(value: u32) -> Self {
        Self::Field(F::from(value))
    }

    pub fn from_i32_as_fe(value: i32) -> Self {
        Self::Field(F::from(value))
    }

    pub fn from_u64_as_fe_unchecked(value: u64) -> Self {
        Self::Field(F::from(value))
    }

    pub fn from_bool_as_fe(value: bool) -> Self {
        if value {
            Self::Field(F::one())
        } else {
            Self::Field(F::zero())
        }
    }

    /// Interprets the value of self as a field element.
    pub fn into_fe(self) -> F {
        match self {
            Self::Field(f) => f,
            Self::Binary(b) => b.into(),
        }
    }

    /// Interprets the value of self as an i64, ignoring higher bytes if a field element
    pub fn as_i64_from_lower_bytes(&self) -> i64 {
        match self {
            Self::Binary(b) => *b,
            Self::Field(f) => {
                let mut bytes = f.to_bytes_le();
                bytes.truncate(8);
                i64::from_le_bytes(bytes.try_into().unwrap())
            }
        }
    }

    pub fn bin(&self) -> i64 {
        match self {
            Self::Binary(b) => *b,
            Self::Field(_) => panic!(),
        }
    }

    fn u(&self) -> u32 {
        self.bin().try_into().unwrap()
    }

    fn s(&self) -> i32 {
        self.bin().try_into().unwrap()
    }

    fn is_zero(&self) -> bool {
        match self {
            Self::Binary(b) => *b == 0,
            Self::Field(f) => f.is_zero(),
        }
    }

    fn add(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Binary(a), Self::Binary(b)) => Self::Binary(a.checked_add(*b).unwrap()),
            (Self::Field(a), Self::Field(b)) => Self::Field(*a + *b),
            (Self::Binary(a), Self::Field(b)) => Self::Field(F::from(*a) + *b),
            (Self::Field(a), Self::Binary(b)) => Self::Field(*a + F::from(*b)),
        }
    }

    fn sub(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Binary(a), Self::Binary(b)) => Self::Binary(a.checked_sub(*b).unwrap()),
            (Self::Field(a), Self::Field(b)) => Self::Field(*a - *b),
            (Self::Binary(a), Self::Field(b)) => Self::Field(F::from(*a) - *b),
            (Self::Field(a), Self::Binary(b)) => Self::Field(*a - F::from(*b)),
        }
    }

    fn mul(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Binary(a), Self::Binary(b)) => match a.checked_mul(*b) {
                Some(v) => Self::Binary(v),
                None => {
                    let a = F::from(*a);
                    let b = F::from(*b);
                    Self::Field(a * b)
                }
            },
            (Self::Field(a), Self::Field(b)) => Self::Field(*a * *b),
            (Self::Binary(a), Self::Field(b)) => Self::Field(F::from(*a) * *b),
            (Self::Field(a), Self::Binary(b)) => Self::Field(*a * F::from(*b)),
        }
    }

    fn div(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Binary(a), Self::Binary(b)) => Self::Binary(a / b),
            (Self::Field(a), Self::Field(b)) => Self::Field(*a / *b),
            (Self::Binary(a), Self::Field(b)) => Self::Field(F::from(*a) / *b),
            (Self::Field(a), Self::Binary(b)) => Self::Field(*a / F::from(*b)),
        }
    }
}

fn decompose_lower32(x: i64) -> (u8, u8, u8, u8, u8) {
    let b1 = (x & 0xff) as u8;
    let b2 = ((x >> 8) & 0xff) as u8;
    let b3 = ((x >> 16) & 0xff) as u8;
    let b4 = ((x >> 24) & 0xff) as u8;
    let sign = ((x >> 31) & 1) as u8;
    (b1, b2, b3, b4, sign)
}

impl<F: FieldElement> From<i64> for Elem<F> {
    fn from(value: i64) -> Self {
        Self::Binary(value)
    }
}

impl<F: FieldElement> From<u32> for Elem<F> {
    fn from(value: u32) -> Self {
        Self::Binary(value as i64)
    }
}

impl<F: FieldElement> From<i32> for Elem<F> {
    fn from(value: i32) -> Self {
        Self::Binary(value as i64)
    }
}

impl<F: FieldElement> From<usize> for Elem<F> {
    fn from(value: usize) -> Self {
        Self::Binary(value as i64)
    }
}

impl<F: FieldElement> Display for Elem<F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary(b) => write!(f, "{b}"),
            Self::Field(fe) => write!(f, "{fe}"),
        }
    }
}

pub type MemoryState = HashMap<u32, u32>;
pub type RegisterMemoryState<F> = HashMap<u32, F>;

#[derive(Debug)]
pub enum MemOperationKind {
    Read,
    Write,
}

#[derive(Debug)]
pub struct MemOperation {
    /// The row of the execution trace the memory operation happened.
    pub row: usize,
    pub kind: MemOperationKind,
    pub address: u32,
}

pub struct RegWrite<F: FieldElement> {
    /// The row of the execution trace this write will result into. Multiple
    /// writes at the same row are valid: the last write to a given reg_idx will
    /// define the final value of the register in that row.
    row: usize,
    /// Index of the register in the register bank.
    reg_idx: u16,
    val: F,
}

pub struct ExecutionTrace<F: FieldElement> {
    reg_map: HashMap<String, u16>,

    /// Values of the registers in the execution trace.
    ///
    /// Each N elements is a row with all registers, where N is the number of
    /// registers.
    reg_writes: Vec<RegWrite<F>>,

    /// Writes and reads to memory.
    mem_ops: Vec<MemOperation>,

    /// The length of the trace, after applying the reg_writes.
    len: usize,

    /// witness columns
    cols: HashMap<String, Vec<Elem<F>>>,
}

impl<F: FieldElement> ExecutionTrace<F> {
    pub fn new(
        witness_cols: Vec<String>,
        reg_map: HashMap<String, u16>,
        reg_writes: Vec<RegWrite<F>>,
        pc: usize,
    ) -> Self {
        let cols: HashMap<String, _> = witness_cols
            .into_iter()
            .filter(|n| n.starts_with("main::"))
            .map(|n| (n, vec![Elem::Field(F::zero()), Elem::Field(F::zero())]))
            .collect();

        ExecutionTrace {
            reg_map,
            reg_writes,
            mem_ops: Vec::new(),
            len: pc,
            cols,
        }
    }

    /// Replay the execution and get the register values per trace row.
    fn replay(&self) -> TraceReplay<F> {
        TraceReplay {
            trace: self,
            regs: vec![0.into(); self.reg_map.len()],
            pc_idx: self.reg_map["pc"] as usize,
            next_write: 0,
            next_r: 0,
        }
    }

    /// transpose the register write operations into value columns
    fn generate_registers_trace(&self) -> Vec<(String, Vec<Elem<F>>)> {
        let mut reg_values: HashMap<&str, Vec<Elem<F>>> =
            HashMap::with_capacity(self.reg_map.len());

        let mut rows = self.replay();
        while let Some(row) = rows.next_row() {
            for (reg_name, &index) in self.reg_map.iter() {
                reg_values
                    .entry(reg_name)
                    .or_default()
                    .push(Elem::Field(row[index as usize]));
            }
        }

        reg_values
            .into_iter()
            .map(|(n, c)| (format!("main::{n}"), c))
            .collect()
    }
}

pub struct TraceReplay<'a, F: FieldElement> {
    trace: &'a ExecutionTrace<F>,
    regs: Vec<F>,
    pc_idx: usize,
    next_write: usize,
    next_r: usize,
}

impl<'a, F: FieldElement> TraceReplay<'a, F> {
    /// Returns the next row's registers value.
    ///
    /// Just like an iterator's next(), but returns the value borrowed from self.
    pub fn next_row(&mut self) -> Option<&[F]> {
        if self.next_r == self.trace.len {
            return None;
        }

        // we optimistically increment the PC, if it is a jump or special case,
        // one of the writes will overwrite it
        self.regs[self.pc_idx] += 1.into();

        while let Some(next_write) = self.trace.reg_writes.get(self.next_write) {
            if next_write.row > self.next_r {
                break;
            }
            self.next_write += 1;

            self.regs[next_write.reg_idx as usize] = next_write.val;
        }

        self.next_r += 1;
        Some(&self.regs[..])
    }
}

#[derive(Default)]
pub struct RegisterMemory<F: FieldElement> {
    last: HashMap<u32, Elem<F>>,
    second_last: HashMap<u32, Elem<F>>,
}

impl<F: FieldElement> RegisterMemory<F> {
    pub fn for_bootloader(&self) -> HashMap<u32, F> {
        self.second_last
            .iter()
            .map(|(k, v)| (*k, v.into_fe()))
            .collect()
    }
}

mod builder {
    use std::{cmp, collections::HashMap};

    use powdr_ast::{
        analyzed::{Analyzed, DegreeRange},
        asm_analysis::{Machine, RegisterTy},
    };
    use powdr_number::FieldElement;

    use crate::{
        BinaryMachine, Elem, ExecMode, Execution, ExecutionTrace, MemOperation, MemOperationKind,
        MemoryMachine, MemoryState, RegWrite, RegisterMemory, ShiftMachine, SplitGlMachine,
        Submachine, SubmachineBoxed, PC_INITIAL_VAL,
    };

    fn namespace_degree_range<F: FieldElement>(
        opt_pil: &Analyzed<F>,
        namespace: &str,
    ) -> DegreeRange {
        opt_pil
            .committed_polys_in_source_order()
            .find(|(s, _)| s.absolute_name.contains(&format!("{namespace}::")))
            .and_then(|(s, _)| s.degree)
            // all machines/columns should have a degree range defined
            .unwrap()
    }

    fn register_names(main: &Machine) -> Vec<&str> {
        main.registers
            .iter()
            .filter_map(|statement| {
                if statement.ty != RegisterTy::Assignment {
                    Some(&statement.name[..])
                } else {
                    None
                }
            })
            .collect()
    }

    pub struct TraceBuilder<'b, F: FieldElement> {
        trace: ExecutionTrace<F>,

        pub submachines: HashMap<String, Box<dyn Submachine<F>>>,
        pub regs_machine: MemoryMachine<F>,
        pub memory_machine: MemoryMachine<F>,

        /// Maximum rows we can run before we stop the execution.
        max_rows: usize,

        // index of special case registers to look after:
        pc_idx: u16,

        /// The value of PC at the start of the execution of the current row.
        curr_pc: Elem<F>,

        /// The PC in the register bank refers to the batches, we have to track our
        /// actual program counter independently.
        next_statement_line: u32,

        /// When PC is written, we need to know what line to actually execute next
        /// from this map of batch to statement line.
        batch_to_line_map: &'b [u32],

        /// Current register bank
        regs: Vec<Elem<F>>,

        /// Current memory.
        mem: HashMap<u32, u32>,

        /// Separate register memory, last and second last.
        reg_mem: RegisterMemory<F>,

        /// The execution mode we running.
        /// Fast: do not save the register's trace and memory accesses.
        /// Trace: save everything - needed for continuations.
        mode: ExecMode,
    }

    impl<'a, 'b, F: FieldElement> TraceBuilder<'b, F> {
        /// Creates a new builder.
        ///
        /// May fail if max_rows_len is too small or if the main machine is
        /// empty. In this case, the final (empty) execution trace is returned
        /// in Err.
        pub fn new(
            main: &'a Machine,
            opt_pil: Option<&Analyzed<F>>,
            witness_cols: Vec<String>,
            mem: MemoryState,
            batch_to_line_map: &'b [u32],
            max_rows_len: usize,
            mode: ExecMode,
        ) -> Result<Self, Box<Execution<F>>> {
            let reg_map = register_names(main)
                .into_iter()
                .enumerate()
                .map(|(i, name)| (name.to_string(), i as u16))
                .collect::<HashMap<String, u16>>();

            let reg_len = reg_map.len();

            // To save cache/memory bandwidth, I set the register index to be
            // u16, so panic if it doesn't fit (it obviously will fit for RISC-V).
            <usize as TryInto<u16>>::try_into(reg_len).unwrap();

            // To avoid a special case when replaying the trace, we create a
            // special write operation that sets the PC with 0 in the first row.
            let pc_idx = reg_map["pc"];
            let reg_writes = vec![RegWrite {
                row: 0,
                reg_idx: pc_idx,
                val: 0.into(),
            }];

            let mut regs = vec![0.into(); reg_len];
            regs[pc_idx as usize] = PC_INITIAL_VAL.into();

            let submachines: HashMap<String, Box<dyn Submachine<F>>> = [
                ("binary", BinaryMachine::new_boxed("main_binary")),
                ("shift", ShiftMachine::new_boxed("main_shift")),
                ("split_gl", SplitGlMachine::new_boxed("main_split_gl")),
            ]
            .into_iter()
            .map(|(name, m)| (name.to_string(), m))
            .collect();

            let mut ret = Self {
                pc_idx,
                curr_pc: PC_INITIAL_VAL.into(),
                trace: ExecutionTrace::new(witness_cols, reg_map, reg_writes, PC_INITIAL_VAL + 1),
                submachines,
                regs_machine: MemoryMachine::new("main_regs"),
                memory_machine: MemoryMachine::new("main_memory"),
                next_statement_line: 1,
                batch_to_line_map,
                max_rows: max_rows_len,
                regs,
                mem,
                reg_mem: Default::default(),
                mode,
            };

            if ret.has_enough_rows() || ret.set_next_pc().is_none() {
                Err(Box::new(ret.finish(opt_pil)))
            } else {
                Ok(ret)
            }
        }

        pub(crate) fn main_columns_len(&self) -> usize {
            let cols_len = self
                .trace
                .cols
                .values()
                .next()
                .map(|v| v.len())
                .unwrap_or(0);

            // sanity check
            assert!(self.trace.len <= cols_len);

            cols_len
        }

        /// get the value of PC as of the start of the execution of the current row.
        pub(crate) fn get_pc(&self) -> Elem<F> {
            self.curr_pc
        }

        /// get the value of PC as updated by the last executed instruction.
        /// The actual PC is only updated when moving to a new row.
        pub(crate) fn get_next_pc(&self) -> Elem<F> {
            self.regs[self.pc_idx as usize]
        }

        /// get current value of register
        pub(crate) fn get_reg(&self, idx: &str) -> Elem<F> {
            self.get_reg_idx(self.trace.reg_map[idx])
        }

        /// get current value of register by register index instead of name
        fn get_reg_idx(&self, idx: u16) -> Elem<F> {
            if idx == self.pc_idx {
                return self.get_pc();
            }
            self.regs[idx as usize]
        }

        /// sets the PC
        pub(crate) fn set_pc(&mut self, value: Elem<F>) {
            // updates the internal statement-based program counter accordingly:
            self.next_statement_line = self.batch_to_line_map[value.u() as usize];
            self.set_reg_idx(self.pc_idx, value);
        }

        /// set next value of register, accounting to x0 writes
        ///
        /// to set the PC, use set_pc() instead of this
        pub(crate) fn set_reg(&mut self, idx: &str, value: impl Into<Elem<F>>) {
            self.set_reg_impl(idx, value.into())
        }

        fn set_reg_impl(&mut self, idx: &str, value: Elem<F>) {
            let idx = self.trace.reg_map[idx];
            assert!(idx != self.pc_idx);
            self.set_reg_idx(idx, value);
        }

        /// raw set next value of register by register index instead of name
        fn set_reg_idx(&mut self, idx: u16, value: Elem<F>) {
            // Record register write in trace. Only for non-assignment registers.
            if let ExecMode::Trace = self.mode {
                self.trace.reg_writes.push(RegWrite {
                    row: self.trace.len,
                    reg_idx: idx,
                    val: value.into_fe(),
                });
            }

            self.regs[idx as usize] = value;
        }

        pub fn set_col_idx(&mut self, name: &str, idx: usize, value: Elem<F>) {
            if let ExecMode::Trace = self.mode {
                let col = self
                    .trace
                    .cols
                    .get_mut(name)
                    .unwrap_or_else(|| panic!("col not found: {name}"));
                *col.get_mut(idx).unwrap() = value;
            }
        }

        pub fn set_col(&mut self, name: &str, value: Elem<F>) {
            if let ExecMode::Trace = self.mode {
                let col = self
                    .trace
                    .cols
                    .get_mut(name)
                    .unwrap_or_else(|| panic!("col not found: {name}"));
                *col.last_mut().unwrap() = value;
            }
        }

        pub fn get_col(&self, name: &str) -> Elem<F> {
            if let ExecMode::Trace = self.mode {
                let col = self
                    .trace
                    .cols
                    .get(name)
                    .unwrap_or_else(|| panic!("col not found: {name}"));
                *col.last().unwrap()
            } else {
                Elem::Field(F::zero())
            }
        }

        pub fn push_row(&mut self) {
            if let ExecMode::Trace = self.mode {
                self.trace
                    .cols
                    .values_mut()
                    .for_each(|v| v.push(Elem::Field(0.into())));
            }
        }

        pub fn extend_rows(&mut self, len: u32) {
            if let ExecMode::Trace = self.mode {
                self.trace.cols.values_mut().for_each(|v| {
                    let last = *v.last().unwrap();
                    v.resize(len as usize, last);
                });
            }
        }

        /// advance to next row, returns the index to the statement that must be
        /// executed now, or None if the execution is finished
        pub fn advance(&mut self) -> Option<u32> {
            let next_pc = self.regs[self.pc_idx as usize];
            if self.curr_pc != next_pc {
                // If we are at the limit of rows, stop the execution
                if self.has_enough_rows() {
                    return None;
                }

                self.trace.len += 1;
                self.curr_pc = next_pc;
            }

            // advance to the next statement
            let st_line = self.next_statement_line;

            // optimistically advance the internal and register PCs
            self.next_statement_line += 1;
            self.set_next_pc().and(Some(st_line))
        }

        pub(crate) fn set_mem(&mut self, addr: u32, val: u32) {
            if let ExecMode::Trace = self.mode {
                self.trace.mem_ops.push(MemOperation {
                    row: self.trace.len,
                    kind: MemOperationKind::Write,
                    address: addr,
                });
            }

            self.mem.insert(addr, val);
        }

        pub(crate) fn get_mem(&mut self, addr: u32) -> u32 {
            if let ExecMode::Trace = self.mode {
                self.trace.mem_ops.push(MemOperation {
                    row: self.trace.len,
                    kind: MemOperationKind::Read,
                    address: addr,
                });
            }

            *self.mem.get(&addr).unwrap_or(&0)
        }

        pub(crate) fn set_reg_mem(&mut self, addr: u32, val: Elem<F>) {
            if addr != 0 {
                self.reg_mem.last.insert(addr, val);
            }
        }

        pub(crate) fn get_reg_mem(&mut self, addr: u32) -> Elem<F> {
            let zero: Elem<F> = 0u32.into();
            if addr == 0 {
                zero
            } else {
                *self.reg_mem.last.get(&addr).unwrap_or(&zero)
            }
        }

        pub(crate) fn backup_reg_mem(&mut self) {
            self.reg_mem.second_last = self.reg_mem.last.clone();
        }

        pub fn finish(mut self, opt_pil: Option<&Analyzed<F>>) -> Execution<F> {
            if let ExecMode::Fast = self.mode {
                return Execution {
                    trace_len: self.trace.len,
                    memory: self.mem,
                    memory_accesses: Vec::new(),
                    trace: HashMap::new(),
                    register_memory: HashMap::new(),
                };
            }

            let pil = opt_pil.unwrap();

            let main_degree = {
                let range = namespace_degree_range(pil, "main");
                assert!(
                    self.main_columns_len().next_power_of_two() <= range.max as usize,
                    "main machine larger than PIL's max degree: {} > {}",
                    self.main_columns_len().next_power_of_two(),
                    range.max as usize
                );
                std::cmp::max(
                    self.main_columns_len().next_power_of_two() as u32,
                    range.min as u32,
                )
            };

            // turn register write operations into witness columns
            let main_regs = self.trace.generate_registers_trace();
            self.trace.cols.extend(main_regs);

            // fill up main trace to degree
            self.extend_rows(main_degree);

            // add submachine traces to main trace
            // ----------------------------
            for (_, mut machine) in self.submachines {
                if machine.len() == 0 {
                    // ignore empty machines
                    continue;
                }
                machine.final_row_override();
                let range = namespace_degree_range(pil, machine.namespace());
                assert!(
                    machine.len().next_power_of_two() <= range.max as u32,
                    "machine {} larger than PIL's max degree: {} > {}",
                    machine.namespace(),
                    machine.len().next_power_of_two(),
                    range.max as u32
                );
                // extend with dummy blocks up to the required machine degree
                let machine_degree =
                    std::cmp::max(machine.len().next_power_of_two(), range.min as u32);
                while machine.len() < machine_degree {
                    machine.push_dummy_block();
                }
                for (col_name, col) in machine.take_cols() {
                    assert!(self.trace.cols.insert(col_name, col).is_none());
                }
            }

            // add regs memory trace
            // ----------------------------
            let regs_degree = {
                let range = namespace_degree_range(pil, &self.regs_machine.namespace);
                assert!(
                    self.regs_machine.len().next_power_of_two() <= range.max as u32,
                    "register memory machine larger than PIL's max degree: {} > {}",
                    self.regs_machine.len().next_power_of_two(),
                    range.max as u32
                );
                std::cmp::max(
                    self.regs_machine.len().next_power_of_two(),
                    range.min as u32,
                )
            };
            for (col_name, col) in self.regs_machine.take_cols(regs_degree) {
                assert!(self.trace.cols.insert(col_name, col).is_none());
            }

            // add main memory trace
            // ----------------------------
            let mem_degree = {
                let range = namespace_degree_range(pil, &self.memory_machine.namespace);
                assert!(
                    self.memory_machine.len().next_power_of_two() <= range.max as u32,
                    "register memory machine larger than PIL's max degree: {} > {}",
                    self.memory_machine.len().next_power_of_two(),
                    range.max as u32
                );
                std::cmp::max(
                    self.memory_machine.len().next_power_of_two(),
                    range.min as u32,
                )
            };
            for (col_name, col) in self.memory_machine.take_cols(mem_degree) {
                assert!(self.trace.cols.insert(col_name, col).is_none());
            }

            Execution {
                trace_len: self.trace.len,
                memory: self.mem,
                memory_accesses: std::mem::take(&mut self.trace.mem_ops),
                trace: self
                    .trace
                    .cols
                    .into_iter()
                    // convert Elem<F> into F
                    .map(|(name, elems)| {
                        let fes = elems.into_iter().map(|e| e.into_fe()).collect();
                        (name, fes)
                    })
                    .collect(),
                register_memory: self.reg_mem.for_bootloader(),
            }
        }

        /// Should we stop the execution because the maximum number of rows has
        /// been reached?
        fn has_enough_rows(&self) -> bool {
            self.trace.len >= self.max_rows
        }

        /// Optimistically increment PC, but the execution might rewrite it.
        ///
        /// Only do it when running the last statement of a batch.
        fn set_next_pc(&mut self) -> Option<()> {
            let pc = self.curr_pc.u();
            let line_of_next_batch = *self.batch_to_line_map.get(pc as usize + 1)?;

            match self.next_statement_line.cmp(&line_of_next_batch) {
                cmp::Ordering::Less => (),
                cmp::Ordering::Equal => {
                    // Write it directly. We don't want to call set_reg_idx and
                    // trace the natural increment of the PC.
                    self.regs[self.pc_idx as usize] = (pc + 1).into();
                }
                cmp::Ordering::Greater => {
                    if pc < 2 {
                        // end of program, no real batch to execute
                        return None;
                    }
                    panic!(
                        "next_statement_line: {} > line_of_next_batch: {}",
                        self.next_statement_line, line_of_next_batch
                    );
                }
            };

            Some(())
        }
    }
}

pub fn get_main_machine(program: &AnalysisASMFile) -> &Machine {
    program.get_machine(&parse_absolute_path("::Main")).unwrap()
}

struct PreprocessedMain<'a, F: FieldElement> {
    /// list of all statements (batches expanded)
    statements: Vec<&'a FunctionStatement>,
    /// label to batch number
    label_map: HashMap<&'a str, Elem<F>>,
    /// batch number to its first statement idx
    batch_to_line_map: Vec<u32>,
    /// file number to (dir,name)
    debug_files: Vec<(&'a str, &'a str)>,
    /// function label to batch number
    function_starts: BTreeMap<usize, &'a str>,
    /// .debug loc to batch number
    location_starts: BTreeMap<usize, (usize, usize)>,
}

/// Returns the list of instructions, directly indexable by PC, the map from
/// labels to indices into that list, and the list with the start of each batch.
fn preprocess_main_function<F: FieldElement>(machine: &Machine) -> PreprocessedMain<F> {
    let CallableSymbol::Function(main_function) = &machine.callable.0["main"] else {
        panic!("main function missing")
    };

    let orig_statements = &main_function.body.statements;

    let mut statements = Vec::new();
    let mut label_map = HashMap::new();
    let mut batch_to_line_map = vec![0; PC_INITIAL_VAL];
    let mut debug_files = Vec::new();
    let mut function_starts = BTreeMap::new();
    let mut location_starts = BTreeMap::new();

    for (batch_idx, batch) in orig_statements.iter_batches().enumerate() {
        batch_to_line_map.push(statements.len() as u32);
        let mut statement_seen = false;
        for s in batch.statements {
            match s {
                FunctionStatement::Assignment(_)
                | FunctionStatement::Instruction(_)
                | FunctionStatement::Return(_) => {
                    statement_seen = true;
                    statements.push(s)
                }
                FunctionStatement::DebugDirective(d) => {
                    match &d.directive {
                        DebugDirective::File(idx, dir, file) => {
                            // debug files should be densely packed starting
                            // from 1, so the idx should match vec size + 1:
                            assert_eq!(*idx, debug_files.len() + 1);
                            debug_files.push((dir.as_str(), file.as_str()));
                        }
                        DebugDirective::Loc(file, line, _) => {
                            location_starts.insert(batch_idx + PC_INITIAL_VAL, (*file, *line));
                            statements.push(s);
                        }
                        DebugDirective::OriginalInstruction(_) => {
                            // keep debug locs for debugging purposes
                            statements.push(s);
                        }
                    }
                }
                FunctionStatement::Label(LabelStatement { source: _, name }) => {
                    // assert there are no statements in the middle of a block
                    assert!(!statement_seen);
                    label_map.insert(name.as_str(), (batch_idx + PC_INITIAL_VAL).into());
                    // TODO: would looking for "___dot_Lfunc_begin" be less hacky? would require more work to handle ecalls though...
                    if !name.contains("___dot_L") {
                        function_starts.insert(batch_idx + PC_INITIAL_VAL, name.as_str());
                    }
                }
            }
        }
    }
    assert!(statements.len() <= u32::MAX as usize);

    // add a final element to the map so the queries don't overflow:
    batch_to_line_map.push(statements.len() as u32);

    PreprocessedMain {
        statements,
        label_map,
        batch_to_line_map,
        debug_files,
        function_starts,
        location_starts,
    }
}

type Callback<'a, F> = dyn powdr_executor::witgen::QueryCallback<F> + 'a;

struct Executor<'a, 'b, F: FieldElement> {
    proc: TraceBuilder<'b, F>,
    label_map: HashMap<&'a str, Elem<F>>,
    inputs: &'b Callback<'b, F>,
    bootloader_inputs: Vec<Elem<F>>,
    fixed: Arc<Vec<(String, VariablySizedColumn<F>)>>,
    // program columns: maps "ROM" fixed cols to respective witness cols
    program_cols: HashMap<String, String>,
    step: u32,
    mode: ExecMode,
}

impl<'a, 'b, F: FieldElement> Executor<'a, 'b, F> {
    fn init(&mut self) {
        self.step = 4;
        for i in 0..2 {
            for (fixed, col) in &self.program_cols {
                let val = Elem::Field(
                    *self
                        .get_fixed(fixed)
                        .unwrap_or(&Vec::new())
                        .get(i as usize)
                        .unwrap_or(&F::zero()),
                );
                self.proc.set_col_idx(col, i as usize, val);
            }
            self.proc
                .set_col_idx("main::_operation_id", i as usize, 2.into());
            self.proc
                .set_col_idx("main::pc_update", i as usize, (i + 1).into());
        }
    }

    fn get_fixed(&self, name: &str) -> Option<&Vec<F>> {
        self.fixed
            .iter()
            .find(|(n, _)| n == name)
            // ROM is uniquely sized, which for now is all we looking at
            .map(|(_, v)| v.get_uniquely_sized().expect("not uniquely sized!"))
    }

    fn sink_id(&self) -> u32 {
        // sink_id is the length of the ROM. we find it by looking at the line instr__loop activates
        self.get_fixed("main__rom::p_instr__loop")
            .unwrap()
            .iter()
            .position(|val| val.is_one())
            .map(|pos| pos as u32)
            .expect("could not find sink_id by looking at the p_instr__loop column")
    }

    /// read register value, updating the register memory machine
    fn reg_read(&mut self, step_offset: u32, reg: u32, selector_idx: u32) -> Elem<F> {
        let val = self.proc.get_reg_mem(reg);
        self.proc
            .regs_machine
            .read(self.step + step_offset, reg, val, selector_idx);
        val
    }

    /// write value to register, updating the register memory machine
    fn reg_write(&mut self, step_offset: u32, reg: u32, val: Elem<F>, selector_idx: u32) {
        if let ExecMode::Trace = self.mode {
            self.proc
                .regs_machine
                .write(self.step + step_offset, reg, val, selector_idx);
        }
        self.proc.set_reg_mem(reg, val);
    }

    fn set_program_columns(&mut self, pc: u32) {
        if let ExecMode::Trace = self.mode {
            // set witness from the program definition
            for (fixed, col) in &self.program_cols {
                let val = Elem::Field(
                    *self
                        .get_fixed(fixed)
                        .unwrap_or(&Vec::new())
                        .get(pc as usize)
                        .unwrap_or(&F::zero()),
                );
                self.proc.set_col(col, val);
            }
            self.proc.set_col("main::_operation_id", 2.into());
        }
    }

    fn exec_instruction(&mut self, name: &str, args: &[Expression]) -> Vec<Elem<F>> {
        // shorthand macros for setting/getting main machine witness values in the current row
        macro_rules! set_col {
            ($name:ident, $val:expr) => {
                self.proc
                    .set_col(concat!("main::", stringify!($name)), $val);
            };
        }
        macro_rules! get_col {
            ($name:ident) => {
                self.proc.get_col(concat!("main::", stringify!($name)))
            };
        }

        let args = args
            .iter()
            .map(|expr| self.eval_expression(expr)[0])
            .collect::<Vec<_>>();

        self.proc.backup_reg_mem();

        set_col!(X, get_col!(X_const));
        set_col!(Y, get_col!(Y_const));
        set_col!(Z, get_col!(Z_const));
        set_col!(W, get_col!(W_const));
        self.proc
            .set_col(&format!("main::instr_{name}"), Elem::from_u32_as_fe(1));

        let r = match name {
            "set_reg" => {
                let addr = args[0].u();
                let val = args[1];
                self.reg_write(0, addr, val, 3);

                set_col!(Y, val);

                if !get_col!(Y_read_free).is_zero() {
                    set_col!(Y_free_value, val);
                }

                Vec::new()
            }
            "get_reg" => {
                let addr = args[0].u();
                let val = self.reg_read(0, addr, 0);

                vec![val]
            }
            "affine" => {
                let read_reg = args[0].u();
                let val1 = self.reg_read(0, read_reg, 0);
                let write_reg = args[1].u();
                let factor = args[2];
                let offset = args[3];

                let res = val1.mul(&factor).add(&offset);

                self.reg_write(1, write_reg, res, 3);
                set_col!(tmp1_col, val1);

                Vec::new()
            }

            "mstore" | "mstore_bootloader" => {
                let read_reg1 = args[0].u();
                let read_reg2 = args[1].u();
                let addr1 = self.reg_read(0, read_reg1, 0);
                let addr2 = self.reg_read(1, read_reg2, 1);
                let offset = args[2].bin();
                let read_reg3 = args[3].u();
                let value = self.reg_read(2, read_reg3, 2);

                let addr = addr1.bin() - addr2.bin() + offset;
                // assumptions from the asm machine
                assert!(addr >= 0);
                assert_eq!(addr % 4, 0);

                set_col!(
                    wrap_bit,
                    if addr > u32::MAX as i64 {
                        Elem::Field(F::one())
                    } else {
                        Elem::Field(F::zero())
                    }
                );

                let addr = addr as u32;
                self.proc.set_mem(addr, value.u());
                self.proc
                    .memory_machine
                    .write(self.step + 3, addr, value, 1);

                set_col!(tmp1_col, addr1);
                set_col!(tmp2_col, addr2);
                set_col!(tmp3_col, value);

                let (b1, b2, b3, b4, _sign) = decompose_lower32(addr.into());
                set_col!(X_b1, Elem::from_u32_as_fe(b1.into()));
                set_col!(X_b2, Elem::from_u32_as_fe(b2.into()));
                set_col!(X_b3, Elem::from_u32_as_fe(b3.into()));
                set_col!(X_b4, Elem::from_u32_as_fe(b4.into()));

                Vec::new()
            }
            "mload" => {
                let read_reg = args[0].u();
                let addr1 = self.reg_read(0, read_reg, 0);
                let offset = args[1].bin();
                let write_addr1 = args[2].u();
                let write_addr2 = args[3].u();

                let addr = addr1.bin() + offset;

                let val = self.proc.get_mem(addr as u32 & 0xfffffffc);
                let rem = addr % 4;

                self.reg_write(2, write_addr1, val.into(), 3);
                self.reg_write(3, write_addr2, rem.into(), 4);

                set_col!(tmp1_col, addr1);
                set_col!(tmp3_col, Elem::from_u32_as_fe(val));
                set_col!(tmp4_col, Elem::from_u32_as_fe(rem as u32));

                let v = addr1.add(&args[1]).as_i64_from_lower_bytes();
                let (b1, b2, b3, b4, _sign) = decompose_lower32(v);
                set_col!(X_b1, Elem::from_u32_as_fe((b1 / 4).into()));
                set_col!(X_b2, Elem::from_u32_as_fe(b2.into()));
                set_col!(X_b3, Elem::from_u32_as_fe(b3.into()));
                set_col!(X_b4, Elem::from_u32_as_fe(b4.into()));
                set_col!(
                    wrap_bit,
                    Elem::from_u32_as_fe(((v as u64 >> 32) & 1) as u32)
                );

                self.proc.memory_machine.read(
                    self.step + 1,
                    addr as u32 & 0xfffffffc,
                    val.into(),
                    0,
                );

                Vec::new()
            }
            // TODO: update to witness generation for continuations
            "load_bootloader_input" => {
                let addr = self.reg_read(0, args[0].u(), 0);
                let write_addr = args[1].u();
                let factor = args[2].bin();
                let offset = args[3].bin();

                let addr = addr.bin() * factor + offset;
                let val = self.bootloader_inputs[addr as usize];

                self.reg_write(2, write_addr, val, 3);

                Vec::new()
            }
            // TODO: update to witness generation for continuations
            "assert_bootloader_input" => {
                let addr = self.reg_read(0, args[0].u(), 0);
                let val = self.reg_read(1, args[1].u(), 1);
                let factor = args[2].bin();
                let offset = args[3].bin();

                let addr = (addr.bin() * factor + offset) as usize;
                let actual_val = self.bootloader_inputs[addr];

                assert_eq!(val, actual_val);

                Vec::new()
            }
            "load_label" => {
                let write_reg = args[0].u();
                let label = args[1];
                self.reg_write(0, write_reg, label, 3);

                set_col!(tmp1_col, label);

                self.proc.set_col("main::instr_load_label_param_l", label);

                Vec::new()
            }
            "jump" => {
                let label = args[0];
                let next_pc = self.proc.get_pc().u() + 1;
                let write_reg = args[1].u();

                self.reg_write(0, write_reg, next_pc.into(), 3);

                self.proc.set_pc(label);

                self.proc.set_col("main::instr_jump_param_l", label);

                Vec::new()
            }
            "jump_dyn" => {
                let read_reg = args[0].u();
                let addr = self.reg_read(0, read_reg, 0);
                let next_pc = self.proc.get_pc().u() + 1;
                let write_reg = args[1].u();

                self.reg_write(0, write_reg, next_pc.into(), 3);

                self.proc.set_pc(addr);

                set_col!(tmp1_col, addr);

                Vec::new()
            }
            // TODO: update to witness generation for continuations
            "jump_to_bootloader_input" => {
                let bootloader_input_idx = args[0].bin() as usize;
                let addr = self.bootloader_inputs[bootloader_input_idx];
                self.proc.set_pc(addr);

                Vec::new()
            }
            "branch_if_diff_nonzero" => {
                let read_reg1 = args[0].u();
                let read_reg2 = args[1].u();
                let val1 = self.reg_read(0, read_reg1, 0);
                let val2 = self.reg_read(1, read_reg2, 1);

                let val: Elem<F> = val1.sub(&val2);
                let label = args[2];
                if !val.is_zero() {
                    self.proc.set_pc(label);
                }

                set_col!(tmp1_col, val1);
                set_col!(tmp2_col, val2);
                set_col!(XX, val);
                set_col!(XXIsZero, Elem::from_bool_as_fe(get_col!(XX).is_zero()));
                if !get_col!(XX).is_zero() {
                    set_col!(XX_inv, Elem::Field(F::one() / get_col!(XX).into_fe()));
                }

                self.proc
                    .set_col("main::instr_branch_if_diff_nonzero_param_l", label);

                Vec::new()
            }
            "branch_if_diff_equal" => {
                let read_reg1 = args[0].u();
                let read_reg2 = args[1].u();
                let val1 = self.reg_read(0, read_reg1, 0);
                let val2 = self.reg_read(1, read_reg2, 1);
                let offset = args[2];
                let val: Elem<F> = val1.sub(&val2).sub(&offset);
                let label = args[3];

                if val.is_zero() {
                    self.proc.set_pc(label);
                }

                set_col!(tmp1_col, val1);
                set_col!(tmp2_col, val2);
                set_col!(XX, val);
                set_col!(XXIsZero, Elem::from_bool_as_fe(get_col!(XX).is_zero()));
                if !get_col!(XX).is_zero() {
                    set_col!(XX_inv, Elem::Field(F::one() / get_col!(XX).into_fe()));
                }

                self.proc
                    .set_col("main::instr_branch_if_diff_equal_param_l", label);

                Vec::new()
            }
            "skip_if_equal" => {
                let read_reg1 = args[0].u();
                let read_reg2 = args[1].u();
                let val1 = self.reg_read(0, read_reg1, 0);
                let val2 = self.reg_read(1, read_reg2, 1);
                let offset = args[2];
                let cond = args[3];
                let val: Elem<F> = val1.sub(&val2).add(&offset);

                if val.is_zero() {
                    let pc = self.proc.get_pc().s();
                    self.proc.set_pc((pc + cond.s() + 1).into());
                }

                set_col!(tmp1_col, val1);
                set_col!(tmp2_col, val2);
                set_col!(XX, val);
                set_col!(XXIsZero, Elem::from_bool_as_fe(get_col!(XX).is_zero()));
                if !get_col!(XX).is_zero() {
                    set_col!(XX_inv, Elem::Field(F::one() / get_col!(XX).into_fe()));
                }

                Vec::new()
            }
            "branch_if_diff_greater_than" => {
                let read_reg1 = args[0].u();
                let read_reg2 = args[1].u();
                // We can't call u() because input registers may have come from
                // a call to `to_signed`, which stores a signed integer.
                let val1 = self.reg_read(0, read_reg1, 0);
                let val2 = self.reg_read(1, read_reg2, 1);
                let offset = args[2];
                let val: Elem<F> = val1.sub(&val2).sub(&offset);
                let label = args[3];

                if val.bin() > 0 {
                    self.proc.set_pc(label);
                }

                set_col!(tmp1_col, val1);
                set_col!(tmp2_col, val2);

                self.proc
                    .set_col("main::instr_branch_if_diff_greater_than_param_l", label);

                let p = Elem::from_u32_as_fe(u32::MAX);
                let val_p = val.add(&p);

                let v = val_p.as_i64_from_lower_bytes();
                let (b1, b2, b3, b4, _sign) = decompose_lower32(v);
                set_col!(X_b1, Elem::from_u32_as_fe(b1.into()));
                set_col!(X_b2, Elem::from_u32_as_fe(b2.into()));
                set_col!(X_b3, Elem::from_u32_as_fe(b3.into()));
                set_col!(X_b4, Elem::from_u32_as_fe(b4.into()));
                set_col!(
                    wrap_bit,
                    if val.bin() > 0 {
                        Elem::Field(F::one())
                    } else {
                        Elem::Field(F::zero())
                    }
                );

                Vec::new()
            }
            "is_diff_greater_than" => {
                let read_reg1 = args[0].u();
                let read_reg2 = args[1].u();
                let val1 = self.reg_read(0, read_reg1, 0);
                let val2 = self.reg_read(1, read_reg2, 1);

                let offset = args[2];
                let write_reg = args[3].u();
                let val = val1.sub(&val2).sub(&offset);

                let r = if val.bin() > 0 { 1 } else { 0 };
                self.reg_write(2, write_reg, r.into(), 3);

                set_col!(tmp1_col, val1);
                set_col!(tmp2_col, val2);

                let p = Elem::from_u32_as_fe(u32::MAX);
                let val = val.add(&p);
                let v = val.as_i64_from_lower_bytes();
                let (b1, b2, b3, b4, _sign) = decompose_lower32(v);
                set_col!(X_b1, Elem::from_u32_as_fe(b1.into()));
                set_col!(X_b2, Elem::from_u32_as_fe(b2.into()));
                set_col!(X_b3, Elem::from_u32_as_fe(b3.into()));
                set_col!(X_b4, Elem::from_u32_as_fe(b4.into()));
                set_col!(wrap_bit, Elem::from_u32_as_fe(r));

                Vec::new()
            }
            "is_equal_zero" => {
                let read_reg = args[0].u();
                let val = self.reg_read(0, read_reg, 0);
                let write_reg = args[1].u();

                let r = if val.is_zero() { 1 } else { 0 };
                self.reg_write(2, write_reg, r.into(), 3);

                set_col!(tmp1_col, val);
                set_col!(XX, val);
                set_col!(XXIsZero, Elem::from_bool_as_fe(get_col!(XX).is_zero()));
                if !get_col!(XX).is_zero() {
                    set_col!(XX_inv, Elem::Field(F::one() / get_col!(XX).into_fe()));
                }

                Vec::new()
            }
            "is_not_equal" => {
                let read_reg1 = args[0].u();
                let read_reg2 = args[1].u();
                let val1 = self.reg_read(0, read_reg1, 0);
                let val2 = self.reg_read(1, read_reg2, 1);
                let write_reg = args[2].u();
                let val: Elem<F> = (val1.bin() - val2.bin()).into();

                let r = if !val.is_zero() { 1 } else { 0 };
                self.reg_write(2, write_reg, r.into(), 3);

                set_col!(tmp1_col, val1);
                set_col!(tmp2_col, val2);
                set_col!(tmp3_col, Elem::from_u32_as_fe(r));
                set_col!(XX, val);
                set_col!(XXIsZero, Elem::from_bool_as_fe(get_col!(XX).is_zero()));
                if !get_col!(XX).is_zero() {
                    set_col!(XX_inv, Elem::Field(F::one() / get_col!(XX).into_fe()));
                }

                Vec::new()
            }
            "add_wrap" => {
                let read_reg1 = args[0].u();
                let read_reg2 = args[1].u();
                let val1 = self.reg_read(0, read_reg1, 0);
                let val2 = self.reg_read(1, read_reg2, 1);
                let offset = args[2];
                let write_reg = args[3].u();

                let val = val1.add(&val2).add(&offset);
                // don't use .u() here: we are deliberately discarding the
                // higher bits
                let r = val.bin() as u32;
                self.reg_write(2, write_reg, r.into(), 3);

                set_col!(tmp1_col, val1);
                set_col!(tmp2_col, val2);
                set_col!(tmp3_col, Elem::from_u32_as_fe(r));

                let v = val.as_i64_from_lower_bytes();
                let (b1, b2, b3, b4, _sign) = decompose_lower32(v);
                set_col!(X_b1, Elem::from_u32_as_fe(b1.into()));
                set_col!(X_b2, Elem::from_u32_as_fe(b2.into()));
                set_col!(X_b3, Elem::from_u32_as_fe(b3.into()));
                set_col!(X_b4, Elem::from_u32_as_fe(b4.into()));
                set_col!(
                    wrap_bit,
                    if v > 0xffffffff {
                        Elem::Field(F::one())
                    } else {
                        Elem::Field(F::zero())
                    }
                );

                Vec::new()
            }
            "wrap16" => {
                let read_reg = args[0].u();
                let val = self.reg_read(0, read_reg, 0);
                let factor = args[1].bin();
                let write_reg = args[2].u();
                let val_offset: Elem<F> = (val.bin() * factor).into();

                // don't use .u() here: we are deliberately discarding the
                // higher bits
                let r = val_offset.bin() as u32;
                self.reg_write(3, write_reg, r.into(), 3);

                set_col!(tmp1_col, val);
                set_col!(tmp3_col, Elem::from_u32_as_fe(r));

                let v = get_col!(tmp3_col).as_i64_from_lower_bytes();
                let (b1, b2, b3, b4, _sign) = decompose_lower32(v);
                set_col!(X_b1, Elem::from_u32_as_fe(b1.into()));
                set_col!(X_b2, Elem::from_u32_as_fe(b2.into()));
                set_col!(X_b3, Elem::from_u32_as_fe(b3.into()));
                set_col!(X_b4, Elem::from_u32_as_fe(b4.into()));

                let (b5, b6, _b7, _b8, _sign) = decompose_lower32(val_offset.bin() >> 32);
                set_col!(Y_b5, Elem::from_u32_as_fe(b5.into()));
                set_col!(Y_b6, Elem::from_u32_as_fe(b6.into()));

                Vec::new()
            }
            "sub_wrap_with_offset" => {
                let read_reg1 = args[0].u();
                let read_reg2 = args[1].u();
                let val1 = self.reg_read(0, read_reg1, 0);
                let val2 = self.reg_read(1, read_reg2, 1);
                let offset = args[2];
                let write_reg = args[3].u();
                let val = val1.sub(&val2).add(&offset);

                let r_i64: i64 = val.bin() + 0x100000000;
                let r = r_i64 as u32;
                self.reg_write(2, write_reg, r.into(), 3);

                set_col!(tmp1_col, val1);
                set_col!(tmp2_col, val2);
                set_col!(tmp3_col, Elem::from_u32_as_fe(r));

                let (b1, b2, b3, b4, _sign) = decompose_lower32(r_i64);
                set_col!(X_b1, Elem::from_u32_as_fe(b1.into()));
                set_col!(X_b2, Elem::from_u32_as_fe(b2.into()));
                set_col!(X_b3, Elem::from_u32_as_fe(b3.into()));
                set_col!(X_b4, Elem::from_u32_as_fe(b4.into()));
                set_col!(
                    wrap_bit,
                    if r_i64 > 0xffffffff {
                        Elem::Field(F::one())
                    } else {
                        Elem::Field(F::zero())
                    }
                );

                Vec::new()
            }
            "sign_extend_byte" => {
                let read_reg = args[0].u();
                let val = self.reg_read(0, read_reg, 0);
                let write_reg = args[1].u();

                // Sign extend the byte
                let byte_val = (val.u() as u8) as i8;
                let extended_val = byte_val as i32 as u32;
                self.reg_write(3, write_reg, extended_val.into(), 3);

                set_col!(tmp1_col, val);
                set_col!(tmp3_col, Elem::from_u32_as_fe(extended_val));

                let v = get_col!(tmp1_col).as_i64_from_lower_bytes();
                let (b1, b2, b3, b4, _sign) = decompose_lower32(v);
                // first 7bits
                set_col!(Y_7bit, Elem::from_u32_as_fe((b1 & 0x7f).into()));
                // no X_b1 needed here
                set_col!(X_b2, Elem::from_u32_as_fe(b2.into()));
                set_col!(X_b3, Elem::from_u32_as_fe(b3.into()));
                set_col!(X_b4, Elem::from_u32_as_fe(b4.into()));
                set_col!(
                    wrap_bit,
                    if byte_val < 0 {
                        Elem::Field(F::one())
                    } else {
                        Elem::Field(F::zero())
                    }
                );

                Vec::new()
            }
            "sign_extend_16_bits" => {
                let read_reg = args[0].u();
                let val = self.reg_read(0, read_reg, 0);
                let write_reg = args[1].u();

                // Perform sign extension on the 16-bit value
                let sign_bit = (val.u() & 0x8000) != 0;
                let extended_val = if sign_bit {
                    val.u() | 0xFFFF0000
                } else {
                    val.u() & 0x0000FFFF
                };
                self.reg_write(3, write_reg, extended_val.into(), 3);

                set_col!(tmp1_col, val);
                set_col!(tmp3_col, Elem::from_u32_as_fe(extended_val));

                let v = get_col!(tmp1_col).as_i64_from_lower_bytes();
                let (b1, b2, b3, b4, _) = decompose_lower32(v);

                set_col!(X_b1, Elem::from_u32_as_fe(b1.into()));
                set_col!(X_b3, Elem::from_u32_as_fe(b3.into()));
                set_col!(X_b4, Elem::from_u32_as_fe(b4.into()));
                set_col!(Y_7bit, Elem::from_u32_as_fe((b2 & 0x7f).into()));
                set_col!(Y_15bit, Elem::from_u32_as_fe(val.u() & 0x7fff));
                set_col!(
                    wrap_bit,
                    if sign_bit {
                        Elem::Field(F::one())
                    } else {
                        Elem::Field(F::zero())
                    }
                );

                Vec::new()
            }
            "to_signed" => {
                let read_reg = args[0].u();
                let val = self.reg_read(0, read_reg, 0);
                let write_reg = args[1].u();
                let r = val.u() as i32;

                self.reg_write(1, write_reg, r.into(), 3);

                set_col!(tmp1_col, val);
                set_col!(tmp3_col, Elem::from_i32_as_fe(r));

                let (b1, b2, b3, b4, _sign) = decompose_lower32(val.u().into());
                set_col!(X_b1, Elem::from_u32_as_fe(b1.into()));
                set_col!(X_b2, Elem::from_u32_as_fe(b2.into()));
                set_col!(X_b3, Elem::from_u32_as_fe(b3.into()));
                set_col!(
                    wrap_bit,
                    if b4 & 0x80 != 0 {
                        Elem::Field(F::one())
                    } else {
                        Elem::Field(F::zero())
                    }
                );

                set_col!(Y_7bit, Elem::from_u32_as_fe(b4 as u32 & 0x7f));

                Vec::new()
            }
            "fail" => {
                // TODO: handle it better
                panic!("reached a fail instruction")
            }
            "divremu" => {
                let read_reg1 = args[0].u();
                let read_reg2 = args[1].u();
                let val1 = self.reg_read(0, read_reg1, 0);
                let val2 = self.reg_read(1, read_reg2, 1);
                let write_reg1 = args[2].u();
                let write_reg2 = args[3].u();

                let y = val1.u();
                let x = val2.u();
                let div;
                let rem;
                if x != 0 {
                    div = y / x;
                    rem = y % x;
                } else {
                    div = 0xffffffff;
                    rem = y;
                }

                self.reg_write(2, write_reg1, div.into(), 3);
                self.reg_write(3, write_reg2, rem.into(), 4);

                set_col!(tmp1_col, val1);
                set_col!(tmp2_col, val2);
                set_col!(tmp3_col, Elem::from_u32_as_fe(div));
                set_col!(tmp4_col, Elem::from_u32_as_fe(rem));
                set_col!(XX, val2);
                set_col!(XXIsZero, Elem::from_bool_as_fe(get_col!(XX).is_zero()));
                if !get_col!(XX).is_zero() {
                    set_col!(XX_inv, Elem::Field(F::one() / get_col!(XX).into_fe()));
                }

                let v = get_col!(tmp3_col).as_i64_from_lower_bytes();
                let (b1, b2, b3, b4, _sign) = decompose_lower32(v);
                set_col!(X_b1, Elem::from_u32_as_fe(b1.into()));
                set_col!(X_b2, Elem::from_u32_as_fe(b2.into()));
                set_col!(X_b3, Elem::from_u32_as_fe(b3.into()));
                set_col!(X_b4, Elem::from_u32_as_fe(b4.into()));

                let (rem_b1, rem_b2, rem_b3, rem_b4, _sign) = decompose_lower32(rem.into());
                set_col!(REM_b1, Elem::from_u32_as_fe(rem_b1.into()));
                set_col!(REM_b2, Elem::from_u32_as_fe(rem_b2.into()));
                set_col!(REM_b3, Elem::from_u32_as_fe(rem_b3.into()));
                set_col!(REM_b4, Elem::from_u32_as_fe(rem_b4.into()));

                if x > 0 {
                    let diff = x - rem - 1;
                    let (b5, b6, b7, b8, _sign) = decompose_lower32(diff.into());
                    set_col!(Y_b5, Elem::from_u32_as_fe(b5.into()));
                    set_col!(Y_b6, Elem::from_u32_as_fe(b6.into()));
                    set_col!(Y_b7, Elem::from_u32_as_fe(b7.into()));
                    set_col!(Y_b8, Elem::from_u32_as_fe(b8.into()));
                }

                Vec::new()
            }
            "mul" => {
                let read_reg1 = args[0].u();
                let read_reg2 = args[1].u();
                let val1 = self.reg_read(0, read_reg1, 0);
                let val2 = self.reg_read(1, read_reg2, 1);
                let write_reg1 = args[2].u();
                let write_reg2 = args[3].u();

                let r = val1.u() as u64 * val2.u() as u64;
                let lo = r as u32;
                let hi = (r >> 32) as u32;

                self.reg_write(2, write_reg1, lo.into(), 3);
                self.reg_write(3, write_reg2, hi.into(), 4);

                set_col!(tmp1_col, val1);
                set_col!(tmp2_col, val2);
                set_col!(tmp3_col, Elem::from_u32_as_fe(lo));
                set_col!(tmp4_col, Elem::from_u32_as_fe(hi));

                if let ExecMode::Trace = self.mode {
                    self.proc
                        .submachines
                        .get_mut("split_gl")
                        .unwrap()
                        .add_operation(
                            "split_gl",
                            &[("output_low", lo.into()), ("output_high", hi.into())],
                        );
                }

                Vec::new()
            }
            "and" | "or" | "xor" => {
                let read_reg1 = args[0].u();
                let read_reg2 = args[1].u();
                let val1 = self.reg_read(0, read_reg1, 0);
                let val2 = self.reg_read(1, read_reg2, 1);
                let offset = args[2].bin();
                let write_reg = args[3].u();
                let val2_offset: Elem<F> = (val2.bin() + offset).into();

                set_col!(tmp1_col, val1);
                set_col!(tmp2_col, val2);

                let r = match name {
                    "and" => val1.u() & val2_offset.u(),
                    "or" => val1.u() | val2_offset.u(),
                    "xor" => val1.u() ^ val2_offset.u(),
                    _ => unreachable!(),
                };

                if let ExecMode::Trace = self.mode {
                    self.proc
                        .submachines
                        .get_mut("binary")
                        .unwrap()
                        .add_operation(name, &[("A", val1), ("B", val2_offset), ("C", r.into())]);
                }

                self.reg_write(3, write_reg, r.into(), 3);

                set_col!(tmp3_col, Elem::from_u32_as_fe(r));

                Vec::new()
            }
            "shl" | "shr" => {
                let read_reg1 = args[0].u();
                let read_reg2 = args[1].u();
                let val1 = self.reg_read(0, read_reg1, 0);
                let val2 = self.reg_read(1, read_reg2, 1);
                let offset = args[2].bin();
                let write_reg = args[3].u();
                let val2_offset: Elem<F> = (val2.bin() + offset).into();

                let r = match name {
                    "shl" => val1.u() << val2_offset.u(),
                    "shr" => val1.u() >> val2_offset.u(),
                    _ => unreachable!(),
                };

                self.reg_write(3, write_reg, r.into(), 3);

                set_col!(tmp1_col, val1);
                set_col!(tmp2_col, val2);
                set_col!(tmp3_col, Elem::from_u32_as_fe(r));

                if let ExecMode::Trace = self.mode {
                    self.proc
                        .submachines
                        .get_mut("shift")
                        .unwrap()
                        .add_operation(name, &[("A", val1), ("B", val2_offset), ("C", r.into())]);
                }

                Vec::new()
            }
            "split_gl" => {
                let read_reg = args[0].u();
                let val1 = self.reg_read(0, read_reg, 0);
                let write_reg1 = args[1].u();
                let write_reg2 = args[2].u();

                let value = val1.into_fe().to_integer();
                // This instruction is only for Goldilocks, so the value must
                // fit into a u64.
                let value = value.try_into_u64().unwrap();
                let lo = (value & 0xffffffff) as u32;
                let hi = (value >> 32) as u32;

                self.reg_write(2, write_reg1, lo.into(), 3);
                self.reg_write(3, write_reg2, hi.into(), 4);

                set_col!(tmp1_col, val1);
                set_col!(tmp3_col, Elem::from_u32_as_fe(lo));
                set_col!(tmp4_col, Elem::from_u32_as_fe(hi));

                if let ExecMode::Trace = self.mode {
                    self.proc
                        .submachines
                        .get_mut("split_gl")
                        .unwrap()
                        .add_operation(
                            "split_gl",
                            &[("output_low", lo.into()), ("output_high", hi.into())],
                        );
                }

                Vec::new()
            }
            "poseidon_gl" => {
                let input_ptr = self.proc.get_reg_mem(args[0].u()).u();
                assert_eq!(input_ptr % 4, 0);

                let inputs = (0..24)
                    .map(|i| self.proc.get_mem(input_ptr + i * 4))
                    .chunks(2)
                    .into_iter()
                    .map(|mut chunk| {
                        let low = chunk.next().unwrap() as u64;
                        let high = chunk.next().unwrap() as u64;
                        F::from((high << 32) | low)
                    })
                    .collect::<Vec<_>>();

                let result = poseidon_gl::poseidon_gl(&inputs)
                    .into_iter()
                    .flat_map(|v| {
                        let v = v.to_integer().try_into_u64().unwrap();
                        vec![(v & 0xffffffff) as u32, (v >> 32) as u32]
                    })
                    .collect::<Vec<_>>();

                let output_ptr = self.proc.get_reg_mem(args[1].u()).u();
                assert_eq!(output_ptr % 4, 0);
                result.iter().enumerate().for_each(|(i, &v)| {
                    self.proc.set_mem(output_ptr + i as u32 * 4, v);
                });

                vec![]
            }
            "poseidon2_gl" => {
                let input_ptr = self.proc.get_reg_mem(args[0].u()).u();
                assert_eq!(input_ptr % 4, 0);

                let inputs: [u64; 8] = (0..16)
                    .map(|i| self.proc.get_mem(input_ptr + i * 4))
                    .chunks(2)
                    .into_iter()
                    .map(|mut chunk| {
                        let low = chunk.next().unwrap() as u64;
                        let high = chunk.next().unwrap() as u64;
                        (high << 32) | low
                    })
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap();

                let result = poseidon2_gl::poseidon2_gl(&inputs)
                    .into_iter()
                    .flat_map(|v| vec![(v & 0xffffffff) as u32, (v >> 32) as u32]);

                let output_ptr = self.proc.get_reg_mem(args[1].u()).u();
                assert_eq!(output_ptr % 4, 0);
                result.enumerate().for_each(|(i, v)| {
                    self.proc.set_mem(output_ptr + i as u32 * 4, v);
                });

                vec![]
            }
            "affine_256" => {
                assert!(args.is_empty());
                // take input from registers
                let x1 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i)).into_fe())
                    .collect::<Vec<_>>();
                let y1 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 8)).into_fe())
                    .collect::<Vec<_>>();
                let x2 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 16)).into_fe())
                    .collect::<Vec<_>>();
                let result = arith::affine_256(&x1, &y1, &x2);
                // store result in registers
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i), Elem::Field(result.0[i]))
                });
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i + 8), Elem::Field(result.1[i]))
                });

                vec![]
            }
            "mod_256" => {
                assert!(args.is_empty());
                // take input from registers
                let y2 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i)).into_fe())
                    .collect::<Vec<_>>();
                let y3 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 8)).into_fe())
                    .collect::<Vec<_>>();
                let x1 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 16)).into_fe())
                    .collect::<Vec<_>>();
                let result = arith::mod_256(&y2, &y3, &x1);
                // store result in registers
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i), Elem::Field(result[i]))
                });
                vec![]
            }
            "ec_add" => {
                assert!(args.is_empty());
                // take input from registers
                let x1 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i)).into_fe())
                    .collect::<Vec<_>>();
                let y1 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 8)).into_fe())
                    .collect::<Vec<_>>();
                let x2 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 16)).into_fe())
                    .collect::<Vec<_>>();
                let y2 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 24)).into_fe())
                    .collect::<Vec<_>>();
                let result = arith::ec_add(&x1, &y1, &x2, &y2);
                // store result in registers
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i), Elem::Field(result.0[i]))
                });
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i + 8), Elem::Field(result.1[i]))
                });

                vec![]
            }
            "ec_double" => {
                assert!(args.is_empty());
                // take input from registers
                let x = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i)).into_fe())
                    .collect::<Vec<_>>();
                let y = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 8)).into_fe())
                    .collect::<Vec<_>>();
                let result = arith::ec_double(&x, &y);
                // store result in registers
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i), Elem::Field(result.0[i]))
                });
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i + 8), Elem::Field(result.1[i]))
                });

                vec![]
            }
            "commit_public" => {
                let idx = self.proc.get_reg_mem(args[0].u());
                let limb = self.proc.get_reg_mem(args[1].u());
                log::debug!("Committing public: idx={idx}, limb={limb}");
                vec![]
            }
            instr => {
                panic!("unknown instruction: {instr}");
            }
        };

        r
    }

    fn eval_expression(&mut self, expression: &Expression) -> Vec<Elem<F>> {
        match expression {
            Expression::Reference(_, r) => {
                // an identifier looks like this:
                let name = r.try_to_identifier().unwrap();

                // labels share the identifier space with registers:
                // try one, then the other
                let val = self
                    .label_map
                    .get(name.as_str())
                    .cloned()
                    .unwrap_or_else(|| self.proc.get_reg(name.as_str()));
                vec![val]
            }
            Expression::PublicReference(_, _) => todo!(),
            Expression::Number(_, Number { value: n, .. }) => {
                let unsigned: u32 = n
                    .try_into()
                    .unwrap_or_else(|_| panic!("Value does not fit in 32 bits."));

                vec![unsigned.into()]
            }
            Expression::String(_, _) => todo!(),
            Expression::Tuple(_, _) => todo!(),
            Expression::LambdaExpression(_, _) => todo!(),
            Expression::ArrayLiteral(_, _) => todo!(),
            Expression::BinaryOperation(
                _,
                BinaryOperation {
                    left: l,
                    op,
                    right: r,
                },
            ) => {
                let l = &self.eval_expression(l)[0];
                let r = &self.eval_expression(r)[0];

                let result = match (l, r) {
                    (Elem::Binary(l), Elem::Binary(r)) => match op {
                        powdr_ast::parsed::BinaryOperator::Add => Elem::Binary(l + r),
                        powdr_ast::parsed::BinaryOperator::Sub => Elem::Binary(l - r),
                        powdr_ast::parsed::BinaryOperator::Mul => match l.checked_mul(*r) {
                            // Multiplication is a special case as the input for
                            // poseidon_gl requires field multiplication. So,
                            // if native multiplication overflows, we use field
                            // multiplication.
                            //
                            // TODO: support types in the zkVM specification, so
                            // that we don't have to guess which kind of
                            // arithmetic we have to use.
                            Some(v) => Elem::Binary(v),
                            None => {
                                let l = F::from(*l);
                                let r = F::from(*r);
                                Elem::Field(l * r)
                            }
                        },
                        powdr_ast::parsed::BinaryOperator::Div => Elem::Binary(l / r),
                        powdr_ast::parsed::BinaryOperator::Mod => Elem::Binary(l % r),
                        powdr_ast::parsed::BinaryOperator::Pow => {
                            Elem::Binary(l.pow(u32::try_from(*r).unwrap()))
                        }
                        _ => todo!(),
                    },
                    (Elem::Field(l), Elem::Field(r)) => {
                        let result = match op {
                            // We need to subtract field elements in the bootloader:
                            powdr_ast::parsed::BinaryOperator::Sub => *l - *r,
                            _ => todo!(),
                        };
                        Elem::Field(result)
                    }
                    (Elem::Binary(l), Elem::Field(r)) => {
                        // We need to add a field element to a binary when calling poseidon_gl:
                        let result = match op {
                            powdr_ast::parsed::BinaryOperator::Add => F::from(*l) + *r,
                            _ => todo!(),
                        };
                        Elem::Field(result)
                    }
                    _ => panic!("tried to operate a binary value with a field value"),
                };

                vec![result]
            }
            Expression::UnaryOperation(_, UnaryOperation { op, expr: arg }) => {
                let arg = self.eval_expression(arg)[0].bin();
                let result = match op {
                    powdr_ast::parsed::UnaryOperator::Minus => -arg,
                    powdr_ast::parsed::UnaryOperator::LogicalNot => todo!(),
                    powdr_ast::parsed::UnaryOperator::Next => unreachable!(),
                };

                vec![Elem::Binary(result)]
            }
            Expression::FunctionCall(
                _,
                FunctionCall {
                    function,
                    arguments,
                },
            ) => match function.as_ref() {
                Expression::Reference(_, f) if f.to_string() == "std::prover::eval" => {
                    self.eval_expression(&arguments[0])
                }
                Expression::Reference(_, f) if f.to_string() == "std::convert::int" => {
                    // whatever. we don't need to convert anything
                    self.eval_expression(&arguments[0])
                }
                Expression::Reference(_, f) => {
                    self.exec_instruction(f.try_to_identifier().unwrap(), arguments)
                }
                _ => {
                    unimplemented!(
                        "Function call not implemented: {function}{}",
                        arguments.iter().format(", ")
                    )
                }
            },
            Expression::FreeInput(_, expr) => {
                let Expression::FunctionCall(
                    _,
                    FunctionCall {
                        function,
                        arguments,
                    },
                ) = expr.as_ref()
                else {
                    panic!("Free input does not match pattern: {expr}");
                };
                let Expression::Reference(_, f) = function.as_ref() else {
                    panic!("Free input does not match pattern: {expr}");
                };
                let variant = f
                    .to_string()
                    .strip_prefix("std::prelude::Query::")
                    .unwrap_or_else(|| panic!("Free input does not match pattern: {expr}"))
                    .to_string();
                let values = arguments
                    .iter()
                    .map(|arg| self.eval_expression(arg)[0].to_string())
                    .collect::<Vec<_>>();
                let query = format!("{variant}({})", values.join(","));
                match (self.inputs)(&query).unwrap() {
                    Some(val) => {
                        let e = Elem::try_from_fe_as_bin(&val)
                            .expect("field value does not fit into u32 or i32");
                        vec![e]
                    }
                    None => {
                        panic!("unknown query command: {query}");
                    }
                }
            }
            Expression::MatchExpression(_, _) => todo!(),
            Expression::IfExpression(_, _) => panic!(),
            Expression::BlockExpression(_, _) => panic!(),
            Expression::IndexAccess(_, _) => todo!(),
            Expression::StructExpression(_, _) => todo!(),
        }
    }
}

pub type FixedColumns<F> = Arc<Vec<(String, VariablySizedColumn<F>)>>;

/// Result of the execution.
/// If executing in ExecMode::Fast, trace and memory fields will be empty.
pub struct Execution<F: FieldElement> {
    /// number of rows used by the execution (not of the full main columns, which are extended to a power-of-two)
    pub trace_len: usize,
    /// witness columns
    pub trace: HashMap<String, Vec<F>>,
    /// final memory state
    pub memory: MemoryState,
    /// sequence of memory accesses
    pub memory_accesses: Vec<MemOperation>,
    /// final register memory state
    pub register_memory: RegisterMemoryState<F>,
}

#[derive(Clone, Copy)]
enum ExecMode {
    Fast,
    Trace,
}

/// Execute a Powdr/RISCV assembly program, without generating a witness.
/// Returns the execution trace length.
pub fn execute_fast<F: FieldElement>(
    asm: &AnalysisASMFile,
    initial_memory: MemoryState,
    inputs: &Callback<F>,
    bootloader_inputs: &[F],
    profiling: Option<ProfilerOptions>,
) -> usize {
    log::info!("Executing...");
    execute_inner(
        asm,
        None,
        None,
        initial_memory,
        inputs,
        bootloader_inputs,
        usize::MAX,
        ExecMode::Fast,
        profiling,
    )
    .trace_len
}

/// Execute and generate a valid witness for a Powdr/RISCV assembly program.
#[allow(clippy::too_many_arguments)]
pub fn execute<F: FieldElement>(
    asm: &AnalysisASMFile,
    opt_pil: &Analyzed<F>,
    fixed: FixedColumns<F>,
    initial_memory: MemoryState,
    inputs: &Callback<F>,
    bootloader_inputs: &[F],
    max_steps_to_execute: Option<usize>,
    profiling: Option<ProfilerOptions>,
) -> Execution<F> {
    log::info!("Executing...");
    execute_inner(
        asm,
        Some(opt_pil),
        Some(fixed),
        initial_memory,
        inputs,
        bootloader_inputs,
        max_steps_to_execute.unwrap_or(usize::MAX),
        ExecMode::Trace,
        profiling,
    )
}

/// FIXME: copied from `riscv/runtime.rs` instead of adding dependency.
/// Helper function for register names used in submachine instruction params.
fn register_by_idx(idx: usize) -> String {
    format!("xtra{idx}")
}

#[allow(clippy::too_many_arguments)]
fn execute_inner<F: FieldElement>(
    asm: &AnalysisASMFile,
    opt_pil: Option<&Analyzed<F>>,
    fixed: Option<FixedColumns<F>>,
    initial_memory: MemoryState,
    inputs: &Callback<F>,
    bootloader_inputs: &[F],
    max_steps_to_execute: usize,
    mode: ExecMode,
    profiling: Option<ProfilerOptions>,
) -> Execution<F> {
    let main_machine = get_main_machine(asm);

    let PreprocessedMain {
        statements,
        label_map,
        batch_to_line_map,
        debug_files,
        function_starts,
        location_starts,
    } = preprocess_main_function(main_machine);

    let witness_cols: Vec<String> = opt_pil
        .map(|pil| {
            pil.committed_polys_in_source_order()
                .flat_map(|(s, _)| s.array_elements().map(|(name, _)| name))
                .collect()
        })
        .unwrap_or_default();

    // program columns to witness columns
    let program_cols: HashMap<_, _> = if let Some(fixed) = &fixed {
        fixed
            .iter()
            .filter_map(|(name, _col)| {
                if !name.starts_with("main__rom::p_") {
                    return None;
                }
                let wit_name = format!("main::{}", name.strip_prefix("main__rom::p_").unwrap());
                if !witness_cols.contains(&wit_name) {
                    return None;
                }
                Some((name.clone(), wit_name))
            })
            .collect()
    } else {
        Default::default()
    };

    let proc = match TraceBuilder::<'_, F>::new(
        main_machine,
        opt_pil,
        witness_cols,
        initial_memory,
        &batch_to_line_map,
        max_steps_to_execute,
        mode,
    ) {
        Ok(proc) => proc,
        Err(ret) => return *ret,
    };

    let bootloader_inputs = bootloader_inputs
        .iter()
        .map(|v| Elem::try_from_fe_as_bin(v).unwrap_or(Elem::Field(*v)))
        .collect();

    let mut e = Executor {
        proc,
        label_map,
        inputs,
        bootloader_inputs,
        fixed: fixed.unwrap_or_default(),
        program_cols,
        step: 0,
        mode,
    };

    e.init();

    let mut profiler =
        profiling.map(|opt| Profiler::new(opt, &debug_files[..], function_starts, location_starts));

    let mut curr_pc = 0u32;
    loop {
        let stm = statements[curr_pc as usize];

        log::trace!("l {curr_pc}: {stm}",);

        e.step += 4;

        match stm {
            FunctionStatement::Assignment(a) => {
                e.proc.push_row();
                let pc = e.proc.get_pc().u();
                e.set_program_columns(pc);
                if let Some(p) = &mut profiler {
                    p.add_instruction_cost(e.proc.get_pc().u() as usize);
                }

                let results = e.eval_expression(a.rhs.as_ref());
                assert_eq!(a.lhs_with_reg.len(), results.len());

                let asgn_reg = a.lhs_with_reg[0].1.clone();
                if let AssignmentRegister::Register(x) = asgn_reg {
                    assert_eq!(x, "X"); // we currently only assign through X
                    let x_const = e.proc.get_col("main::X_const");

                    match a.rhs.as_ref() {
                        Expression::FreeInput(_, _expr) => {
                            // we currently only use X for free inputs
                            assert!(x_const.is_zero());
                            e.proc.set_col("main::X", results[0]);
                            e.proc.set_col("main::X_free_value", results[0]);
                        }
                        _ => {
                            // We're assinging a value or the result of an instruction.
                            // Currently, only X used as the assignment register in this case.
                            let x = results[0];
                            e.proc.set_col("main::X", x);

                            let x_read_free = e.proc.get_col("main::X_read_free");

                            // We need to solve for X_free_value:
                            // X = X_const + X_read_free * X_free_value
                            // X - X_const = X_read_free * X_free_value
                            // X_free_value = (X - X_const) / X_read_free
                            let x_free_value = if x_read_free.is_zero() {
                                Elem::Field(F::zero())
                            } else {
                                x.sub(&x_const).div(&x_read_free)
                            };
                            e.proc.set_col("main::X_free_value", x_free_value);
                        }
                    }
                } else {
                    panic!("should be an assignment register");
                }

                for ((dest, _), val) in a.lhs_with_reg.iter().zip(results) {
                    e.proc.set_reg(dest, val);
                }
            }
            FunctionStatement::Instruction(i) => {
                e.proc.push_row();
                let pc = e.proc.get_pc().u();
                e.set_program_columns(pc);

                if let Some(p) = &mut profiler {
                    p.add_instruction_cost(e.proc.get_pc().u() as usize);
                }

                if ["jump", "jump_dyn"].contains(&i.instruction.as_str()) {
                    let pc_before = e.proc.get_pc().u();

                    e.exec_instruction(&i.instruction, &i.inputs);

                    // we can't use `get_pc/get_reg`, as its value is only updated when moving to the next row
                    let pc_after = e.proc.get_next_pc().u();

                    let target_reg = e.eval_expression(&i.inputs[1]);
                    assert_eq!(target_reg.len(), 1);
                    let target_reg = target_reg[0].u();

                    if let Some(p) = &mut profiler {
                        let pc_return = e.proc.get_reg_mem(target_reg).u();
                        // in the generated powdr asm, not writing to `x1` means the returning pc is ignored
                        if target_reg != 1 {
                            p.jump(pc_after as usize);
                        } else {
                            p.jump_and_link(
                                pc_before as usize,
                                pc_after as usize,
                                pc_return as usize,
                            );
                        }
                    }
                } else {
                    e.exec_instruction(&i.instruction, &i.inputs);
                }
            }
            FunctionStatement::Return(_) => {
                e.proc.push_row();
                let pc = e.proc.get_pc().u();
                e.set_program_columns(pc);
                break;
            }
            FunctionStatement::DebugDirective(dd) => {
                e.step -= 4;
                match &dd.directive {
                    DebugDirective::Loc(file, line, column) => {
                        let (dir, file) = debug_files[file - 1];
                        log::trace!("Executed {dir}/{file}:{line}:{column}");
                    }
                    DebugDirective::OriginalInstruction(insn) => {
                        log::trace!("  {insn}");
                    }
                    DebugDirective::File(_, _, _) => unreachable!(),
                };
            }
            FunctionStatement::Label(_) => {
                unreachable!()
            }
        };

        curr_pc = match e.proc.advance() {
            Some(pc) => {
                // We set pc_update=PC here, after the PC has been updated but before "pushing" the next row
                e.proc.set_col("main::pc_update", e.proc.get_pc());
                pc
            }
            None => break,
        };
    }

    if let Some(mut p) = profiler {
        p.finish();
    }

    if let ExecMode::Trace = mode {
        let sink_id = e.sink_id();

        // reset
        e.proc.set_col("main::pc_update", 0.into());
        e.proc.set_pc(0.into());
        assert!(e.proc.advance().is_none());
        e.proc.push_row();
        e.set_program_columns(0);
        e.proc.set_col("main::_operation_id", sink_id.into());

        // jump_to_operation
        e.proc.set_col("main::pc_update", 1.into());
        e.proc.set_pc(1.into());
        e.proc.set_reg("query_arg_1", 0);
        e.proc.set_reg("query_arg_2", 0);
        assert!(e.proc.advance().is_none());
        e.proc.push_row();
        e.set_program_columns(1);
        e.proc.set_col("main::_operation_id", sink_id.into());

        // loop
        e.proc.set_col("main::pc_update", sink_id.into());
        e.proc.set_pc(sink_id.into());
        assert!(e.proc.advance().is_none());
        e.proc.push_row();
        e.set_program_columns(sink_id);
        e.proc.set_col("main::pc_update", sink_id.into());
        e.proc.set_col("main::_operation_id", sink_id.into());
    }

    e.proc.finish(opt_pil)
}

/// Utility function for writing the executor witness CSV file.
///
/// If `all_witness_cols` is given, all columns there will also be output, but have empty values when not present in the executor witness
/// (this is useful for debugging, for easy comparison with auto witgen export CSV file).
pub fn write_executor_csv<F: FieldElement, P: AsRef<Path>>(
    file_path: P,
    executor_witness: &[(String, Vec<F>)],
    all_witness_cols: Option<&[String]>,
) {
    let columns: Vec<_> = if let Some(witness_cols) = all_witness_cols {
        witness_cols
            .iter()
            .map(|name| {
                if let Some((_, values)) = executor_witness.iter().find(|(n, _)| n == name) {
                    (name, values.as_ref())
                } else {
                    (name, [].as_ref())
                }
            })
            .collect()
    } else {
        executor_witness
            .iter()
            .map(|(name, values)| (name, values.as_ref()))
            .collect()
    };

    write_polys_csv_file(
        std::fs::File::create(file_path.as_ref()).unwrap(),
        powdr_number::CsvRenderMode::Hex,
        &columns[..],
    );
}
