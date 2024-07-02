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
    io,
};

use builder::TraceBuilder;

use itertools::Itertools;
use powdr_ast::{
    asm_analysis::{
        AnalysisASMFile, CallableSymbol, FunctionStatement, Item, LabelStatement, Machine,
    },
    parsed::{
        asm::DebugDirective, BinaryOperation, Expression, FunctionCall, Number, UnaryOperation,
    },
};
use powdr_number::{FieldElement, LargeInt};
use powdr_riscv_syscalls::SYSCALL_REGISTERS;
pub use profiler::ProfilerOptions;

pub mod arith;
pub mod poseidon_gl;
mod profiler;

use crate::profiler::Profiler;

/// Initial value of the PC.
///
/// To match the ZK proof witness, the PC must start after some offset used for
/// proof initialization.
///
/// TODO: get this value from some authoritative place
const PC_INITIAL_VAL: usize = 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Elem<F: FieldElement> {
    /// Only the ranges of i32 and u32 are actually valid for a Binary value.
    /// I.e., [-2**31, 2**32).
    Binary(i64),
    Field(F),
}

impl<F: FieldElement> Elem<F> {
    /// Interprets the value of a field as a binary, if it can be represented either as a
    /// u32 or a i32.
    ///
    /// Panics otherwise.
    pub fn new_from_fe_as_bin(value: &F) -> Self {
        if let Some(v) = value.to_integer().try_into_u32() {
            Self::Binary(v as i64)
        } else if let Some(v) = value.try_into_i32() {
            Self::Binary(v as i64)
        } else {
            panic!("Value does not fit in 32 bits.")
        }
    }

    /// Interprets the value of self as a field element.
    pub fn into_fe(&self) -> F {
        match *self {
            Self::Field(f) => f,
            Self::Binary(b) => b.into(),
        }
    }

    pub fn fe(&self) -> F {
        match self {
            Self::Field(f) => *f,
            Self::Binary(_) => panic!(),
        }
    }

    pub fn bin(&self) -> i64 {
        match self {
            Self::Binary(b) => *b,
            Self::Field(_) => panic!(),
        }
    }

    fn bin_mut(&mut self) -> &mut i64 {
        match self {
            Self::Binary(b) => b,
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
    val: Elem<F>,
}

pub struct ExecutionTrace<F: FieldElement> {
    pub reg_map: HashMap<String, u16>,

    /// Values of the registers in the execution trace.
    ///
    /// Each N elements is a row with all registers, where N is the number of
    /// registers.
    pub reg_writes: Vec<RegWrite<F>>,

    /// Writes and reads to memory.
    pub mem_ops: Vec<MemOperation>,

    /// The length of the trace, after applying the reg_writes.
    pub len: usize,
}

impl<F: FieldElement> ExecutionTrace<F> {
    /// Replay the execution and get the register values per trace row.
    pub fn replay(&self) -> TraceReplay<F> {
        TraceReplay {
            trace: self,
            regs: vec![0.into(); self.reg_map.len()],
            pc_idx: self.reg_map["pc"] as usize,
            next_write: 0,
            next_r: 0,
        }
    }
}

pub struct TraceReplay<'a, F: FieldElement> {
    trace: &'a ExecutionTrace<F>,
    regs: Vec<Elem<F>>,
    pc_idx: usize,
    next_write: usize,
    next_r: usize,
}

impl<'a, F: FieldElement> TraceReplay<'a, F> {
    /// Returns the next row's registers value.
    ///
    /// Just like an iterator's next(), but returns the value borrowed from self.
    pub fn next_row(&mut self) -> Option<&[Elem<F>]> {
        if self.next_r == self.trace.len {
            return None;
        }

        // we optimistically increment the PC, if it is a jump or special case,
        // one of the writes will overwrite it
        *self.regs[self.pc_idx].bin_mut() += 1;

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

mod builder {
    use std::{cmp, collections::HashMap};

    use powdr_ast::asm_analysis::{Machine, RegisterTy};
    use powdr_number::FieldElement;

    use crate::{
        Elem, ExecMode, ExecutionTrace, MemOperation, MemOperationKind, MemoryState, RegWrite,
        PC_INITIAL_VAL,
    };

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

        /// Maximum rows we can run before we stop the execution.
        max_rows: usize,

        // index of special case registers to look after:
        x0_idx: u16,
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
            mem: MemoryState,
            batch_to_line_map: &'b [u32],
            max_rows_len: usize,
            mode: ExecMode,
        ) -> Result<Self, Box<(ExecutionTrace<F>, MemoryState)>> {
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

            let mut ret = Self {
                x0_idx: reg_map["x0"],
                pc_idx,
                curr_pc: PC_INITIAL_VAL.into(),
                trace: ExecutionTrace {
                    reg_map,
                    reg_writes,
                    mem_ops: Vec::new(),
                    len: PC_INITIAL_VAL + 1,
                },
                next_statement_line: 1,
                batch_to_line_map,
                max_rows: max_rows_len,
                regs,
                mem,
                mode,
            };

            if ret.has_enough_rows() || ret.set_next_pc().is_none() {
                Err(Box::new(ret.finish()))
            } else {
                Ok(ret)
            }
        }

        /// get current value of PC
        pub(crate) fn get_pc(&self) -> Elem<F> {
            self.curr_pc
        }

        /// get current value of register
        pub(crate) fn get_reg(&self, idx: &str) -> Elem<F> {
            self.get_reg_idx(self.trace.reg_map[idx])
        }

        /// get current value of register by register index instead of name
        fn get_reg_idx(&self, idx: u16) -> Elem<F> {
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
            if idx == self.x0_idx {
                return;
            }
            self.set_reg_idx(idx, value);
        }

        /// raw set next value of register by register index instead of name
        fn set_reg_idx(&mut self, idx: u16, value: Elem<F>) {
            // Record register write in trace.
            if let ExecMode::Trace = self.mode {
                self.trace.reg_writes.push(RegWrite {
                    row: self.trace.len,
                    reg_idx: idx,
                    val: value,
                });
            }

            self.regs[idx as usize] = value;
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

        pub fn finish(self) -> (ExecutionTrace<F>, MemoryState) {
            (self.trace, self.mem)
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
    for (name, m) in program.items.iter() {
        if name.len() == 1 && name.parts().next() == Some("Main") {
            let Item::Machine(m) = m else {
                panic!();
            };
            return m;
        }
    }
    panic!();
}

struct PreprocessedMain<'a, T: FieldElement> {
    /// list of all statements (batches expanded)
    statements: Vec<&'a FunctionStatement>,
    /// label to batch number
    label_map: HashMap<&'a str, Elem<T>>,
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
fn preprocess_main_function<T: FieldElement>(machine: &Machine) -> PreprocessedMain<T> {
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
    bootloader_inputs: &'b [Elem<F>],
    _stdout: io::Stdout,
}

impl<'a, 'b, F: FieldElement> Executor<'a, 'b, F> {
    fn exec_instruction(&mut self, name: &str, args: &[Expression]) -> Vec<Elem<F>> {
        let args = args
            .iter()
            .map(|expr| self.eval_expression(expr)[0])
            .collect::<Vec<_>>();

        match name {
            "mstore" | "mstore_bootloader" => {
                let addr = args[0].bin() as u32;
                assert_eq!(addr % 4, 0);
                self.proc.set_mem(addr, args[1].u());

                Vec::new()
            }
            "mload" => {
                let addr = args[0].bin() as u32;
                let val = self.proc.get_mem(addr & 0xfffffffc);
                let rem = addr % 4;

                vec![val.into(), rem.into()]
            }
            "load_bootloader_input" => {
                let addr = args[0].bin() as usize;
                let val = self.bootloader_inputs[addr];

                vec![val]
            }
            "assert_bootloader_input" => {
                let addr = args[0].bin() as usize;
                let actual_val = self.bootloader_inputs[addr];

                assert_eq!(args[1], actual_val);

                vec![]
            }
            "load_label" => args,
            "jump" | "jump_dyn" => {
                let next_pc = self.proc.get_pc().u() + 1;
                self.proc.set_pc(args[0]);

                vec![next_pc.into()]
            }
            "jump_to_bootloader_input" => {
                let bootloader_input_idx = args[0].bin() as usize;
                let addr = self.bootloader_inputs[bootloader_input_idx];
                self.proc.set_pc(addr);

                Vec::new()
            }
            "branch_if_nonzero" => {
                if !args[0].is_zero() {
                    self.proc.set_pc(args[1]);
                }

                Vec::new()
            }
            "branch_if_zero" => {
                if args[0].is_zero() {
                    self.proc.set_pc(args[1]);
                }

                Vec::new()
            }
            "skip_if_zero" => {
                if args[0].is_zero() {
                    let pc = self.proc.get_pc().s();
                    self.proc.set_pc((pc + args[1].s() + 1).into());
                }

                Vec::new()
            }
            "branch_if_positive" => {
                if args[0].bin() > 0 {
                    self.proc.set_pc(args[1]);
                }

                Vec::new()
            }
            "is_positive" => {
                let r = if args[0].bin() > 0 { 1 } else { 0 };

                vec![r.into()]
            }
            "is_equal_zero" => {
                let r = if args[0].is_zero() { 1 } else { 0 };

                vec![r.into()]
            }
            "is_not_equal_zero" => {
                let r = if !args[0].is_zero() { 1 } else { 0 };

                vec![r.into()]
            }
            "wrap" | "wrap16" => {
                // don't use .u() here: we are deliberately discarding the
                // higher bits
                let r = args[0].bin() as u32;

                vec![r.into()]
            }
            "wrap_signed" => {
                let r = (args[0].bin() + 0x100000000) as u32;

                vec![r.into()]
            }
            "sign_extend_byte" => {
                let r = args[0].u() as i8 as u32;

                vec![r.into()]
            }
            "sign_extend_16_bits" => {
                let r = args[0].u() as i16 as u32;

                vec![r.into()]
            }
            "to_signed" => {
                let r = args[0].u() as i32;

                vec![r.into()]
            }
            "fail" => {
                // TODO: handle it better
                panic!("reached a fail instruction")
            }
            "divremu" => {
                let y = args[0].u();
                let x = args[1].u();
                let div;
                let rem;
                if x != 0 {
                    div = y / x;
                    rem = y % x;
                } else {
                    div = 0xffffffff;
                    rem = y;
                }

                vec![div.into(), rem.into()]
            }
            "mul" => {
                let r = args[0].u() as u64 * args[1].u() as u64;
                let lo = r as u32;
                let hi = (r >> 32) as u32;

                vec![lo.into(), hi.into()]
            }
            "and" => vec![(args[0].u() & args[1].u()).into()],
            "or" => vec![(args[0].u() | args[1].u()).into()],
            "xor" => vec![(args[0].u() ^ args[1].u()).into()],
            "shl" => vec![(args[0].u() << args[1].u()).into()],
            "shr" => vec![(args[0].u() >> args[1].u()).into()],
            "split_gl" => {
                let value = args[0].into_fe().to_integer();
                // This instruction is only for Goldilocks, so the value must
                // fit into a u64.
                let value = value.try_into_u64().unwrap();
                let lo = (value & 0xffffffff) as u32;
                let hi = (value >> 32) as u32;

                vec![lo.into(), hi.into()]
            }
            "poseidon_gl" => {
                assert!(args.is_empty());
                let inputs = (0..12)
                    .map(|i| self.proc.get_reg(&register_by_idx(i)).into_fe())
                    .collect::<Vec<_>>();
                let result = poseidon_gl::poseidon_gl(&inputs);
                (0..4).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i), Elem::Field(result[i]))
                });
                vec![]
            }
            "affine_256" => {
                assert!(args.is_empty());
                // take input from registers
                let x1 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 3)).into_fe())
                    .collect::<Vec<_>>();
                let y1 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 11)).into_fe())
                    .collect::<Vec<_>>();
                let x2 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 19)).into_fe())
                    .collect::<Vec<_>>();
                let result = arith::affine_256(&x1, &y1, &x2);
                // store result in registers
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i + 3), Elem::Field(result.0[i]))
                });
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i + 11), Elem::Field(result.1[i]))
                });
                vec![]
            }
            "mod_256" => {
                assert!(args.is_empty());
                // take input from registers
                let y2 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 3)).into_fe())
                    .collect::<Vec<_>>();
                let y3 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 11)).into_fe())
                    .collect::<Vec<_>>();
                let x1 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 19)).into_fe())
                    .collect::<Vec<_>>();
                let result = arith::mod_256(&y2, &y3, &x1);
                // store result in registers
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i + 3), Elem::Field(result[i]))
                });
                vec![]
            }
            "ec_add" => {
                assert!(args.is_empty());
                // take input from registers
                let x1 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 4)).into_fe())
                    .collect::<Vec<_>>();
                let y1 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 12)).into_fe())
                    .collect::<Vec<_>>();
                let x2 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 20)).into_fe())
                    .collect::<Vec<_>>();
                let y2 = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 28)).into_fe())
                    .collect::<Vec<_>>();
                let result = arith::ec_add(&x1, &y1, &x2, &y2);
                // store result in registers
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i + 4), Elem::Field(result.0[i]))
                });
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i + 12), Elem::Field(result.1[i]))
                });
                vec![]
            }
            "ec_double" => {
                assert!(args.is_empty());
                // take input from registers
                let x = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 2)).into_fe())
                    .collect::<Vec<_>>();
                let y = (0..8)
                    .map(|i| self.proc.get_reg(&register_by_idx(i + 10)).into_fe())
                    .collect::<Vec<_>>();
                let result = arith::ec_double(&x, &y);
                // store result in registers
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i + 2), Elem::Field(result.0[i]))
                });
                (0..8).for_each(|i| {
                    self.proc
                        .set_reg(&register_by_idx(i + 10), Elem::Field(result.1[i]))
                });
                vec![]
            }
            instr => {
                panic!("unknown instruction: {instr}");
            }
        }
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
                    .strip_prefix("std::prover::Query::")
                    .unwrap_or_else(|| panic!("Free input does not match pattern: {expr}"))
                    .to_string();
                let values = arguments
                    .iter()
                    .map(|arg| self.eval_expression(arg)[0].to_string())
                    .collect::<Vec<_>>();
                let query = format!("{variant}({})", values.join(","));
                match (self.inputs)(&query).unwrap() {
                    Some(val) => vec![Elem::new_from_fe_as_bin(&val)],
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

/// return true if the expression is a jump instruction
fn is_jump(e: &Expression) -> bool {
    if let Expression::FunctionCall(_, FunctionCall { function, .. }) = e {
        if let Expression::Reference(_, f) = function.as_ref() {
            return ["jump", "jump_dyn"].contains(&f.try_to_identifier().unwrap().as_str());
        }
    }
    false
}

pub fn execute_ast<T: FieldElement>(
    program: &AnalysisASMFile,
    initial_memory: MemoryState,
    inputs: &Callback<T>,
    bootloader_inputs: &[Elem<T>],
    max_steps_to_execute: usize,
    mode: ExecMode,
    profiling: Option<ProfilerOptions>,
) -> (ExecutionTrace<T>, MemoryState) {
    let main_machine = get_main_machine(program);
    let PreprocessedMain {
        statements,
        label_map,
        batch_to_line_map,
        debug_files,
        function_starts,
        location_starts,
    } = preprocess_main_function(main_machine);

    let proc = match TraceBuilder::<'_, T>::new(
        main_machine,
        initial_memory,
        &batch_to_line_map,
        max_steps_to_execute,
        mode,
    ) {
        Ok(proc) => proc,
        Err(ret) => return *ret,
    };

    let mut e = Executor {
        proc,
        label_map,
        inputs,
        bootloader_inputs,
        _stdout: io::stdout(),
    };

    let mut profiler =
        profiling.map(|opt| Profiler::new(opt, &debug_files[..], function_starts, location_starts));

    let mut curr_pc = 0u32;
    loop {
        let stm = statements[curr_pc as usize];

        log::trace!("l {curr_pc}: {stm}",);

        match stm {
            FunctionStatement::Assignment(a) => {
                if let Some(p) = &mut profiler {
                    p.add_instruction_cost(e.proc.get_pc().u() as usize);
                }

                let pc_before = e.proc.get_reg("pc").u() as usize;

                let results = e.eval_expression(a.rhs.as_ref());
                assert_eq!(a.lhs_with_reg.len(), results.len());

                let pc_after = e.proc.get_reg("pc").u() as usize;

                if is_jump(a.rhs.as_ref()) {
                    let pc_return = results[0].u() as usize;
                    assert_eq!(a.lhs_with_reg.len(), 1);
                    if let Some(p) = &mut profiler {
                        // in the generated powdr asm, writing to `tmp1` means the returning pc is ignored
                        if a.lhs_with_reg[0].0 == "tmp1" {
                            p.jump(pc_after);
                        } else {
                            p.jump_and_link(pc_before, pc_after, pc_return);
                        }
                    }
                }

                for ((dest, _), val) in a.lhs_with_reg.iter().zip(results) {
                    e.proc.set_reg(dest, val);
                }
            }
            FunctionStatement::Instruction(i) => {
                assert!(!["jump", "jump_dyn"].contains(&i.instruction.as_str()));
                if let Some(p) = &mut profiler {
                    p.add_instruction_cost(e.proc.get_pc().u() as usize);
                }
                e.exec_instruction(&i.instruction, &i.inputs);
            }
            FunctionStatement::Return(_) => break,
            FunctionStatement::DebugDirective(dd) => {
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
            Some(pc) => pc,
            None => break,
        };
    }

    if let Some(mut p) = profiler {
        p.finish();
    }
    e.proc.finish()
}

pub enum ExecMode {
    Fast,
    Trace,
}

/// Execute a Powdr/RISCV assembly source.
///
/// Generic argument F is just used by the powdr_parser, before everything is
/// converted to i64, so it is important to the execution itself.
pub fn execute<F: FieldElement>(
    asm_source: &str,
    initial_memory: MemoryState,
    inputs: &Callback<F>,
    bootloader_inputs: &[Elem<F>],
    mode: ExecMode,
    profiling: Option<ProfilerOptions>,
) -> (ExecutionTrace<F>, MemoryState) {
    log::info!("Parsing...");
    let parsed = powdr_parser::parse_asm(None, asm_source).unwrap();
    log::info!("Resolving imports...");
    let resolved = powdr_importer::load_dependencies_and_resolve(None, parsed).unwrap();
    log::info!("Analyzing...");
    let analyzed = powdr_analysis::analyze(resolved).unwrap();

    log::info!("Executing...");
    execute_ast(
        &analyzed,
        initial_memory,
        inputs,
        bootloader_inputs,
        usize::MAX,
        mode,
        profiling,
    )
}

/// FIXME: copied from `riscv/runtime.rs` instead of adding dependency.
/// Helper function for register names used in submachine instruction params.
fn register_by_idx(mut idx: usize) -> String {
    // s0..11 callee saved registers
    static SAVED_REGS: [&str; 12] = [
        "x8", "x9", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27",
    ];

    // first, use syscall_registers
    if idx < SYSCALL_REGISTERS.len() {
        return SYSCALL_REGISTERS[idx].to_string();
    }
    idx -= SYSCALL_REGISTERS.len();
    // second, callee saved registers
    if idx < SAVED_REGS.len() {
        return SAVED_REGS[idx].to_string();
    }
    idx -= SAVED_REGS.len();
    // lastly, use extra submachine registers
    format!("xtra{idx}")
}
