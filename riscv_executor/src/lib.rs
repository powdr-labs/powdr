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
    collections::HashMap,
    io::{self, Write},
};

use ast::{
    asm_analysis::{AnalysisASMFile, CallableSymbol, FunctionStatement, LabelStatement, Machine},
    parsed::{asm::DebugDirective, Expression, FunctionCall},
};
use builder::TraceBuilder;
use number::{BigInt, FieldElement};

mod poseidon_gl;

/// Initial value of the PC.
///
/// To match the ZK proof witness, the PC must start after some offset used for
/// proof initialization.
///
/// TODO: get this value from some authoritative place
const PC_INITIAL_VAL: usize = 2;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Elem(pub i64);

impl Elem {
    const fn zero() -> Self {
        Self(0)
    }

    fn u(&self) -> u32 {
        self.0.try_into().unwrap()
    }

    fn s(&self) -> i32 {
        self.0.try_into().unwrap()
    }

    fn fe<F: FieldElement>(&self) -> F {
        F::from(self.0)
    }

    // Rust doesn't allow us to implement From<F> for Elem...
    fn from_fe<F: FieldElement>(value: F) -> Self {
        let value = value.to_degree();
        let p: u64 = F::modulus().to_arbitrary_integer().try_into().unwrap();
        if value < p >> 1 {
            (value as i64).into()
        } else {
            (-((p - value) as i64)).into()
        }
    }
}

impl From<u32> for Elem {
    fn from(value: u32) -> Self {
        Self(value as i64)
    }
}

impl From<i32> for Elem {
    fn from(value: i32) -> Self {
        Self(value as i64)
    }
}

impl From<i64> for Elem {
    fn from(value: i64) -> Self {
        Self(value)
    }
}

impl From<usize> for Elem {
    fn from(value: usize) -> Self {
        Self(value as i64)
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
    /// Line of the register trace the memory operation happened.
    pub idx: usize,
    pub kind: MemOperationKind,
    pub address: u32,
}

pub struct ExecutionTrace {
    pub reg_map: HashMap<String, usize>,

    /// Values of the registers in the execution trace.
    ///
    /// Each N elements is a row with all registers, where N is the number of
    /// registers.
    pub regs: Vec<Elem>,

    /// Writes and reads to memory.
    pub mem: Vec<MemOperation>,

    /// The length of the trace.
    pub len: u64,
}

impl ExecutionTrace {
    /// Split the values of the registers' trace into rows.
    pub fn regs_rows(&self) -> impl Iterator<Item = &[Elem]> {
        self.regs.chunks_exact(self.reg_map.len())
    }

    pub fn row(&self, idx: usize) -> &[Elem] {
        &self.regs[(idx * self.reg_map.len())..((idx + 1) * self.reg_map.len())]
    }
}

mod builder {
    use std::{cmp, collections::HashMap};

    use ast::asm_analysis::{Machine, RegisterTy};
    use number::FieldElement;

    use crate::{
        Elem, ExecMode, ExecutionTrace, MemOperation, MemOperationKind, MemoryState, PC_INITIAL_VAL,
    };

    fn register_names<T: FieldElement>(main: &Machine<T>) -> Vec<&str> {
        main.registers
            .iter()
            .filter_map(|stmnt| {
                if stmnt.ty != RegisterTy::Assignment {
                    Some(&stmnt.name[..])
                } else {
                    None
                }
            })
            .collect()
    }

    pub struct TraceBuilder<'b> {
        trace: ExecutionTrace,

        /// First register of current row.
        /// Next row is reg_map.len() elems ahead.
        curr_idx: usize,

        /// Maximum value curr_idx can have before we stop the execution.
        max_curr_idx: usize,

        // index of special case registers to look after:
        x0_idx: usize,
        pc_idx: usize,

        /// The PC in the register bank refers to the batches, we have to track our
        /// actual program counter independently.
        next_statement_line: u32,

        /// When PC is written, we need to know what line to actually execute next
        /// from this map of batch to statement line.
        batch_to_line_map: &'b [u32],

        /// Current memory.
        mem: HashMap<u32, u32>,

        /// The execution mode we running.
        /// Fast: do not save the register's trace and memory accesses.
        /// Trace: save everything - needed for continuations.
        mode: ExecMode,
    }

    impl<'a, 'b> TraceBuilder<'b> {
        /// Creates a new builder.
        ///
        /// May fail if max_rows_len is too small or if the main machine is
        /// empty. In this case, the final (empty) execution trace is returned
        /// in Err.
        pub fn new<T: FieldElement>(
            main: &'a Machine<T>,
            batch_to_line_map: &'b [u32],
            max_rows_len: usize,
            mode: ExecMode,
        ) -> Result<Self, Box<(ExecutionTrace, MemoryState)>> {
            let reg_map = register_names(main)
                .into_iter()
                .enumerate()
                .map(|(i, name)| (name.to_string(), i))
                .collect::<HashMap<String, usize>>();

            let reg_len = reg_map.len();

            // the + 2 accounts for current row and next row
            const ROW_COUNT: usize = PC_INITIAL_VAL + 2;

            // first rows has all values zeroed...
            let mut values = vec![Elem::zero(); ROW_COUNT * reg_len];

            // ...except for the PC
            let pc_idx = reg_map["pc"];
            for i in 0..=PC_INITIAL_VAL {
                values[pc_idx + reg_len * i] = i.into();
            }

            let mut ret = Self {
                curr_idx: PC_INITIAL_VAL * reg_len,
                x0_idx: reg_map["x0"],
                pc_idx,
                trace: ExecutionTrace {
                    reg_map,
                    regs: values,
                    mem: Vec::new(),
                    len: 0,
                },
                next_statement_line: 1,
                batch_to_line_map,
                max_curr_idx: max_rows_len.saturating_sub(1).saturating_mul(reg_len),
                mem: HashMap::new(),
                mode,
            };

            if ret.has_enough_rows() || ret.set_next_pc().is_none() {
                Err(Box::new(ret.finish()))
            } else {
                Ok(ret)
            }
        }

        /// get current value of register
        pub(crate) fn get_reg(&self, idx: &str) -> Elem {
            self.get_reg_idx(self.trace.reg_map[idx])
        }

        /// get current value of register by register index instead of name
        fn get_reg_idx(&self, idx: usize) -> Elem {
            self.trace.regs[self.curr_idx + idx]
        }

        fn get_reg_idx_next(&self, idx: usize) -> Elem {
            self.trace.regs[self.curr_idx + self.reg_len() + idx]
        }

        /// sets the PC
        pub(crate) fn set_pc(&mut self, value: Elem) {
            // updates the internal statement-based program counter accordingly:
            self.next_statement_line = self.batch_to_line_map[value.u() as usize];
            self.set_reg_idx(self.pc_idx, value);
        }

        /// set next value of register, accounting to x0 writes
        ///
        /// to set the PC, use s_pc() instead of this
        pub(crate) fn set_reg(&mut self, idx: &str, value: impl Into<Elem>) {
            self.set_reg_impl(idx, value.into())
        }

        fn set_reg_impl(&mut self, idx: &str, value: Elem) {
            let idx = self.trace.reg_map[idx];
            if idx == self.x0_idx {
                return;
            }
            self.set_reg_idx(idx, value);
        }

        /// raw set next value of register by register index instead of name
        fn set_reg_idx(&mut self, idx: usize, value: Elem) {
            let final_idx = self.curr_idx + self.reg_len() + idx;
            self.trace.regs[final_idx] = value;
        }

        /// advance to next row, returns the index to the statement that must be
        /// executed now, or None if the execution is finished
        pub fn advance(&mut self, was_nop: bool) -> Option<u32> {
            if !was_nop {
                self.trace.len += 1;
            }

            if self.get_reg_idx(self.pc_idx) != self.get_reg_idx_next(self.pc_idx) {
                if let ExecMode::Trace = self.mode {
                    // PC changed, create a new line
                    self.curr_idx += self.reg_len();
                    self.trace.regs.extend_from_within(self.curr_idx..);
                } else if !was_nop {
                    let next_idx = self.curr_idx + self.reg_len();
                    self.trace.regs.copy_within(next_idx.., self.curr_idx);
                }

                // If we are at the limit of rows, stop the execution
                if self.has_enough_rows() {
                    return None;
                }
            } else {
                // PC didn't change, execution was inside same batch,
                // so there is no need to create a new row, just update curr
                if !was_nop {
                    let next_idx = self.curr_idx + self.reg_len();
                    self.trace.regs.copy_within(next_idx.., self.curr_idx);
                }
            }

            // advance the next statement
            let curr_line = self.next_statement_line;
            self.next_statement_line += 1;

            // optimistically write next PC, but the code might rewrite it
            self.set_next_pc().and(Some(curr_line))
        }

        pub(crate) fn set_mem(&mut self, addr: u32, val: u32) {
            if let ExecMode::Trace = self.mode {
                self.trace.mem.push(MemOperation {
                    idx: self.curr_idx / self.reg_len() + 1,
                    kind: MemOperationKind::Write,
                    address: addr,
                });
            }

            self.mem.insert(addr, val);
        }

        pub(crate) fn get_mem(&mut self, addr: u32) -> u32 {
            if let ExecMode::Trace = self.mode {
                self.trace.mem.push(MemOperation {
                    idx: self.curr_idx / self.reg_len() + 1,
                    kind: MemOperationKind::Read,
                    address: addr,
                });
            }

            *self.mem.get(&addr).unwrap_or(&0)
        }

        pub fn finish(mut self) -> (ExecutionTrace, MemoryState) {
            // remove the last row (future row), as it is not part of the trace
            self.trace.regs.drain((self.curr_idx + self.reg_len())..);
            (self.trace, self.mem)
        }

        fn reg_len(&self) -> usize {
            self.trace.reg_map.len()
        }

        /// Should we stop the execution because the maximum number of rows has
        /// been reached?
        fn has_enough_rows(&self) -> bool {
            self.curr_idx >= self.max_curr_idx
        }

        fn set_next_pc(&mut self) -> Option<()> {
            let curr_pc = self.get_reg_idx(self.pc_idx).u();

            let line_of_next_batch = *self.batch_to_line_map.get(curr_pc as usize + 1)?;

            self.set_reg_idx(
                self.pc_idx,
                match self.next_statement_line.cmp(&line_of_next_batch) {
                    cmp::Ordering::Less => curr_pc,
                    cmp::Ordering::Equal => curr_pc + 1,
                    cmp::Ordering::Greater => {
                        panic!(
                            "next_statement_line: {} > line_of_next_batch: {}",
                            self.next_statement_line, line_of_next_batch
                        );
                    }
                }
                .into(),
            );

            Some(())
        }
    }
}

fn get_main_machine<T: FieldElement>(program: &AnalysisASMFile<T>) -> &Machine<T> {
    for (name, m) in program.machines.iter() {
        if name.parts.len() == 1 && name.parts[0] == "Main" {
            return m;
        }
    }
    panic!();
}

struct PreprocessedMain<'a, T: FieldElement> {
    statements: Vec<&'a FunctionStatement<T>>,
    label_map: HashMap<&'a str, Elem>,
    batch_to_line_map: Vec<u32>,
    debug_files: Vec<(&'a str, &'a str)>,
}

/// Returns the list of instructions, directly indexable by PC, the map from
/// labels to indices into that list, and the list with the start of each batch.
fn preprocess_main_function<T: FieldElement>(machine: &Machine<T>) -> PreprocessedMain<T> {
    let CallableSymbol::Function(main_function) = &machine.callable.0["main"] else {
        panic!("main function missing")
    };

    let orig_statements = &main_function.body.statements;

    let mut statements = Vec::new();
    let mut label_map = HashMap::new();
    let mut batch_to_line_map = vec![0; PC_INITIAL_VAL];
    let mut debug_files = Vec::new();

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
                        DebugDirective::Loc(_, _, _) | DebugDirective::OriginalInstruction(_) => {
                            // keep debug locs for debugging purposes
                            statements.push(s);
                        }
                    }
                }
                FunctionStatement::Label(LabelStatement { start: _, name }) => {
                    // assert there are no statements in the middle of a block
                    assert!(!statement_seen);
                    label_map.insert(name.as_str(), ((batch_idx + PC_INITIAL_VAL) as i64).into());
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
    }
}

struct Executor<'a, 'b, F: FieldElement> {
    proc: TraceBuilder<'b>,
    label_map: HashMap<&'a str, Elem>,
    inputs: HashMap<F, Vec<F>>,
    bootloader_inputs: &'b [F],
    stdout: io::Stdout,
}

impl<'a, 'b, F: FieldElement> Executor<'a, 'b, F> {
    fn exec_instruction(&mut self, name: &str, args: &[Expression<F>]) -> Vec<Elem> {
        let args = args
            .iter()
            .map(|expr| self.eval_expression(expr)[0])
            .collect::<Vec<_>>();

        match name {
            "mstore" => {
                let addr = args[0].0 as u32;
                assert_eq!(addr % 4, 0);
                self.proc.set_mem(args[0].0 as u32, args[1].u());

                Vec::new()
            }
            "mload" => {
                let addr = args[0].0 as u32;
                let val = self.proc.get_mem(addr & 0xfffffffc);
                let rem = addr % 4;

                vec![val.into(), rem.into()]
            }
            "jump" => {
                self.proc.set_pc(args[0]);

                Vec::new()
            }
            "load_label" => args,
            "jump_dyn" => {
                self.proc.set_pc(args[0]);

                Vec::new()
            }
            "jump_and_link_dyn" => {
                let pc = self.proc.get_reg("pc");
                self.proc.set_reg("x1", pc.u() + 1);
                self.proc.set_pc(args[0]);

                Vec::new()
            }
            "call" => {
                let pc = self.proc.get_reg("pc");
                self.proc.set_reg("x1", pc.u() + 1);
                self.proc.set_pc(args[0]);

                Vec::new()
            }
            "tail" => {
                self.proc.set_pc(args[0]);
                self.proc.set_reg("x6", args[0]);

                Vec::new()
            }
            "ret" => {
                let target = self.proc.get_reg("x1");
                self.proc.set_pc(target);

                Vec::new()
            }
            "branch_if_nonzero" => {
                if args[0].0 != 0 {
                    self.proc.set_pc(args[1]);
                }

                Vec::new()
            }
            "branch_if_zero" => {
                if args[0].0 == 0 {
                    self.proc.set_pc(args[1]);
                }

                Vec::new()
            }
            "skip_if_zero" => {
                if args[0].0 == 0 {
                    let pc = self.proc.get_reg("pc").s();
                    self.proc.set_pc((pc + args[1].s() + 1).into());
                }

                Vec::new()
            }
            "branch_if_positive" => {
                if args[0].0 > 0 {
                    self.proc.set_pc(args[1]);
                }

                Vec::new()
            }
            "is_positive" => {
                let r = if args[0].0 > 0 { 1 } else { 0 };

                vec![r.into()]
            }
            "is_equal_zero" => {
                let r = if args[0].0 == 0 { 1 } else { 0 };

                vec![r.into()]
            }
            "is_not_equal_zero" => {
                let r = if args[0].0 != 0 { 1 } else { 0 };

                vec![r.into()]
            }
            "wrap" | "wrap16" => {
                let r = args[0].0 as u32;

                vec![r.into()]
            }
            "wrap_signed" => {
                let r = (args[0].0 + 0x100000000) as u32;

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
                let arg: u64 = args[0].fe::<F>().to_degree();
                let lo = (arg & 0xffffffff) as u32;
                let hi = (arg >> 32) as u32;

                vec![lo.into(), hi.into()]
            }
            "poseidon_gl" => {
                let inputs = args
                    .iter()
                    .take(12)
                    .map(|arg| arg.fe::<F>())
                    .collect::<Vec<_>>();
                let result = poseidon_gl::poseidon_gl(&inputs);
                result.into_iter().map(Elem::from_fe).collect()
            }
            instr => {
                panic!("unknown instruction: {instr}");
            }
        }
    }

    fn eval_expression(&mut self, expression: &Expression<F>) -> Vec<Elem> {
        match expression {
            Expression::Reference(r) => {
                // an identifier looks like this:
                assert!(r.namespace.is_none());

                let name = r.name.as_str();

                // labels share the identifier space with registers:
                // try one, then the other
                let val = self
                    .label_map
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| self.proc.get_reg(name));
                vec![val]
            }
            Expression::PublicReference(_) => todo!(),
            Expression::Number(n) => {
                vec![if let Some(unsigned) = to_u32(n) {
                    unsigned.into()
                } else {
                    panic!("Value does not fit in 32 bits.")
                }]
            }
            Expression::String(_) => todo!(),
            Expression::Tuple(_) => todo!(),
            Expression::LambdaExpression(_) => todo!(),
            Expression::ArrayLiteral(_) => todo!(),
            Expression::BinaryOperation(l, op, r) => {
                let l = self.eval_expression(l)[0];
                let r = self.eval_expression(r)[0];

                let result = match op {
                    ast::parsed::BinaryOperator::Add => l.0 + r.0,
                    ast::parsed::BinaryOperator::Sub => l.0 - r.0,
                    ast::parsed::BinaryOperator::Mul => {
                        // Do multiplication in the field, in case we overflow.
                        let l: F = l.fe();
                        let r: F = r.fe();
                        let res = l * r;
                        Elem::from_fe(res).0
                    }
                    ast::parsed::BinaryOperator::Div => l.0 / r.0,
                    ast::parsed::BinaryOperator::Mod => l.0 % r.0,
                    ast::parsed::BinaryOperator::Pow => l.0.pow(r.u()),
                    ast::parsed::BinaryOperator::BinaryAnd => todo!(),
                    ast::parsed::BinaryOperator::BinaryXor => todo!(),
                    ast::parsed::BinaryOperator::BinaryOr => todo!(),
                    ast::parsed::BinaryOperator::ShiftLeft => todo!(),
                    ast::parsed::BinaryOperator::ShiftRight => todo!(),
                    ast::parsed::BinaryOperator::LogicalOr => todo!(),
                    ast::parsed::BinaryOperator::LogicalAnd => todo!(),
                    ast::parsed::BinaryOperator::Less => todo!(),
                    ast::parsed::BinaryOperator::LessEqual => todo!(),
                    ast::parsed::BinaryOperator::Equal => todo!(),
                    ast::parsed::BinaryOperator::NotEqual => todo!(),
                    ast::parsed::BinaryOperator::GreaterEqual => todo!(),
                    ast::parsed::BinaryOperator::Greater => todo!(),
                };

                vec![result.into()]
            }
            Expression::UnaryOperation(op, arg) => {
                let arg = self.eval_expression(arg)[0];
                let result = match op {
                    ast::parsed::UnaryOperator::Plus => arg.0,
                    ast::parsed::UnaryOperator::Minus => -arg.0,
                    ast::parsed::UnaryOperator::LogicalNot => todo!(),
                    ast::parsed::UnaryOperator::Next => unreachable!(),
                };

                vec![result.into()]
            }
            Expression::FunctionCall(FunctionCall {
                function,
                arguments,
            }) => match function.as_ref() {
                Expression::Reference(f) => {
                    assert!(f.namespace.is_none());
                    self.exec_instruction(&f.name, arguments)
                }
                _ => panic!(),
            },
            Expression::FreeInput(expr) => 'input: {
                if let Expression::Tuple(t) = &**expr {
                    if let Expression::String(name) = &t[0] {
                        let val = self.eval_expression(&t[1])[0];
                        break 'input vec![match name.as_str() {
                            "input" => {
                                let idx = val.u() as usize;
                                to_u32(&self.inputs[&F::zero()][idx]).unwrap().into()
                            }
                            "data" => {
                                let idx = val.u() as usize;
                                let what = self.eval_expression(&t[2])[0];
                                let what = what.u();
                                to_u32(&self.inputs[&what.into()][idx]).unwrap().into()
                            }
                            "bootloader_input" => {
                                let idx = val.u() as usize;
                                to_u32(&self.bootloader_inputs[idx]).unwrap().into()
                            }
                            "print_char" => {
                                self.stdout.write_all(&[val.u() as u8]).unwrap();
                                // what is print_char supposed to return?
                                Elem::zero()
                            }
                            unk => {
                                panic!("unknown IO command: {unk}");
                            }
                        }];
                    }
                };
                panic!("does not matched IO pattern")
            }
            Expression::MatchExpression(_, _) => todo!(),
            Expression::IfExpression(_) => panic!(),
            Expression::IndexAccess(_) => todo!(),
        }
    }
}

pub fn execute_ast<T: FieldElement>(
    program: &AnalysisASMFile<T>,
    inputs: &HashMap<T, Vec<T>>,
    bootloader_inputs: &[T],
    max_steps_to_execute: usize,
    mode: ExecMode,
) -> (ExecutionTrace, MemoryState) {
    let main_machine = get_main_machine(program);
    let PreprocessedMain {
        statements,
        label_map,
        batch_to_line_map,
        debug_files,
    } = preprocess_main_function(main_machine);

    let proc = match TraceBuilder::new(main_machine, &batch_to_line_map, max_steps_to_execute, mode)
    {
        Ok(proc) => proc,
        Err(ret) => return *ret,
    };

    let mut e = Executor {
        proc,
        label_map,
        inputs: inputs.clone(),
        bootloader_inputs,
        stdout: io::stdout(),
    };

    let mut curr_pc = 0u32;
    loop {
        let stm = statements[curr_pc as usize];

        log::trace!("l {curr_pc}: {stm}",);

        let is_nop = match stm {
            FunctionStatement::Assignment(a) => {
                let results = e.eval_expression(a.rhs.as_ref());
                assert_eq!(a.lhs_with_reg.len(), results.len());
                for ((dest, _), val) in a.lhs_with_reg.iter().zip(results) {
                    e.proc.set_reg(dest, val);
                }

                false
            }
            FunctionStatement::Instruction(i) => {
                e.exec_instruction(&i.instruction, &i.inputs);

                false
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

                true
            }
            FunctionStatement::Label(_) => {
                unreachable!()
            }
        };

        curr_pc = match e.proc.advance(is_nop) {
            Some(pc) => pc,
            None => break,
        };
    }

    e.proc.finish()
}

pub enum ExecMode {
    Fast,
    Trace,
}

/// Execute a Powdr/RISCV assembly source.
///
/// Generic argument F is just used by the parser, before everything is
/// converted to i64, so it is important to the execution itself.
pub fn execute<F: FieldElement>(
    asm_source: &str,
    inputs: &HashMap<F, Vec<F>>,
    bootloader_inputs: &[F],
    mode: ExecMode,
) -> (ExecutionTrace, MemoryState) {
    log::info!("Parsing...");
    let parsed = parser::parse_asm::<F>(None, asm_source).unwrap();
    log::info!("Resolving imports...");
    let resolved = importer::resolve(None, parsed).unwrap();
    log::info!("Analyzing...");
    let analyzed = analysis::analyze(resolved, &mut ast::DiffMonitor::default()).unwrap();

    log::info!("Executing...");
    execute_ast(&analyzed, inputs, bootloader_inputs, usize::MAX, mode)
}

fn to_u32<F: FieldElement>(val: &F) -> Option<u32> {
    val.to_arbitrary_integer().try_into().ok().or_else(|| {
        // Number is negative, gets it binary representation as u32.
        let modulus = F::modulus().to_arbitrary_integer();
        let diff = modulus - val.to_arbitrary_integer();
        if diff <= 0x80000000u32.into() {
            let negated: i64 = diff.try_into().unwrap();
            Some((-negated) as u32)
        } else {
            None
        }
    })
}
