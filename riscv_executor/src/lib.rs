//! A specialized executor for our RISC-V assembly that can speedup witgen.
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
    parsed::{asm::DebugDirective, Expression},
};
use builder::{MemoryBuilder, TraceBuilder};
use number::{BigInt, FieldElement};

#[derive(Clone, Copy, PartialEq, Eq)]
struct Elem(i64);

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
}

impl From<u32> for Elem {
    fn from(value: u32) -> Self {
        Self(value as i64)
    }
}

impl From<i64> for Elem {
    fn from(value: i64) -> Self {
        Self(value)
    }
}

impl From<i32> for Elem {
    fn from(value: i32) -> Self {
        Self(value as i64)
    }
}

pub struct ExecutionTrace<'a> {
    reg_map: HashMap<&'a str, usize>,

    /// Values of the registers in the execution trace.
    ///
    /// Each N elements is a row with all registers.
    values: Vec<Elem>,
}

mod builder {
    use std::collections::HashMap;

    use ast::asm_analysis::{Machine, RegisterTy};
    use number::FieldElement;

    use crate::{Elem, ExecutionTrace};

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

    pub struct TraceBuilder<'a, 'b> {
        trace: ExecutionTrace<'a>,

        /// First register of current row.
        /// Next row is reg_map.len() elems ahead.
        curr_idx: usize,

        // index of special case registers to look after:
        x0_idx: usize,
        pc_idx: usize,

        /// The PC in the register bank refers to the batches, we have to track our
        /// actual program counter independently.
        next_statement_line: u32,

        /// When PC is written, we need to know what line to actually execute next
        /// from this map of batch to statement line.
        batch_to_line_map: &'b [u32],
    }

    impl<'a, 'b> TraceBuilder<'a, 'b> {
        // creates a new builder
        pub fn new<T: FieldElement>(main: &'a Machine<T>, batch_to_line_map: &'b [u32]) -> Self {
            let reg_map = register_names(main)
                .into_iter()
                .enumerate()
                .map(|(i, name)| (name, i))
                .collect::<HashMap<&str, usize>>();

            // first row has all values zeroed
            let values = vec![Elem::zero(); 2 * reg_map.len()];

            let mut ret = Self {
                curr_idx: 0,
                x0_idx: reg_map["x0"],
                pc_idx: reg_map["pc"],
                trace: ExecutionTrace { reg_map, values },
                next_statement_line: 1,
                batch_to_line_map,
            };

            ret.set_next_pc();

            ret
        }

        /// get current value of register
        pub(crate) fn g(&self, idx: &str) -> Elem {
            self.g_idx(self.trace.reg_map[idx])
        }

        /// get current value of register by register index instead of name
        fn g_idx(&self, idx: usize) -> Elem {
            self.trace.values[self.curr_idx + idx]
        }

        fn g_idx_next(&self, idx: usize) -> Elem {
            self.trace.values[self.curr_idx + self.reg_len() + idx]
        }

        /// set next value of register, accounting to x0 or pc writes
        pub(crate) fn s(&mut self, idx: &str, value: impl Into<Elem>) {
            self.s_impl(idx, value.into())
        }

        fn s_impl(&mut self, idx: &str, value: Elem) {
            let idx = self.trace.reg_map[idx];
            if idx == self.x0_idx {
                return;
            } else if idx == self.pc_idx {
                // PC has been written, so we must update our statement-based
                // program counter accordingly:
                self.next_statement_line = self.batch_to_line_map[value.u() as usize];
            }
            self.s_idx(idx, value);
        }

        /// raw set next value of register by register index instead of name
        fn s_idx(&mut self, idx: usize, value: Elem) {
            let final_idx = self.curr_idx + self.reg_len() + idx;
            self.trace.values[final_idx] = value;
        }

        /// advance to next row, returns the index to the statement that must be
        /// executed now
        pub fn advance(&mut self, was_nop: bool) -> u32 {
            if self.g_idx(self.pc_idx) != self.g_idx_next(self.pc_idx) {
                // PC changed, create a new line
                self.curr_idx += self.reg_len();
                self.trace.values.extend_from_within(self.curr_idx..);
            } else {
                // PC didn't change, execution was inside same batch,
                // so there is no need to create a new row, just update curr
                if !was_nop {
                    let next_idx = self.curr_idx + self.reg_len();
                    self.trace.values.copy_within(next_idx.., self.curr_idx);
                }
            }

            // advance the next statement
            let curr_line = self.next_statement_line;
            self.next_statement_line += 1;

            // optimistically write next PC, but the code might rewrite it
            self.set_next_pc();

            curr_line
        }

        pub fn finish(self) -> ExecutionTrace<'a> {
            self.trace
        }

        fn reg_len(&self) -> usize {
            self.trace.reg_map.len()
        }

        fn set_next_pc(&mut self) {
            let curr_pc = self.g_idx(self.pc_idx).u();

            let line_of_next_batch = self.batch_to_line_map[curr_pc as usize + 1];

            self.s_idx(
                self.pc_idx,
                if self.next_statement_line >= line_of_next_batch {
                    assert_eq!(self.next_statement_line, line_of_next_batch);
                    curr_pc + 1
                } else {
                    curr_pc
                }
                .into(),
            );
        }
    }

    pub struct MemoryBuilder(
        // TODO: track modifications to help build the memory machine
        HashMap<u32, Elem>,
    );

    impl MemoryBuilder {
        pub fn new() -> Self {
            Self(HashMap::new())
        }

        pub(crate) fn s(&mut self, addr: u32, val: Elem) {
            if val.u() != 0 {
                self.0.insert(addr, val);
            } else {
                self.0.remove(&addr);
            }
        }

        pub(crate) fn g(&mut self, addr: u32) -> Elem {
            *self.0.get(&addr).unwrap_or(&Elem::zero())
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
    let mut batch_to_line_map = Vec::new();
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
                    label_map.insert(name.as_str(), (batch_idx as i64).into());
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
    proc: TraceBuilder<'a, 'b>,
    mem: MemoryBuilder,
    label_map: HashMap<&'a str, Elem>,
    inputs: &'b [F],
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
                self.mem.s(args[0].0 as u32, args[1]);

                Vec::new()
            }
            "mload" => {
                let addr = args[0].0 as u32;
                let val = self.mem.g(addr & 0xfffffffc);
                let rem = addr % 4;

                vec![val, rem.into()]
            }
            "jump" => {
                self.proc.s("pc", args[0]);

                Vec::new()
            }
            "load_label" => args,
            "jump_dyn" => {
                self.proc.s("pc", args[0]);

                Vec::new()
            }
            "jump_and_link_dyn" => {
                let pc = self.proc.g("pc");
                self.proc.s("x1", pc.u() + 1);
                self.proc.s("pc", args[0]);

                Vec::new()
            }
            "call" => {
                let pc = self.proc.g("pc");
                self.proc.s("x1", pc.u() + 1);
                self.proc.s("pc", args[0]);

                Vec::new()
            }
            "tail" => {
                self.proc.s("pc", args[0]);
                self.proc.s("x6", args[0]);

                Vec::new()
            }
            "ret" => {
                let target = self.proc.g("x1");
                self.proc.s("pc", target);

                Vec::new()
            }
            "branch_if_nonzero" => {
                if args[0].0 != 0 {
                    self.proc.s("pc", args[1]);
                }

                Vec::new()
            }
            "branch_if_zero" => {
                if args[0].0 == 0 {
                    self.proc.s("pc", args[1]);
                }

                Vec::new()
            }
            "skip_if_zero" => {
                if args[0].0 == 0 {
                    let pc = self.proc.g("pc").s();
                    self.proc.s("pc", pc + args[1].s() + 1);
                }

                Vec::new()
            }
            "branch_if_positive" => {
                if args[0].0 > 0 {
                    self.proc.s("pc", args[1]);
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
            bin_op => {
                let val = match bin_op {
                    "poseidon" => todo!(),
                    "and" => (args[0].u() & args[1].u()).into(),
                    "or" => (args[0].u() | args[1].u()).into(),
                    "xor" => (args[0].u() ^ args[1].u()).into(),
                    "shl" => (args[0].u() << args[1].u()).into(),
                    "shr" => (args[0].u() >> args[1].u()).into(),
                    _ => {
                        unreachable!()
                    }
                };

                vec![val]
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
                    .unwrap_or_else(|| self.proc.g(name));
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
                    ast::parsed::BinaryOperator::Mul => l.0 * r.0,
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
            Expression::FunctionCall(f) => self.exec_instruction(&f.id, &f.arguments),
            Expression::FreeInput(expr) => 'input: {
                if let Expression::Tuple(t) = &**expr {
                    if let Expression::String(name) = &t[0] {
                        let val = self.eval_expression(&t[1])[0];
                        break 'input vec![match name.as_str() {
                            "input" => {
                                let idx = val.u() as usize;
                                to_u32(&self.inputs[idx]).unwrap().into()
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
            Expression::IndexAccess(_) => todo!(),
        }
    }
}

pub fn execute_ast<'a, T: FieldElement>(
    program: &'a AnalysisASMFile<T>,
    inputs: &[T],
) -> ExecutionTrace<'a> {
    let main_machine = get_main_machine(program);
    let PreprocessedMain {
        statements,
        label_map,
        batch_to_line_map,
        debug_files,
    } = preprocess_main_function(main_machine);

    let mut e = Executor {
        proc: TraceBuilder::new(main_machine, &batch_to_line_map),
        mem: MemoryBuilder::new(),
        label_map,
        inputs,
        stdout: io::stdout(),
    };

    let mut curr_pc = 0u32;
    loop {
        let stm = statements[curr_pc as usize];

        //println!("l {curr_pc}: {stm}",);

        let is_nop = match stm {
            FunctionStatement::Assignment(a) => {
                let results = e.eval_expression(a.rhs.as_ref());
                assert_eq!(a.lhs_with_reg.len(), results.len());
                for ((dest, _), val) in a.lhs_with_reg.iter().zip(results) {
                    e.proc.s(dest, val);
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
                        println!("Executed {dir}/{file}:{line}:{column}");
                    }
                    DebugDirective::OriginalInstruction(insn) => {
                        println!("  {insn}");
                    }
                    DebugDirective::File(_, _, _) => unreachable!(),
                };

                true
            }
            FunctionStatement::Label(_) => {
                unreachable!()
            }
        };

        curr_pc = e.proc.advance(is_nop);
    }

    e.proc.finish()
}

/// Execute a Powdr/RISCV assembly source.
///
/// The FieldElement is just used by the parser, before everything is converted
/// to i64, so it is probably not very important.
pub fn execute<F: FieldElement>(asm_source: &str, inputs: &[F]) {
    log::info!("Parsing...");
    let parsed = parser::parse_asm::<F>(None, asm_source).unwrap();
    log::info!("Resolving imports...");
    let resolved = importer::resolve(None, parsed).unwrap();
    log::info!("Analyzing...");
    let analyzed = analysis::analyze(resolved, &mut ast::DiffMonitor::default()).unwrap();

    log::info!("Executing...");
    execute_ast(&analyzed, inputs);
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

#[cfg(test)]
mod test {
    use crate::execute;
    use number::GoldilocksField;
    use std::fs;

    #[test]
    fn execute_from_file() {
        println!("{}", std::env::current_dir().unwrap().to_string_lossy());

        println!("Loading...");
        let asm = fs::read("../tmp/evm.asm").unwrap();
        println!("Validating UTF-8...");
        let asm_str = std::str::from_utf8(&asm).unwrap();

        execute::<GoldilocksField>(asm_str, &[]);
    }
}
