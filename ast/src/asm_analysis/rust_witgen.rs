use super::{InstructionDefinitionStatement, Machine, RegisterTy};

use itertools::iproduct;

pub struct RustWitgen<T> {
    pub machine: Machine<T>,
    pub code: String,
}

// this impl handles translation from Machine to Rust-witgen
impl<T> RustWitgen<T> {
    pub fn new(machine: Machine<T>) -> Self {
        Self {
            machine,
            code: String::new(),
        }
    }

    pub fn generate(&mut self) {
        self.create_imports();
        self.create_execute();
        self.create_context_struct();
        self.create_impl();
    }

    fn create_impl(&mut self) {
        let impl_ = r#"
impl<F: FieldElement> Context<F> {
    "#;
        let impl_ = format!(
            "{}
            \n{}
            \n{}
            \n{}
            \n{}
            \n{}
            \n{}
            \n{}
            \n{}
            \n{}
            \n{}
            }}",
            impl_,
            self.create_fixed_methods(),
            self.create_init(),
            self.create_empty_instructions(),
            self.create_run_instruction(),
            self.create_update_inputs(),
            self.create_update_writes_to_assignment_registers(),
            self.create_updated_writes_to_state_registers(),
            self.create_update_pc(),
            self.create_update_control_flow_flags(),
            self.create_update_flags(),
        );

        self.code = format!("{}\n{}", self.code, impl_);
    }

    fn create_update_flags(&self) -> String {
        let preamble = format!(
            r#"
fn update_flags(&mut self) {{
        let pc = self
            .{}
            .last()
            .unwrap()
            .to_arbitrary_integer()
            .to_le_bytes();
        let pc = u32::from_le_bytes([
            *pc.get(0).unwrap_or_else(|| &0),
            *pc.get(1).unwrap_or_else(|| &0),
            *pc.get(2).unwrap_or_else(|| &0),
            *pc.get(3).unwrap_or_else(|| &0),
        ]);
        "#,
            self.pc()
        );

        let update = |x| {
            format!(
                "self.{} = self.fixed.get(\"main.p_{}\").unwrap()[pc as usize];",
                x, x
            )
        };

        let updates = self
            .instruction_flags()
            .into_iter()
            .chain(self.write_state_to_assignment_reg_columns().into_iter())
            .chain(self.write_assignment_to_state_reg_columns().into_iter())
            .chain(self.asgn_reg_const_columns().into_iter())
            .chain(self.asgn_reg_free_value_read_columns().into_iter())
            .map(update)
            .collect::<Vec<_>>()
            .join("\n");

        format!("{}\n{}\n}}", preamble, updates)
    }

    fn create_update_control_flow_flags(&self) -> String {
        let update = r#"
fn update_control_flow_flags(&mut self) {
        self.operation_id.push(F::from(2));

        if self.current_row < self.length - 1 {
            self.operation_id_no_change.push(F::one());
        } else {
            self.operation_id_no_change.push(F::zero());
        }

        if self.current_row == 0 {
            self.instr_reset.push(F::one());
        } else {
            self.instr_reset.push(F::zero());
        }

        if self.running {
            self.instr_loop.push(F::zero());
        } else {
            self.instr_loop.push(F::one());
        }
    }
"#;
        update.to_string()
    }

    fn create_update_pc(&self) -> String {
        format!(
            r#"
fn update_pc(&mut self) {{
        let pc = self.{}.last().unwrap();
        let pc_prime = if self.instr_jump_to_operation.last().unwrap().is_one() {{
            self.operation_id.last().unwrap().clone()
        }} else if self.instr_loop.last().unwrap().is_one() {{
            pc.clone()
        }} else {{
            pc.clone() + F::one()
        }};
        self.{}.push(pc_prime);
    }}"#,
            self.pc(),
            self.pc()
        )
    }

    fn create_updated_writes_to_state_registers(&self) -> String {
        let update = r#"
fn update_writes_to_state_registers(&mut self) {
    "#;

        let reg_updates = self
            .state_regs()
            .into_iter()
            .map(|r| self.create_updated_write_to_state_register(r))
            .collect::<Vec<_>>()
            .join("\n");

        format!("{}\n{}\n}}", update, reg_updates)
    }

    fn create_updated_write_to_state_register(&self, reg: String) -> String {
        let conds = self
            .asgn_regs()
            .into_iter()
            .map(|r| {
                format!(
                    "if {} == &F::one() {{\nself.{}.push(self.{}.last().unwrap().clone());\n}}",
                    write_assignment_to_state_reg_flag(r.clone(), reg.clone()),
                    reg.clone(),
                    r.clone()
                )
            })
            .collect::<Vec<_>>()
            .join(" else ");

        let last = format!(" else if self.{}.len() < self.{}.len() {{\nself.{}.push(self.{}.last().cloned().unwrap_or_else(|| F::zero().clone()));\n}}", reg.clone(), self.pc(), reg.clone(), reg.clone());

        format!("{}\n{}", conds, last)
    }

    fn create_update_writes_to_assignment_registers(&self) -> String {
        let update = r#"
fn update_writes_to_assignment_registers(&mut self) {
    "#;

        let decls = self
            .asgn_regs()
            .into_iter()
            .map(|r| {
                format!(
                    "let mut {}_prime = self.{}.last().unwrap().clone();",
                    r.clone(),
                    asgn_reg_const(r)
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        let conds = iproduct!(self.asgn_regs().into_iter(), self.state_regs().into_iter())
            .map(|(a, s)| {
                format!(
                    "if {} == &F::one() {{\n{}_prime += *self.{}.last().unwrap();\n}}\n",
                    write_state_to_assignment_reg_flag(s.clone(), a.clone()),
                    a.clone(),
                    s.clone()
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        let read_conds = self.asgn_regs().into_iter()
            .map(|a| format!(
                "if self.{}.last().unwrap() == &F::one() {{ \n{}_prime += *self.{}.last().unwrap();\n}}\n",
                asgn_reg_free_value_read(a.clone()),
                a.clone(),
                asgn_reg_free_value(a),
            ))
            .collect::<Vec<_>>()
            .join("\n");

        let pushes = self
            .asgn_regs()
            .into_iter()
            .map(|r| format!("self.{}.push({}_prime);", r, r))
            .collect::<Vec<_>>()
            .join("\n");

        format!("{}\n{}\n{}\n{}\n{}\n}}", update, decls, conds, read_conds, pushes)
    }

    fn create_update_inputs(&self) -> String {
        let init = r#"
        // for pc + each state register
        fn update_inputs(&mut self) {"#;

        // TODO properly using QueryCallback
        let regs: Vec<_> = self
            .asgn_reg_free_value_columns()
            .into_iter()
            .map(|r| format!("self.{}.push(F::zero());", r))
            .collect();

        format!("{}\n{}\n}}", init, regs.join("\n"))
    }

    fn create_run_instruction(&self) -> String {
        let run = r#"
fn run_instructions(&mut self) {
    "#;
        let decls = self
            .instruction_flags()
            .into_iter()
            .map(|i| format!("let {} = self.{}.last().unwrap();", i, i))
            .collect::<Vec<_>>()
            .join("\n");

        let conds = self
            .instruction_flags()
            .into_iter()
            .map(|i| format!("if {} == &F::one() {{\nself.instr_{}();\n}} else ", i, i))
            .collect::<Vec<_>>()
            .join("\n");

        format!("{}\n{}\n{}{{}}\n}}", run, decls, conds)
    }

    fn create_empty_instructions(&self) -> String {
        self.instruction_flags()
            .into_iter()
            .map(|i| self.create_empty_instruction(i))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn create_empty_instruction(&self, instr: String) -> String {
        format!("fn instr_{}(&mut self) {{\nTODO\n}}", instr)
    }

    fn create_init(&self) -> String {
        let init = r#"
        // for pc + each state register
        fn init(&mut self) {"#;

        let regs: Vec<_> = self
            .state_regs()
            .into_iter()
            .chain(vec![self.pc()].into_iter())
            .map(|r| format!("self.{}.push(F::zero());", r))
            .collect();

        format!("{}\n{}\n}}", init, regs.join("\n"))
    }

    fn create_fixed_methods(&self) -> String {
        let fixed = r#"
pub fn new(length: usize) -> Self {
        Self {
            length,
            ..Default::default()
        }
    }

    pub fn with_fixed(mut self, fixed: HashMap<String, Vec<F>>) -> Self {
        self.fixed = fixed;
        self
    }

    pub fn run(&mut self) {
        self.running = true;

        self.init();

        while self.current_row < self.length {
            self.update();
            self.current_row += 1;

            // Leo: can remove this for now
            if self.current_row >= 6 {
                self.running = false;
            }
        }

        // Leo: can remove this for now, maybe Georg's PR already solves it
        // TODO fix
        *self.a.first_mut().unwrap() = self.a.last().unwrap().clone();
    }

    fn update(&mut self) {
        // order matters here:
        // - the starting point is pc = 0, state registers = 0
        // - update the control flow flags
        // - update the instruction flags
        // - update writes from state registers into assignment registers
        // - run the instructions
        // - update writes from assignment registers into state registers
        // - all assignment registers should have been updated by now
        // - update pc

        self.update_inputs();
        self.update_control_flow_flags();
        self.update_flags();
        self.update_writes_to_assignment_registers();

        if self.current_row < self.length - 1 {
            self.run_instructions();
            self.update_writes_to_state_registers();
            self.update_pc();
        }
    }
"#;
        format!("{}", fixed)
    }

    fn create_imports(&mut self) {
        let imports = r#"
use powdr_ast::analyzed::Analyzed;
use powdr_number::{FieldElement, GoldilocksField};

use crate::Callback;

use num_traits::{One, Zero, ToBytes};

use std::collections::{BTreeMap, HashMap};
"#;
        self.code = format!("{}\n{}", imports, self.code);
    }

    fn create_execute(&mut self) {
        let preamble = r#"
pub fn execute<F: FieldElement>(
    length: usize,
    fixed: HashMap<String, Vec<F>>,
) -> Vec<(String, Vec<F>)> {

    println!("keys: {:?}", fixed.keys());

    let mut ctx = Context::new(length).with_fixed(fixed);

    ctx.run();

    vec![
    "#;

        let tuple = |x: String| format!("(\"{}\".to_string(), ctx.{}),", x, x);

        let all_tuples = self
            .all_columns()
            .into_iter()
            .map(tuple)
            .collect::<Vec<_>>()
            .join("\n");

        let execute = format!("{preamble}\n{all_tuples}\n]\n}}");
        self.code = format!("{}\n{execute}", self.code);
    }

    fn create_context_struct(&mut self) {
        let preamble = r#"
#[derive(Default, Debug)]
struct Context<F> {
    pub length: usize,
    pub current_row: usize,
    pub running: bool,

    pub fixed: HashMap<String, Vec<F>>,
        "#;

        let decl = |x: String| format!("pub {x}: Vec<F>,");

        let all_decls = self
            .all_columns()
            .into_iter()
            .map(decl)
            .collect::<Vec<_>>()
            .join("\n");

        let context = format!("{preamble}\n{all_decls}\n}}");

        self.code = format!("{}\n{context}", self.code);
    }

    fn all_columns(&self) -> Vec<String> {
        self.asgn_regs()
            .into_iter()
            .chain(self.state_regs().into_iter())
            .chain(vec![self.pc()].into_iter())
            .chain(self.asgn_reg_const_columns().into_iter())
            .chain(self.asgn_reg_free_value_columns().into_iter())
            .chain(self.asgn_reg_free_value_read_columns().into_iter())
            .chain(self.write_state_to_assignment_reg_columns().into_iter())
            .chain(self.write_assignment_to_state_reg_columns().into_iter())
            .chain(self.instruction_flags().into_iter())
            .collect()
    }

    fn asgn_reg_const_columns(&self) -> Vec<String> {
        self.asgn_regs().into_iter().map(asgn_reg_const).collect()
    }

    fn asgn_reg_free_value_columns(&self) -> Vec<String> {
        self.asgn_regs()
            .into_iter()
            .map(asgn_reg_free_value)
            .collect()
    }

    fn asgn_reg_free_value_read_columns(&self) -> Vec<String> {
        self.asgn_regs()
            .into_iter()
            .map(asgn_reg_free_value_read)
            .collect()
    }

    fn write_state_to_assignment_reg_columns(&self) -> Vec<String> {
        iproduct!(self.state_regs().into_iter(), self.asgn_regs().into_iter())
            .map(|(s, a)| write_state_to_assignment_reg_flag(s, a))
            .collect()
    }

    fn write_assignment_to_state_reg_columns(&self) -> Vec<String> {
        iproduct!(self.asgn_regs().into_iter(), self.state_regs().into_iter())
            .map(|(a, s)| write_assignment_to_state_reg_flag(a, s))
            .collect()
    }

    fn instruction_flags(&self) -> Vec<String> {
        self.machine
            .instructions
            .iter()
            .map(instruction_flag)
            .collect()
    }

    fn asgn_regs(&self) -> Vec<String> {
        self.machine
            .registers
            .iter()
            .filter(|r| matches!(r.ty, RegisterTy::Assignment))
            .map(|r| r.name.clone())
            .collect()
    }

    fn state_regs(&self) -> Vec<String> {
        self.machine
            .registers
            .iter()
            .filter(|r| matches!(r.ty, RegisterTy::Write))
            .map(|r| r.name.clone())
            .collect()
    }

    fn pc(&self) -> String {
        let pc_idx = self.machine.pc.unwrap();
        self.machine.registers[pc_idx].name.clone()
    }
}

fn instruction_flag<T>(instr: &InstructionDefinitionStatement<T>) -> String {
    format!("instr_{}", instr.name)
}

fn asgn_reg_const(asgn_reg: String) -> String {
    format!("{}_const", asgn_reg)
}

fn asgn_reg_free_value(asgn_reg: String) -> String {
    format!("{}_free_value", asgn_reg)
}

fn asgn_reg_free_value_read(asgn_reg: String) -> String {
    format!("{}_read_free", asgn_reg)
}

fn write_state_to_assignment_reg_flag(state_reg: String, asgn_reg: String) -> String {
    format!("read_{}_{}", asgn_reg, state_reg)
}

fn write_assignment_to_state_reg_flag(asgn_reg: String, state_reg: String) -> String {
    format!("reg_write_{}_{}", asgn_reg, state_reg)
}
