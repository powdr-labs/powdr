use crate::{analyzed::{Analyzed, Expression, FunctionValueDefinition}, parsed::{MatchPattern, LambdaExpression, FunctionCall}};

use super::{InstructionDefinitionStatement, Machine, RegisterTy};

use itertools::iproduct;
use powdr_number::FieldElement;

use std::collections::{HashMap, HashSet};

pub struct RustWitgen<T> {
    pub machine: Machine<T>,
    pub code: String,
    pub wit_cols: HashSet<String>,
    pub wit_cols_vec: Vec<String>,
    pub queries: HashMap<String, Expression<T>>
}

// TODO
// - use the inputs

// this impl handles translation from Machine to Rust-witgen
impl<T: FieldElement> RustWitgen<T> {
    pub fn new(pil: &Analyzed<T>, machine: Machine<T>) -> Self {
        let wit_cols_vec: Vec<_> = pil.committed_polys_in_source_order()
            .iter()
            .map(|c| c.0.absolute_name.split('.').last().unwrap().to_string())
            //.chain(builtin_columns().into_iter())
            .collect();
        let wit_cols = wit_cols_vec.clone().into_iter().collect();
        println!("wit_cols = {wit_cols:?}");

        let queries: HashMap<String, Expression<T>> = pil.committed_polys_in_source_order()
            .iter()
            .filter_map(|c| {
                if c.1.is_none() {
                    return None;
                }
                let name = c.0.absolute_name.split('.').last().unwrap().to_string();
                if name.ends_with("free_value") {
                    println!("name = {name}");
                    let e: Expression<T> = match c.1.as_ref().unwrap() {
                        FunctionValueDefinition::Query(Expression::LambdaExpression(LambdaExpression { params, body })) => (**body).clone(),
                        e => panic!("{e:?}")
                    };
                    Some((name, e))
                } else {
                    None
                }
            })
            .collect();

        Self {
            machine,
            code: String::new(),
            wit_cols,
            wit_cols_vec,
            queries
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
impl<'a, F: FieldElement> Context<'a, F> {
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
            \n{}
            \n{}
            \n{}
            }}",
            impl_,
            self.create_new(),
            self.create_fixed_methods(),
            self.create_run(),
            self.create_init(),
            self.create_empty_instructions(),
            self.create_run_instruction(),
            self.create_query(),
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
                "self.{}.push(self.fixed.get(\"main.p_{}\").unwrap()[pc as usize].clone());",
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
            .filter(|i| self.wit_cols.contains(i))
            .map(update)
            .collect::<Vec<_>>()
            .join("\n");

        format!("{}\n{}\n}}", preamble, updates)
    }

    fn create_update_control_flow_flags(&self) -> String {
        let update = r#"
fn update_control_flow_flags(&mut self) {
        self._operation_id.push(F::from(2));

        if self.current_row == 0 {
            self.instr__reset.push(F::one());
        } else {
            self.instr__reset.push(F::zero());
        }

        if self.current_row == 1 {
            self.instr__jump_to_operation.push(F::one());
        } else {
            self.instr__jump_to_operation.push(F::zero());
        }

        if self.running {
            self.instr__loop.push(F::zero());
        } else {
            self.instr__loop.push(F::one());
        }
    }
"#;
        update.to_string()
    }

    fn create_update_pc(&self) -> String {
        format!(
            r#"
#[allow(non_snake_case)]
fn update_pc(&mut self) {{
        let pc = self.{}.last().unwrap();
        let pc_prime = if self.instr__jump_to_operation.last().unwrap().is_one() {{
            self._operation_id.last().unwrap().clone()
        }} else if self.instr__loop.last().unwrap().is_one() {{
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
            .filter_map(|r| {
                let col = write_assignment_to_state_reg_flag(r.clone(), reg.clone());
                if self.wit_cols.contains(&col) { Some(
                    format!(
                        "if self.{}.last().unwrap() == &F::one() {{\nself.{}.push(self.{}.last().unwrap().clone());\n}}",
                        col,
                        reg.clone(),
                        r.clone()
                    )
                )} else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .join(" else ");

        let last = format!(" else if self.{}.len() < self.{}.len() {{\nself.{}.push(self.{}.last().cloned().unwrap_or_else(|| F::zero().clone()));\n}}", reg.clone(), self.pc(), reg.clone(), reg.clone());

        format!("{}\n{}", conds, last)
    }

    fn create_update_writes_to_assignment_registers(&self) -> String {
        let update = r#"
#[allow(non_snake_case)]
fn update_writes_to_assignment_registers(&mut self) {
    "#;

        let rhs = |col: String| {
            if self.wit_cols.contains(&col) {
                format!("self.{col}.last().unwrap().clone()")
            } else {
                "F::ZERO".to_string()
            }
        };
        let decls = self
            .asgn_regs()
            .into_iter()
            .map(|r| {
                format!(
                    "let mut {}_prime = {};",
                    r.clone(),
                    rhs(asgn_reg_const(r))
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        let conds = iproduct!(self.asgn_regs().into_iter(), self.state_regs().into_iter())
            .filter_map(|(a, s)| {
                let col = write_state_to_assignment_reg_flag(s.clone(), a.clone());
                if self.wit_cols.contains(&col) { Some(
                    format!(
                        "if self.{}.last().unwrap() == &F::one() {{\n{}_prime += *self.{}.last().unwrap();\n}}\n",
                        col,
                        a.clone(),
                        s.clone()
                    )
                )} else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        let read_conds = self.asgn_regs().into_iter()
            .filter_map(|a| {
                let col = asgn_reg_free_value_read(a.clone());
                if self.wit_cols.contains(&col) { Some(
                    format!(
                        "if self.{}.last().unwrap() == &F::one() {{ \n{}_prime += *self.{}.last().unwrap();\n}}\n",
                        col,
                        a.clone(),
                        asgn_reg_free_value(a),
                    )
                )} else {
                    None
                }
            })
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

    fn create_query(&self) -> String {
        r#"fn query(&self, query: &String) -> F {
            match (self.callback)(query).unwrap() {
                Some(val) => val,
                None => {
                    panic!("unknown query command: {query}");
                }
            }
        }"#.to_string()
    }

    fn create_update_inputs(&self) -> String {
        let init = r#"
        // for each assignment register
        fn update_inputs(&mut self) {"#;

        // TODO properly using QueryCallback
        let regs: Vec<_> = self
            .asgn_regs()
            .into_iter()
            .filter_map(|r| {
                let free_value = asgn_reg_free_value(r.clone());
                if !self.wit_cols.contains(&free_value) {
                    return None;
                }
                println!("free_value = {free_value}");

                if !self.queries.contains_key(&free_value) {
                    return Some(format!("self.{}.push(F::zero());", free_value));
                }

                let read = asgn_reg_free_value_read(r.clone());
                let query = self.queries.get(&free_value).unwrap();
                let inputs = create_free_value_query(query);

                let pc = "let pc = self.pc.last().unwrap();";

                let inner = inputs
                    .into_iter()
                    .map(|(row, q)| {
                        format!("if pc == &F::from({row}) {{\nself.query(&\"{q}\".to_string())\n}}")
                    })
                    .collect::<Vec<_>>()
                    .join(" else ");
                let inner = format!("{inner} else {{\nF::zero()\n}}");
                let outer = format!("let prime = if self.{}.last().unwrap() == &F::one() {{\n{}\n}} else {{\nF::zero()\n}};", read, inner);

                Some(format!("{pc}\n{outer}\nself.{}.push(prime);", free_value))
            })
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
        format!("fn instr_{}(&mut self) {{\n//TODO\n}}", instr)
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

    fn create_new(&self) -> String {
        let new = r#"
pub fn new(length: usize, callback: &'a Callback<F>) -> Self {
        Self {
            length,
            current_row: 0,
            running: false,
            callback,
            fixed: HashMap::new(),
            "#;

        let decl = |x: String| format!("{x}: Vec::new(),");
        let decls = self
            .all_columns()
            .into_iter()
            .map(decl)
            .collect::<Vec<_>>()
            .join("\n");

        format!("{}\n{decls}\n}}}}", new)
    }

    fn create_fixed_methods(&self) -> String {
        let fixed = r#"
    pub fn with_fixed(mut self, fixed: HashMap<String, Vec<F>>) -> Self {
        self.fixed = fixed;
        self
    }

    fn update(&mut self) {
        // order matters here:
        // - the starting point is pc = 0, state registers = 0
        // - update the control flow flags
        // - update the instruction flags
        // - update writes from state registers, contants, and free values into assignment registers
        // - run the instructions
        // - update writes from assignment registers into state registers
        // - all assignment registers should have been updated by now
        // - update pc

        self.update_control_flow_flags();
        self.update_flags();
        self.update_inputs();
        self.update_writes_to_assignment_registers();

        if self.current_row < self.length {
            self.run_instructions();
            self.update_writes_to_state_registers();
            if self.current_row < self.length - 1 {
                self.update_pc();
            }
        }
    }
"#;
        format!("{}", fixed)
    }

    fn create_run(&mut self) -> String {
    let preamble = r#"
    pub fn run(&mut self) {
        self.running = true;

        self.init();

        while self.current_row < self.length {
            self.update();
            self.current_row += 1;

            // Leo: can remove this for now
            if self.current_row >= 7 {
                self.running = false;
            }
        }

        // Leo: can remove this for now, maybe Georg's PR already solves it
        // TODO fix
"#;
        let last_updates = self
            .state_regs()
            .into_iter()
            .map(|s| format!("*self.{}.first_mut().unwrap() = self.{}.last().unwrap().clone();", s.clone(), s))
            .collect::<Vec<_>>()
            .join("\n");

        format!("{preamble}\n{last_updates}}}")
    }

    fn create_imports(&mut self) {
        let imports = r#"
use powdr_ast::analyzed::Analyzed;
use powdr_number::{FieldElement, GoldilocksField};

use num_traits::{One, Zero, ToBytes};

use std::collections::{BTreeMap, HashMap};
"#;
        self.code = format!("{}\n{}", imports, self.code);
    }

    fn create_execute(&mut self) {
        let preamble = r#"
pub fn execute<F: FieldElement>(
    length: usize,
    inputs: &Callback<F>,
    fixed: HashMap<String, Vec<F>>,
) -> Vec<(String, Vec<F>)> {

    println!("keys: {:?}", fixed.keys());

    let mut ctx = Context::new(length, inputs).with_fixed(fixed);

    ctx.run();

    vec![
    "#;

        let tuple = |x: &String| format!("(\"main.{}\".to_string(), ctx.{}),", x, x);

        println!("{:?}", self.wit_cols_vec);
        let all_tuples = self
            //.all_columns()
            .wit_cols_vec
            .iter()
            .map(tuple)
            .collect::<Vec<_>>()
            .join("\n");

        let execute = format!("{preamble}\n{all_tuples}\n]\n}}");
        self.code = format!("{}\n{execute}", self.code);
    }

    fn create_context_struct(&mut self) {
        let preamble = r#"
type Callback<'a, F> = dyn powdr_executor::witgen::QueryCallback<F> + 'a;

#[allow(non_snake_case)]
struct Context<'a, F> {
    pub length: usize,
    pub current_row: usize,
    pub running: bool,

    pub callback: &'a Callback<'a, F>,

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
            .chain(builtin_columns().into_iter())
            .filter(|i| self.wit_cols.contains(i))
            .collect()
    }

    fn asgn_reg_const_columns(&self) -> Vec<String> {
        self
            .asgn_regs()
            .into_iter()
            .map(asgn_reg_const)
            .filter(|i| self.wit_cols.contains(i))
            .collect()
    }

    fn asgn_reg_free_value_columns(&self) -> Vec<String> {
        self.asgn_regs()
            .into_iter()
            .map(asgn_reg_free_value)
            .filter(|i| self.wit_cols.contains(i))
            .collect()
    }

    fn asgn_reg_free_value_read_columns(&self) -> Vec<String> {
        self.asgn_regs()
            .into_iter()
            .map(asgn_reg_free_value_read)
            .filter(|i| self.wit_cols.contains(i))
            .collect()
    }

    fn write_state_to_assignment_reg_columns(&self) -> Vec<String> {
        iproduct!(self.state_regs().into_iter(), self.asgn_regs().into_iter())
            .map(|(s, a)| write_state_to_assignment_reg_flag(s, a))
            .filter(|i| self.wit_cols.contains(i))
            .collect()
    }

    fn write_assignment_to_state_reg_columns(&self) -> Vec<String> {
        iproduct!(self.asgn_regs().into_iter(), self.state_regs().into_iter())
            .map(|(a, s)| write_assignment_to_state_reg_flag(a, s))
            .filter(|i| self.wit_cols.contains(i))
            .collect()
    }

    fn instruction_flags(&self) -> Vec<String> {
        self.machine
            .instructions
            .iter()
            .map(instruction_flag)
            .filter(|i| self.wit_cols.contains(i))
            .collect()
    }

    fn asgn_regs(&self) -> Vec<String> {
        self.machine
            .registers
            .iter()
            .filter(|r| matches!(r.ty, RegisterTy::Assignment))
            .map(|r| r.name.clone())
            .filter(|i| self.wit_cols.contains(i))
            .collect()
    }

    fn state_regs(&self) -> Vec<String> {
        self.machine
            .registers
            .iter()
            .filter(|r| matches!(r.ty, RegisterTy::Write))
            .map(|r| r.name.clone())
            .filter(|i| self.wit_cols.contains(i))
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

fn builtin_columns() -> Vec<String> {
    vec![
        "instr__jump_to_operation".to_string(),
        "_operation_id".to_string(),
        "instr__loop".to_string(),
        "instr__reset".to_string(),
    ]
}

fn create_free_value_query<F: FieldElement>(expression: &Expression<F>) -> Vec<(F, String)> {
        match expression {
            /*
            Expression::Reference(r) => {
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
            */
            /*
            Expression::FreeInput(expr) => {
                if let Expression::Tuple(t) = &**expr {
                    let mut all_strings: Vec<String> = Vec::new();
                    for expr in t {
                        if let Expression::String(_) = expr {
                            all_strings.push(expr.to_string());
                        } else {
                            let val = self.eval_expression(expr)[0];
                            all_strings.push(val.to_string());
                        }
                    }
                    let query = format!("({})", all_strings.join(","));
                    match (self.inputs)(&query).unwrap() {
                        Some(val) => vec![Elem::new_from_fe_as_bin(&val)],
                        None => {
                            panic!("unknown query command: {query}");
                        }
                    }
                } else {
                    panic!("does not match IO pattern")
                }
            }
            */
            Expression::MatchExpression(expr, arms) => {
                // we assume the expr is `main.pc(i)`
                // TODO: assert the above
                // we also assume each arm to have the form `literal => ("key", reg(i))`
                // here we are interested in the row and building the query string, that is
                // (literal, ("key", reg))
                arms
                    .iter()
                    .map(|a| {
                        println!("Pattern = {:?}", a.pattern);
                        println!("Value = {:?}", a.value);
                        let row = match a.pattern {
                            MatchPattern::Pattern(Expression::Number(n, None)) => n,
                            _ => panic!()
                        };
                        let value = match &a.value {
                            Expression::Number(n, None) => format!("{n}"),
                            Expression::FunctionCall(FunctionCall { function, arguments }) => todo!(),
                            Expression::Tuple(t) => {
                                println!("Tuple = {:?}", t);
                                let mut all_strings: Vec<String> = Vec::new();
                                for expr in t {
                                    if let Expression::String(_) = expr {
                                        all_strings.push(expr.to_string());
                                    } else {
                                        let val = expr.to_string();
                                        all_strings.push(val);
                                    }
                                }
                                let res = format!("({})", all_strings.join(","));
                                let res = res .replace("\"", "\\\"");
                                println!("res = {res}");
                                res
                            }
                            e => panic!("{e:?}")
                        };
                        (row, value)

                    })
                    .collect()
            }
            e => panic!("{e:?}")
        }
    }

