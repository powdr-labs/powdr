#![allow(clippy::print_stdout)]

use crate::{
    analyzed::{Analyzed, Expression, FunctionValueDefinition},
    parsed::{FunctionCall, LambdaExpression, MatchPattern},
};

use super::{InstructionDefinitionStatement, Machine, RegisterTy};

use itertools::iproduct;
use powdr_number::FieldElement;

use std::collections::{HashMap, HashSet};

pub struct RustWitgen<T> {
    pub machine: Machine<T>,
    pub code: String,
    pub wit_cols: HashSet<String>,
    pub wit_cols_vec: Vec<String>,
    pub fixed_cols: HashSet<String>,
    pub queries: HashMap<String, Expression<T>>,
}

// TODO
// - use the inputs

const NOT_SUPPORTED: [&str; 25] = [
    // memory machine
    "m_addr",
    "m_step",
    "m_change",
    "m_value",
    "m_is_write",
    "m_is_read",
    "m_diff_lower",
    "m_diff_upper",
    // inverse helpers
    "XInv",
    "XIsZero",
    // byte decomposition helpers
    "X_b1",
    "X_b2",
    "X_b3",
    "X_b4",
    "Y_b5",
    "Y_b6",
    "Y_b7",
    "Y_b8",
    "wrap_bit",
    "Y_7bit",
    "Y_15bit",
    "REM_b1",
    "REM_b2",
    "REM_b3",
    "REM_b4",
];

// this impl handles translation from Machine to Rust-witgen
impl<T: FieldElement> RustWitgen<T> {
    pub fn new(pil: &Analyzed<T>, machine: Machine<T>) -> Self {
        let fixed_cols: HashSet<_> = pil
            .constant_polys_in_source_order()
            .iter()
            .map(|c| c.0.absolute_name.clone())
            .collect();

        let wit_cols_vec: Vec<_> = pil
            .committed_polys_in_source_order()
            .iter()
            .filter_map(|c| {
                let name = c.0.absolute_name.clone();
                if name.starts_with("main.") {
                    let sname = name.split('.').last().unwrap();
                    if NOT_SUPPORTED.contains(&sname) {
                        None
                    } else {
                        Some(sname.to_string())
                    }
                } else {
                    None
                }
            })
            .collect();
        let wit_cols = wit_cols_vec.clone().into_iter().collect();
        println!("wit_cols = {wit_cols:?}");

        let queries: HashMap<String, Expression<T>> = pil
            .committed_polys_in_source_order()
            .iter()
            .filter_map(|c| {
                c.1.as_ref()?;
                let name = c.0.absolute_name.split('.').last().unwrap().to_string();
                if name.ends_with("free_value") {
                    println!("name = {name}");
                    let e: Expression<T> = match c.1.as_ref().unwrap() {
                        FunctionValueDefinition::Query(Expression::LambdaExpression(
                            LambdaExpression {
                                params: _params,
                                body,
                            },
                        )) => (**body).clone(),
                        e => panic!("{e:?}"),
                    };
                    Some((name, e))
                } else {
                    None
                }
            })
            .collect();

        println!("NEWWWW");

        Self {
            machine,
            code: String::new(),
            wit_cols,
            wit_cols_vec,
            fixed_cols,
            queries,
        }
    }

    pub fn generate(&mut self) {
        println!("generate");
        self.create_imports();
        println!("created imports");
        self.create_execute();
        println!("created execute");
        self.create_context_struct();
        println!("created struct");
        self.create_proc_impl();
        println!("created proc impl");
        self.create_impl();
        println!("created impl");
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
            .bin();
        "#,
            self.pc()
        );

        let update = |x| {
            format!(
                "self.{}.push(Elem::new_from_fe_as_bin(&self.fixed.get(\"main.p_{}\").unwrap()[pc as usize]));",
                x, x
            )
        };

        let updates = self
            .wit_cols_vec
            .iter()
            .filter(|i| self.fixed_cols.contains(&format!("main.p_{}", i)))
            .map(update)
            .collect::<Vec<_>>()
            .join("\n");

        format!("{}\n{}\n}}", preamble, updates)
    }

    fn create_set_regs(&self) -> String {
        self.state_regs()
            .into_iter()
            .map(|r| format!("\"{}\" => self.{}.push(value.into()),", r, r))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn create_get_regs(&self) -> String {
        self.state_regs()
            .into_iter()
            .map(|r| format!("\"{}\" => *self.{}.last().unwrap(),", r, r))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn create_proc_impl(&mut self) {
        let proc = r#"
impl<'a, F: FieldElement> Proc<F> for Context<'a, F> {
    fn get_pc(&self) -> Elem<F> {
        // TODO use {} -> self.pc
        *self.pc.last().unwrap()
    }
    fn set_pc(&mut self, pc: Elem<F>) {
        self.pc.push(pc);
    }
    fn get_mem(&self, addr: u32) -> u32 {
        *self.mem.get(&addr).unwrap_or(&0)
    }
    fn set_mem(&mut self, addr: u32, val: u32) {
        self.mem.insert(addr, val);
    }
"#;

        let reg_gets = format!(
            r#"
    fn get_reg(&self, name: &str) -> Elem<F> {{
        match name {{
            {}
            _ => panic!("unknown register: {{}}", name),
        }}
    }}"#,
            self.create_get_regs()
        );

        let reg_sets = format!(
            r#"
    fn set_reg(&mut self, idx: &str, value: impl Into<Elem<F>>) {{
        match idx {{
            {}
            _ => panic!("unknown register: {{}}", idx),
        }}
    }}"#,
            self.create_set_regs()
        );

        let proc_impl = format!("{}\n{}\n{}}}", proc, reg_gets, reg_sets);

        self.code = format!("{}\n{proc_impl}", self.code);
    }

    fn create_update_control_flow_flags(&self) -> String {
        let update = r#"
fn update_control_flow_flags(&mut self) {
        // TODO: automate
        if self.current_row == 0 {
            self._operation_id.push(2.into());
        } else if self.instr_return.last().unwrap().is_one() {
            // TODO: read the number from _operation_id hint
            self._operation_id.push(2191.into());
        } else {
            self._operation_id.push(*self._operation_id.last().unwrap());
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
        if self.instr__jump_to_operation.last().unwrap().is_one() {{
            self.{}.push(*self._operation_id.last().unwrap());
        }} else if self.instr__loop.last().unwrap().is_one() {{
            self.{}.push(*pc);
        }} else if self.instr_return.last().unwrap().is_one() {{
            self.{}.push(0.into());
        }} else if self.current_row + 1 == self.{}.len() {{
            self.{}.push(Elem::Binary(pc.bin() + 1));
        }};
    }}"#,
            self.pc(),
            self.pc(),
            self.pc(),
            self.pc(),
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
        let last = format!("if self.{}.len() <= self.{}.len() {{\nself.{}.push(self.{}.last().cloned().unwrap_or_else(|| 0.into()));\n}}", reg.clone(), self.pc(), reg.clone(), reg.clone());
        let reset = format!(
            "if self.instr__reset.last().unwrap().is_one() {{\nself.{}.push(0.into());\n}}",
            reg.clone()
        );

        let conds = self
            .asgn_regs()
            .into_iter()
            .filter_map(|r| {
                let col = write_assignment_to_state_reg_flag(r.clone(), reg.clone());
                if self.wit_cols.contains(&col) { Some(
                    format!(
                        "if self.{}.last().unwrap().is_one() {{\nself.{}.push(*self.{}.last().unwrap());\n}}",
                        col,
                        reg.clone(),
                        r.clone()
                    )
                )} else {
                    None
                }
            })
            .chain([reset, last])
            .collect::<Vec<_>>()
            .join(" else ");

        conds.to_string()
    }

    fn create_update_writes_to_assignment_registers(&self) -> String {
        let update = r#"
#[allow(non_snake_case)]
fn update_writes_to_assignment_registers(&mut self) {
    "#;

        let rhs = |col: String| {
            if self.wit_cols.contains(&col) {
                format!("self.{col}.last().unwrap().bin()")
            } else {
                "0".to_string()
            }
        };
        let decls = self
            .asgn_regs()
            .into_iter()
            .map(|r| {
                format!(
                    "let mut {}_prime: i64 = {};",
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
                        "\n{}_prime += self.{}.last().unwrap().bin() * self.{}.last().unwrap().bin();",
                        a.clone(),
                        col,
                        s.clone()
                    )
                )} else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        let read_conds = self
            .asgn_regs()
            .into_iter()
            .filter_map(|a| {
                let col = asgn_reg_free_value_read(a.clone());
                if self.wit_cols.contains(&col) {
                    Some(format!(
                        "{}_prime += self.{}.last().unwrap().bin();\n",
                        a.clone(),
                        asgn_reg_free_value(a),
                    ))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        let pushes = self
            .asgn_regs()
            .into_iter()
            .map(|r| format!("self.{}.push(Elem::Binary({}_prime));", r, r))
            .collect::<Vec<_>>()
            .join("\n");

        format!(
            "{}\n{}\n{}\n{}\n{}\n}}",
            update, decls, conds, read_conds, pushes
        )
    }

    fn create_query(&self) -> String {
        r#"fn query(&self, query: &String) -> Elem<F> {
            match (self.callback)(query).unwrap() {
                Some(val) => Elem::new_from_fe_as_bin(&val),
                None => {
                    panic!("unknown query command: {query}");
                }
            }
        }"#
        .to_string()
    }

    fn create_update_inputs(&self) -> String {
        let init = r#"
        // for each assignment register
        fn update_inputs(&mut self) {"#;

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
                    return Some(format!("self.{}.push(0.into());", free_value));
                }

                let read = asgn_reg_free_value_read(r.clone());
                let query = self.queries.get(&free_value).unwrap();
                let inputs = create_free_value_query(query);

                let pc = "let pc = self.pc.last().unwrap().bin();";

                let inner = inputs
                    .into_iter()
                    .map(|(row, q)| {
                        format!("if pc == {row} {{\nself.query({q})}}")
                    })
                    .collect::<Vec<_>>()
                    .join(" else ");
                let inner = format!("{inner} else {{\n0.into()\n}}");
                let outer = format!("let prime = if self.{}.last().unwrap().is_one() {{\n{}\n}} else {{\n0.into()\n}};", read, inner);

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
            .map(|i| format!("if {}.is_one() {{\nself.instr_{}();\n}} else ", i, i))
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
            .chain(vec![self.pc()])
            .map(|r| format!("self.{}.push(0.into());", r))
            .collect();

        format!("{}\n{}\n}}", init, regs.join("\n"))
    }

    fn create_new(&self) -> String {
        let new = r#"
pub fn new(length: usize, callback: &'a Callback<F>) -> Self {
        Self {
            length,
            current_row: 0,
            callback,
            mem: HashMap::new(),
            fixed: HashMap::new(),
            "#;

        let decl = |x: &String| format!("{x}: Vec::new(),");
        let decls = self
            .wit_cols_vec
            .iter()
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

        if self.current_row < self.length - 1 {
            self.run_instructions();
            self.update_writes_to_state_registers();
            self.update_pc();
        }
    }
"#;
        fixed.to_string()
    }

    fn create_run(&mut self) -> String {
        let preamble = r#"
    pub fn run(&mut self) {
        self.init();

        while self.current_row < self.length {
            self.update();
            self.current_row += 1;
        }

        // Leo: can remove this for now, maybe Georg's PR already solves it
        // TODO fix
"#;
        let last_updates = self
            .state_regs()
            .into_iter()
            .map(|s| {
                format!(
                    "*self.{}.first_mut().unwrap() = *self.{}.last().unwrap();",
                    s.clone(),
                    s
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        format!("{preamble}\n{last_updates}}}")
    }

    fn create_imports(&mut self) {
        let imports = r#"
use powdr_number::FieldElement;

use std::collections::HashMap;

use crate::Elem;
use crate::instr::{Proc, exec_instruction};
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

        let tuple = |x: &String| {
            format!(
                "(\"main.{}\".to_string(), ctx.{}.iter().map(|x| F::from(x.bin())).collect()),",
                x, x
            )
        };

        println!("{:?}", self.wit_cols_vec);
        let all_tuples = self
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
struct Context<'a, F: FieldElement> {
    pub length: usize,
    pub current_row: usize,

    pub callback: &'a Callback<'a, F>,

    pub mem: HashMap<u32, u32>,

    pub fixed: HashMap<String, Vec<F>>,
        "#;

        let decl = |x: &String| format!("pub {x}: Vec<Elem<F>>,");

        let all_decls = self
            .wit_cols_vec
            .iter()
            .map(decl)
            .collect::<Vec<_>>()
            .join("\n");

        let context = format!("{preamble}\n{all_decls}\n}}");

        self.code = format!("{}\n{context}", self.code);
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
        Expression::MatchExpression(_expr, arms) => {
            // we assume the expr is `main.pc(i)`
            // TODO: assert the above
            // we also assume each arm to have the form `literal => ("key", reg(i))`
            // here we are interested in the row and building the query string, that is
            // (literal, ("key", reg))
            arms.iter()
                .map(|a| {
                    println!("Pattern = {:?}", a.pattern);
                    println!("Value = {:?}", a.value);
                    let row = match a.pattern {
                        MatchPattern::Pattern(Expression::Number(n, _)) => n,
                        _ => panic!(),
                    };
                    let value = match &a.value {
                        Expression::Number(n, None) => format!("{n}"),
                        Expression::FunctionCall(FunctionCall {
                            function: _function,
                            arguments: _arguments,
                        }) => todo!(),
                        Expression::Tuple(t) => {
                            println!("Tuple = {:?}", t);
                            let mut all_strings: Vec<String> = Vec::new();
                            for expr in t {
                                if let Expression::String(_) = expr {
                                    all_strings.push(expr.to_string());
                                } else {
                                    // TODO pattern match all of this properly
                                    //let val = expr.to_string();
                                    let val = match expr {
                                        Expression::FunctionCall(FunctionCall {
                                            function,
                                            arguments,
                                        }) => {
                                            println!("Function = {:?}", function);
                                            println!("Arguments = {:?}", arguments);
                                            let name = arguments[0].to_string();
                                            assert!(name.starts_with("main."));
                                            let name = name.split('.').last().unwrap();
                                            // name should be a register name now
                                            let val = format!("self.{}.last().unwrap()", name);
                                            val
                                        }
                                        _ => panic!(),
                                    };
                                    all_strings.push(val);
                                }
                            }
                            //let res = format!("({})", all_strings.join(","));
                            let cmd = all_strings[0].replace('\"', "\\\"");
                            let brackets = (1..all_strings.len())
                                .map(|_| "{}")
                                .collect::<Vec<_>>()
                                .join(", ");
                            let res = format!(
                                "&format!(\"({}, {})\", {})",
                                cmd,
                                brackets,
                                all_strings[1..].join(", ")
                            );
                            println!("res = {res}");
                            res
                        }
                        e => panic!("{e:?}"),
                    };
                    (row, value)
                })
                .collect()
        }
        e => panic!("{e:?}"),
    }
}
