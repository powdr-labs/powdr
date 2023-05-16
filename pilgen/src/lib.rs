//! Compilation from powdr assembly to PIL

use std::collections::{BTreeMap, HashMap};

use number::DegreeType;
use number::FieldElement;

use parser::asm_ast::*;
use parser::ast::*;
use parser_util::ParseError;

pub fn compile<'a, T: FieldElement>(
    file_name: Option<&str>,
    input: &'a str,
) -> Result<PILFile<T>, ParseError<'a>> {
    parser::parse_asm(file_name, input).map(|ast| ASMPILConverter::new().convert(ast))
}

#[derive(Default)]
struct ASMPILConverter<T> {
    degree: DegreeType,
    pil: Vec<Statement<T>>,
    pc_name: Option<String>,
    registers: BTreeMap<String, Register<T>>,
    instructions: BTreeMap<String, Instruction>,
    code_lines: Vec<CodeLine<T>>,
    /// Pairs of columns that are used in the connecting plookup
    line_lookup: Vec<(String, String)>,
    /// Names of fixed columns that contain the program.
    program_constant_names: Vec<String>,
}

impl<T: FieldElement> ASMPILConverter<T> {
    fn new() -> Self {
        Default::default()
    }

    fn set_degree(&mut self, degree: DegreeType) {
        // check the degree is a power of 2
        assert!(
            degree.is_power_of_two(),
            "Degree should be a power of two, found {degree}",
        );
        self.degree = degree;
    }

    fn convert(&mut self, input: ASMFile<T>) -> PILFile<T> {
        self.set_degree(1024);

        let mut statements = input.0.into_iter().peekable();

        if let Some(ASMStatement::Degree(_, degree)) = statements.peek() {
            self.set_degree(T::from(degree.clone()).to_degree());
            statements.next();
        }

        self.pil.push(Statement::Namespace(
            0,
            "Assembly".to_string(),
            Expression::Number(self.degree.into()),
        ));
        self.pil.push(Statement::PolynomialConstantDefinition(
            0,
            "first_step".to_string(),
            FunctionDefinition::Array(
                ArrayExpression::value(vec![build_number(1u64)]).pad_with_zeroes(),
            ),
        ));

        for statement in statements {
            match statement {
                ASMStatement::Degree(..) => {
                    panic!("The degree statement is only supported at the start of the asm source");
                }
                ASMStatement::RegisterDeclaration(start, name, flags) => {
                    self.handle_register_declaration(flags, &name, start);
                }
                ASMStatement::InstructionDeclaration(start, name, params, body) => {
                    self.handle_instruction_def(start, body, name, params);
                }
                ASMStatement::InlinePil(_start, statements) => self.pil.extend(statements.clone()),
                ASMStatement::Assignment(start, write_regs, assign_reg, value) => match *value {
                    Expression::FunctionCall(function_name, args) => {
                        self.handle_functional_instruction(
                            write_regs,
                            assign_reg.unwrap(),
                            function_name,
                            args,
                        );
                    }
                    _ => {
                        self.handle_assignment(start, write_regs, assign_reg, *value);
                    }
                },
                ASMStatement::Instruction(_start, instr_name, args) => {
                    self.handle_instruction(instr_name, args)
                }
                ASMStatement::Label(_start, name) => self.code_lines.push(CodeLine {
                    label: Some(name.clone()),
                    ..Default::default()
                }),
            }
        }
        let assignment_registers = self.assignment_registers().cloned().collect::<Vec<_>>();
        for reg in assignment_registers {
            self.create_constraints_for_assignment_reg(reg);
        }

        self.pil.extend(
            self.registers
                .iter()
                .filter_map(|(name, reg)| {
                    reg.update_expression().map(|mut update| {
                        if Some(name) == self.pc_name.as_ref() {
                            // Force pc to zero on first row.
                            update = build_mul(
                                build_sub(build_number(1u64), next_reference("first_step")),
                                update,
                            )
                        }

                        (name, update)
                    })
                })
                .map(|(name, update)| {
                    Statement::PolynomialIdentity(0, build_sub(next_reference(name), update))
                }),
        );

        self.translate_code_lines();

        self.pil.push(Statement::PlookupIdentity(
            0,
            SelectedExpressions {
                selector: None,
                expressions: self
                    .line_lookup
                    .iter()
                    .map(|x| direct_reference(&x.0))
                    .collect(),
            },
            SelectedExpressions {
                selector: None,
                expressions: self
                    .line_lookup
                    .iter()
                    .map(|x| direct_reference(&x.1))
                    .collect(),
            },
        ));

        PILFile(std::mem::take(&mut self.pil))
    }

    fn handle_register_declaration(
        &mut self,
        flags: Option<RegisterFlag>,
        name: &str,
        start: usize,
    ) {
        let mut conditioned_updates = vec![];
        let mut default_update = None;
        match flags {
            Some(RegisterFlag::IsPC) => {
                assert_eq!(self.pc_name, None);
                self.pc_name = Some(name.to_string());
                self.line_lookup
                    .push((name.to_string(), "p_line".to_string()));
                default_update = Some(build_add(direct_reference(name), build_number(1u64)));
            }
            Some(RegisterFlag::IsAssignment) => {
                // no updates
            }
            None => {
                // This might be superfluous but makes it easier to determine that the register needs to
                // be zero in the first row.
                self.pil.push(Statement::PolynomialIdentity(
                    start,
                    build_mul(direct_reference("first_step"), direct_reference(name)),
                ));
                conditioned_updates = vec![
                    // The value here is actually irrelevant, it is only important
                    // that "first_step'" is included to compute the "default condition"
                    (next_reference("first_step"), build_number(0u64)),
                ];
                let assignment_regs = self.assignment_registers().cloned().collect::<Vec<_>>();
                // TODO do this at the same place where we set up the read flags.
                for reg in assignment_regs {
                    let write_flag = format!("reg_write_{reg}_{name}");
                    self.create_witness_fixed_pair(start, &write_flag);
                    conditioned_updates
                        .push((direct_reference(&write_flag), direct_reference(&reg)));
                }
                default_update = Some(direct_reference(name));
            }
        };
        self.registers.insert(
            name.to_string(),
            Register {
                conditioned_updates,
                default_update,
                is_assignment: flags == Some(RegisterFlag::IsAssignment),
            },
        );
        self.pil.push(witness_column(start, name, None));
    }

    fn handle_instruction_def(
        &mut self,
        start: usize,
        body: Vec<InstructionBodyElement<T>>,
        name: String,
        params: InstructionParams,
    ) {
        let instruction_flag = format!("instr_{name}");
        self.create_witness_fixed_pair(start, &instruction_flag);
        // it's part of the lookup!
        //self.pil.push(constrain_zero_one(&col_name));

        let inputs: Vec<_> = params
            .inputs
            .params
            .into_iter()
            .map(|param| match param.ty {
                Some(ty) if ty == "label" => Input::Label(param.name),
                None => Input::Register(param.name),
                Some(ty) => panic!("param type must be nothing or label, found `{ty}`"),
            })
            .collect();

        let outputs = params
            .outputs
            .map(|outputs| {
                outputs
                    .params
                    .into_iter()
                    .map(|param| {
                        assert!(param.ty.is_none(), "output must be a register");
                        param.name
                    })
                    .collect()
            })
            .unwrap_or_default();

        let instr = Instruction { inputs, outputs };

        let substitutions = instr
            .labels()
            .map(|label| {
                // label
                let param_col_name = format!("instr_{name}_param_{label}");
                self.create_witness_fixed_pair(start, &param_col_name);
                (label.clone(), param_col_name)
            })
            .collect();

        for expr in body {
            match expr {
                InstructionBodyElement::Expression(expr) => {
                    let expr = substitute(expr, &substitutions);
                    match extract_update(expr) {
                        (Some(var), expr) => {
                            self.registers
                                .get_mut(&var)
                                .unwrap()
                                .conditioned_updates
                                .push((direct_reference(&instruction_flag), expr));
                        }
                        (None, expr) => self.pil.push(Statement::PolynomialIdentity(
                            0,
                            build_mul(direct_reference(&instruction_flag), expr.clone()),
                        )),
                    }
                }
                InstructionBodyElement::PlookupIdentity(left, op, right) => {
                    assert!(left.selector.is_none(), "LHS selector not supported, could and-combine with instruction flag later.");
                    let left = SelectedExpressions {
                        selector: Some(direct_reference(&instruction_flag)),
                        expressions: substitute_vec(left.expressions, &substitutions),
                    };
                    let right = substitute_selected_exprs(right, &substitutions);
                    self.pil.push(match op {
                        PlookupOperator::In => Statement::PlookupIdentity(start, left, right),
                        PlookupOperator::Is => Statement::PermutationIdentity(start, left, right),
                    })
                }
            }
        }
        self.instructions.insert(name, instr);
    }

    fn handle_assignment(
        &mut self,
        _start: usize,
        write_regs: Vec<String>,
        assign_reg: Option<String>,
        value: Expression<T>,
    ) {
        assert!(write_regs.len() <= 1);
        assert!(
            assign_reg.is_some(),
            "Implicit assign register not yet supported."
        );
        let assign_reg = assign_reg.unwrap();
        let value = self.process_assignment_value(value);
        self.code_lines.push(CodeLine {
            write_regs: [(assign_reg.clone(), write_regs)].into_iter().collect(),
            value: [(assign_reg, value)].into(),
            ..Default::default()
        })
    }

    fn handle_functional_instruction(
        &mut self,
        write_regs: Vec<String>,
        assign_reg: String,
        instr_name: String,
        args: Vec<Expression<T>>,
    ) {
        assert!(write_regs.len() == 1);
        let instr = &self
            .instructions
            .get(&instr_name)
            .unwrap_or_else(|| panic!("Intruction not found: {instr_name}"));
        assert_eq!(instr.outputs.len(), 1);
        let output = instr.outputs[0].clone();
        assert!(
            output == assign_reg,
            "The instruction {instr_name} uses the assignment register {output}, but the caller uses {assign_reg} to further process the value.",
        );

        let mut args = args;
        args.push(direct_reference(write_regs.first().unwrap().clone()));
        self.handle_instruction(instr_name, args);
    }

    fn handle_instruction(&mut self, instr_name: String, args: Vec<Expression<T>>) {
        let instr = &self
            .instructions
            .get(&instr_name)
            .unwrap_or_else(|| panic!("Instruction not found: {instr_name}"));
        assert_eq!(instr.inputs.len() + instr.outputs.len(), args.len());

        let mut args = args.into_iter();

        let (value, instruction_literal_args): (BTreeMap<_, _>, Vec<_>) =
            instr.inputs.iter().zip(&mut args).fold(
                Default::default(),
                |(mut value, mut instruction_literal_arg), (input, a)| {
                    match input {
                        Input::Register(reg) => {
                            // We read a value into the assignment register "reg".
                            assert!(!value.contains_key(reg));
                            value.insert(reg.clone(), self.process_assignment_value(a));
                        }
                        Input::Label(_) => {
                            if let Expression::PolynomialReference(r) = a {
                                instruction_literal_arg.push(r.name);
                            } else {
                                panic!();
                            }
                        }
                    };
                    (value, instruction_literal_arg)
                },
            );

        let write_regs: BTreeMap<_, _> = instr
            .outputs
            .iter()
            .zip(&mut args)
            .map(|(reg, a)| {
                // Output a value trough assignment register "reg"
                if let Expression::PolynomialReference(r) = a {
                    assert!(!r.next);
                    assert!(r.index.is_none());
                    (reg.clone(), vec![r.name])
                } else {
                    panic!("Expected direct register to assign to in instruction call.");
                }
            })
            .collect();

        assert_eq!(write_regs.len(), instr.outputs.len());

        self.code_lines.push(CodeLine {
            write_regs,
            instruction: Some(instr_name.to_string()),
            value,
            instruction_literal_args,
            ..Default::default()
        });
    }

    fn process_assignment_value(
        &self,
        value: Expression<T>,
    ) -> Vec<(T, AffineExpressionComponent<T>)> {
        match value {
            Expression::Constant(_) => panic!(),
            Expression::PublicReference(_) => panic!(),
            Expression::FunctionCall(_, _) => panic!(),
            Expression::PolynomialReference(reference) => {
                assert!(reference.namespace.is_none());
                assert!(reference.index.is_none());
                assert!(!reference.next);
                // TODO check it actually is a register
                vec![(
                    1.into(),
                    AffineExpressionComponent::Register(reference.name),
                )]
            }
            Expression::Number(value) => vec![(value, AffineExpressionComponent::Constant)],
            Expression::String(_) => panic!(),
            Expression::Tuple(_) => panic!(),
            Expression::MatchExpression(_, _) => panic!(),
            Expression::FreeInput(expr) => {
                vec![(1.into(), AffineExpressionComponent::FreeInput(*expr))]
            }
            Expression::BinaryOperation(left, op, right) => match op {
                BinaryOperator::Add => self.add_assignment_value(
                    self.process_assignment_value(*left),
                    self.process_assignment_value(*right),
                ),
                BinaryOperator::Sub => self.add_assignment_value(
                    self.process_assignment_value(*left),
                    self.negate_assignment_value(self.process_assignment_value(*right)),
                ),
                BinaryOperator::Mul => {
                    let left = self.process_assignment_value(*left);
                    let right = self.process_assignment_value(*right);
                    if let [(f, AffineExpressionComponent::Constant)] = &left[..] {
                        // TODO overflow?
                        right
                            .into_iter()
                            .map(|(coeff, comp)| (*f * coeff, comp))
                            .collect()
                    } else if let [(f, AffineExpressionComponent::Constant)] = &right[..] {
                        // TODO overflow?
                        left.into_iter()
                            .map(|(coeff, comp)| (*f * coeff, comp))
                            .collect()
                    } else {
                        panic!("Multiplication by non-constant.");
                    }
                }
                BinaryOperator::Pow => {
                    let left = self.process_assignment_value(*left);
                    let right = self.process_assignment_value(*right);
                    if let (
                        [(l, AffineExpressionComponent::Constant)],
                        [(r, AffineExpressionComponent::Constant)],
                    ) = (&left[..], &right[..])
                    {
                        // TODO overflow?
                        if r.to_arbitrary_integer() > (u32::MAX).into() {
                            panic!("Exponent too large");
                        }
                        vec![(l.pow(r.to_integer()), AffineExpressionComponent::Constant)]
                    } else {
                        panic!("Exponentiation of non-constants.");
                    }
                }
                BinaryOperator::Div
                | BinaryOperator::Mod
                | BinaryOperator::BinaryAnd
                | BinaryOperator::BinaryXor
                | BinaryOperator::BinaryOr
                | BinaryOperator::ShiftLeft
                | BinaryOperator::ShiftRight => {
                    panic!("Invalid operation in expression {left} {op} {right}")
                }
            },
            Expression::UnaryOperation(op, expr) => {
                assert!(op == UnaryOperator::Minus);
                self.negate_assignment_value(self.process_assignment_value(*expr))
            }
        }
    }

    fn add_assignment_value(
        &self,
        mut left: Vec<(T, AffineExpressionComponent<T>)>,
        right: Vec<(T, AffineExpressionComponent<T>)>,
    ) -> Vec<(T, AffineExpressionComponent<T>)> {
        // TODO combine (or at leats check for) same components.
        left.extend(right);
        left
    }

    fn negate_assignment_value(
        &self,
        expr: Vec<(T, AffineExpressionComponent<T>)>,
    ) -> Vec<(T, AffineExpressionComponent<T>)> {
        expr.into_iter().map(|(v, c)| (-v, c)).collect()
    }

    fn create_constraints_for_assignment_reg(&mut self, register: String) {
        let assign_const = format!("{register}_const");
        self.create_witness_fixed_pair(0, &assign_const);
        let read_free = format!("{register}_read_free");
        self.create_witness_fixed_pair(0, &read_free);
        let free_value = format!("{register}_free_value");
        let regular_registers = self.regular_registers().cloned().collect::<Vec<_>>();
        let assign_constraint = regular_registers
            .iter()
            .map(|name| {
                let read_coefficient = format!("read_{register}_{name}");
                self.create_witness_fixed_pair(0, &read_coefficient);
                build_mul(
                    direct_reference(read_coefficient),
                    direct_reference(name.clone()),
                )
            })
            .chain([
                direct_reference(assign_const),
                build_mul(direct_reference(read_free), direct_reference(free_value)),
            ])
            .reduce(build_add);
        self.pil.push(Statement::PolynomialIdentity(
            0,
            build_sub(direct_reference(register), assign_constraint.unwrap()),
        ));
    }

    /// Translates the code lines to fixed column but also fills
    /// the query hints for the free inputs.
    fn translate_code_lines(&mut self) {
        // TODO this should loop with the number of lines in the program, as should all the other program constants!
        self.pil.push(Statement::PolynomialConstantDefinition(
            0,
            "p_line".to_string(),
            FunctionDefinition::Array(
                ArrayExpression::Value(
                    (0..self.code_lines.len())
                        .map(|i| build_number(i as u32))
                        .collect(),
                )
                .pad_with_last()
                .unwrap_or_else(|| ArrayExpression::RepeatedValue(vec![build_number(0)])),
            ),
        ));
        // TODO check that all of them are matched against execution trace witnesses.
        let mut program_constants = self
            .program_constant_names
            .iter()
            .map(|n| (n, vec![T::from(0); self.code_lines.len()]))
            .collect::<BTreeMap<_, _>>();
        let mut free_value_query_arms = self
            .assignment_registers()
            .map(|r| (r.clone(), vec![]))
            .collect::<BTreeMap<_, _>>();

        let label_positions = self.compute_label_positions();
        for (i, line) in self.code_lines.iter().enumerate() {
            for (assign_reg, writes) in &line.write_regs {
                for reg in writes {
                    program_constants
                        .get_mut(&format!("p_reg_write_{assign_reg}_{reg}"))
                        .unwrap_or_else(|| {
                            panic!("Register combination {reg} <={assign_reg}= not found.")
                        })[i] = 1.into();
                }
            }
            for (assign_reg, value) in &line.value {
                for (coeff, item) in value {
                    match item {
                        AffineExpressionComponent::Register(reg) => {
                            program_constants
                                .get_mut(&format!("p_read_{assign_reg}_{reg}"))
                                .unwrap_or_else(|| {
                                    panic!("Register combination <={assign_reg}= {reg} not found.")
                                })[i] += *coeff;
                        }
                        AffineExpressionComponent::Constant => {
                            program_constants
                                .get_mut(&format!("p_{assign_reg}_const"))
                                .unwrap()[i] += *coeff
                        }
                        AffineExpressionComponent::FreeInput(expr) => {
                            // The program just stores that we read a free input, the actual value
                            // is part of the execution trace that generates the witness.
                            program_constants
                                .get_mut(&format!("p_{assign_reg}_read_free"))
                                .unwrap()[i] += *coeff;
                            free_value_query_arms
                                .get_mut(assign_reg)
                                .unwrap()
                                .push((Some(build_number(i as u64)), expr.clone()));
                        }
                    }
                }
            }
            if let Some(instr) = &line.instruction {
                for (reg, writes) in &line.write_regs {
                    if !writes.is_empty() {
                        // If an instruction stores a value, assume that the assignment register is
                        // assigned in inline pil. We need to allow for "wiggle room" by setting
                        // the free input to 1.
                        // TODO This is horrible and needs to be fixed by a proper mechanism
                        // that enforces that the assignment register is actually properly constrained.
                        program_constants
                            .get_mut(&format!("p_{reg}_read_free"))
                            .unwrap()[i] = 1.into();
                    }
                }
                program_constants
                    .get_mut(&format!("p_instr_{instr}"))
                    .unwrap()[i] = 1.into();
                for (arg, param) in line
                    .instruction_literal_args
                    .iter()
                    .zip(self.instructions[instr].labels())
                {
                    program_constants
                        .get_mut(&format!("p_instr_{instr}_param_{}", param.clone()))
                        .unwrap()[i] = (label_positions[arg] as u64).into();
                }
            } else {
                assert!(line.instruction_literal_args.is_empty());
            }
        }
        let pc_name = self.pc_name.clone();
        let free_value_pil = self
            .assignment_registers()
            .map(|reg| {
                let free_value = format!("{reg}_free_value");
                witness_column(
                    0,
                    free_value,
                    Some(FunctionDefinition::Query(
                        vec!["i".to_string()],
                        Expression::MatchExpression(
                            Box::new(direct_reference(pc_name.as_ref().unwrap())),
                            free_value_query_arms[reg].clone(),
                        ),
                    )),
                )
            })
            .collect::<Vec<_>>();
        self.pil.extend(free_value_pil);
        for (name, values) in program_constants {
            self.pil.push(Statement::PolynomialConstantDefinition(
                0,
                name.clone(),
                FunctionDefinition::Array(
                    ArrayExpression::value(values.into_iter().map(build_number).collect())
                        .pad_with_last()
                        .unwrap_or_else(|| ArrayExpression::RepeatedValue(vec![build_number(0)])),
                ),
            ));
        }
    }

    fn compute_label_positions(&self) -> HashMap<String, usize> {
        self.code_lines
            .iter()
            .enumerate()
            .filter_map(|(i, line)| line.label.as_ref().map(|l| (l, i)))
            .fold(HashMap::new(), |mut r, (l, i)| {
                assert!(r.insert(l.clone(), i).is_none(), "Duplicate label: {l}");
                r
            })
    }

    /// Creates a pair of witness and fixed column and matches them in the lookup.
    fn create_witness_fixed_pair(&mut self, start: usize, name: &str) {
        let fixed_name = format!("p_{name}");
        self.pil.push(witness_column(start, name, None));
        self.line_lookup
            .push((name.to_string(), fixed_name.clone()));
        self.program_constant_names.push(fixed_name);
    }

    fn assignment_registers(&self) -> impl Iterator<Item = &String> {
        self.registers
            .iter()
            .filter_map(|(n, r)| if r.is_assignment { Some(n) } else { None })
    }

    fn regular_registers(&self) -> impl Iterator<Item = &String> {
        // TODO shouldn't we also filter out the PC?
        self.registers
            .iter()
            .filter_map(|(n, r)| if r.is_assignment { None } else { Some(n) })
    }
}

struct Register<T> {
    /// Constraints to update this register, first item being the
    /// condition, second item the value.
    /// TODO check that condition is bool
    conditioned_updates: Vec<(Expression<T>, Expression<T>)>,
    default_update: Option<Expression<T>>,
    is_assignment: bool,
}

impl<T: FieldElement> Register<T> {
    /// Returns the expression assigned to this register in the next row.
    pub fn update_expression(&self) -> Option<Expression<T>> {
        // TODO conditions need to be all boolean
        let updates = self
            .conditioned_updates
            .iter()
            .map(|(cond, value)| build_mul(cond.clone(), value.clone()))
            .reduce(build_add);

        // TODO for computing the default condition, we need to ensure
        // that the conditions all exclude each other
        match (self.conditioned_updates.len(), &self.default_update) {
            (0, update) => update.clone(),
            (_, None) => Some(updates.unwrap()),
            (_, Some(def)) => {
                let default_condition = build_sub(
                    build_number(1u64),
                    self.conditioned_updates
                        .iter()
                        .map(|(cond, _value)| cond.clone())
                        .reduce(build_add)
                        .unwrap(),
                );
                Some(build_add(
                    updates.unwrap(),
                    build_mul(default_condition, def.clone()),
                ))
            }
        }
    }
}

enum Input {
    Register(String),
    Label(String),
}

struct Instruction {
    inputs: Vec<Input>,
    outputs: Vec<String>,
}

impl Instruction {
    fn labels(&self) -> impl Iterator<Item = &String> {
        self.inputs.iter().filter_map(|input| match input {
            Input::Label(label) => Some(label),
            _ => None,
        })
    }
}

// TODO turn this into an enum, split into
// label, assignment, instruction.
#[derive(Default)]
struct CodeLine<T> {
    /// Which regular registers to assign to, from which assignment register
    /// Maps assignment register to a vector of regular registers.
    write_regs: BTreeMap<String, Vec<String>>,
    /// The value on the right-hand-side, per assignment register
    value: BTreeMap<String, Vec<(T, AffineExpressionComponent<T>)>>,
    label: Option<String>,
    instruction: Option<String>,
    // TODO we only support labels for now.
    instruction_literal_args: Vec<String>,
}

enum AffineExpressionComponent<T> {
    Register(String),
    Constant,
    FreeInput(Expression<T>),
}

fn witness_column<S: Into<String>, T>(
    start: usize,
    name: S,
    def: Option<FunctionDefinition<T>>,
) -> Statement<T> {
    Statement::PolynomialCommitDeclaration(
        start,
        vec![PolynomialName {
            name: name.into(),
            array_size: None,
        }],
        def,
    )
}

fn direct_reference<S: Into<String>, T>(name: S) -> Expression<T> {
    Expression::PolynomialReference(PolynomialReference {
        namespace: None,
        name: name.into(),
        index: None,
        next: false,
    })
}

fn next_reference<T>(name: &str) -> Expression<T> {
    Expression::PolynomialReference(PolynomialReference {
        namespace: None,
        name: name.to_owned(),
        index: None,
        next: true,
    })
}

fn build_mul<T>(left: Expression<T>, right: Expression<T>) -> Expression<T> {
    build_binary_expr(left, BinaryOperator::Mul, right)
}

fn build_sub<T>(left: Expression<T>, right: Expression<T>) -> Expression<T> {
    build_binary_expr(left, BinaryOperator::Sub, right)
}

fn build_add<T>(left: Expression<T>, right: Expression<T>) -> Expression<T> {
    build_binary_expr(left, BinaryOperator::Add, right)
}

fn build_binary_expr<T>(
    left: Expression<T>,
    op: BinaryOperator,
    right: Expression<T>,
) -> Expression<T> {
    Expression::BinaryOperation(Box::new(left), op, Box::new(right))
}

fn build_unary_expr<T>(op: UnaryOperator, exp: Expression<T>) -> Expression<T> {
    Expression::UnaryOperation(op, Box::new(exp))
}

fn build_number<T: FieldElement, V: Into<T>>(value: V) -> Expression<T> {
    Expression::Number(value.into())
}

fn extract_update<T: FieldElement>(expr: Expression<T>) -> (Option<String>, Expression<T>) {
    // TODO check that there are no other "next" references in the expression
    if let Expression::BinaryOperation(left, BinaryOperator::Sub, right) = expr {
        if let Expression::PolynomialReference(PolynomialReference {
            namespace,
            name,
            index,
            next: true,
        }) = *left
        {
            assert_eq!(namespace, None);
            assert_eq!(index, None);
            (Some(name), *right)
        } else {
            (None, build_binary_expr(*left, BinaryOperator::Sub, *right))
        }
    } else {
        (None, expr)
    }
}

fn substitute<T: FieldElement>(
    input: Expression<T>,
    substitution: &HashMap<String, String>,
) -> Expression<T> {
    match input {
        // TODO namespace
        Expression::PolynomialReference(r) => {
            Expression::PolynomialReference(PolynomialReference {
                name: substitute_string(&r.name, substitution),
                ..r.clone()
            })
        }
        Expression::BinaryOperation(left, op, right) => build_binary_expr(
            substitute(*left, substitution),
            op,
            substitute(*right, substitution),
        ),
        Expression::UnaryOperation(op, exp) => build_unary_expr(op, substitute(*exp, substitution)),
        Expression::FunctionCall(name, args) => Expression::FunctionCall(
            name,
            args.into_iter()
                .map(|e| substitute(e, substitution))
                .collect(),
        ),
        Expression::Tuple(items) => Expression::Tuple(
            items
                .into_iter()
                .map(|e| substitute(e, substitution))
                .collect(),
        ),
        Expression::Constant(_)
        | Expression::PublicReference(_)
        | Expression::Number(_)
        | Expression::String(_)
        | Expression::FreeInput(_) => input.clone(),
        Expression::MatchExpression(scrutinee, arms) => Expression::MatchExpression(
            Box::new(substitute(*scrutinee, substitution)),
            arms.into_iter()
                .map(|(n, e)| (n, substitute(e, substitution)))
                .collect(),
        ),
    }
}

fn substitute_selected_exprs<T: FieldElement>(
    input: SelectedExpressions<T>,
    substitution: &HashMap<String, String>,
) -> SelectedExpressions<T> {
    SelectedExpressions {
        selector: input.selector.map(|s| substitute(s, substitution)),
        expressions: substitute_vec(input.expressions, substitution),
    }
}

fn substitute_vec<T: FieldElement>(
    input: Vec<Expression<T>>,
    substitution: &HashMap<String, String>,
) -> Vec<Expression<T>> {
    input
        .into_iter()
        .map(|e| substitute(e, substitution))
        .collect()
}

fn substitute_string(input: &str, substitution: &HashMap<String, String>) -> String {
    substitution
        .get(input)
        .cloned()
        .unwrap_or_else(|| input.to_string())
}

#[cfg(test)]
mod test {
    use std::fs;

    use number::GoldilocksField;

    use super::compile;

    #[test]
    pub fn compile_simple_sum() {
        let expectation = r#"
namespace Assembly(1024);
pol constant first_step = [1] + [0]*;
pol commit pc;
pol commit X;
(first_step * A) = 0;
pol commit reg_write_X_A;
pol commit A;
(first_step * CNT) = 0;
pol commit reg_write_X_CNT;
pol commit CNT;
pol commit XInv;
pol commit XIsZero;
XIsZero = (1 - (X * XInv));
(XIsZero * X) = 0;
(XIsZero * (1 - XIsZero)) = 0;
pol commit instr_jmpz;
pol commit instr_jmpz_param_l;
pol commit instr_jmp;
pol commit instr_jmp_param_l;
pol commit instr_dec_CNT;
pol commit instr_assert_zero;
(instr_assert_zero * (XIsZero - 1)) = 0;
pol commit X_const;
pol commit X_read_free;
pol commit read_X_A;
pol commit read_X_CNT;
pol commit read_X_pc;
X = (((((read_X_A * A) + (read_X_CNT * CNT)) + (read_X_pc * pc)) + X_const) + (X_read_free * X_free_value));
A' = (((first_step' * 0) + (reg_write_X_A * X)) + ((1 - (first_step' + reg_write_X_A)) * A));
CNT' = ((((first_step' * 0) + (reg_write_X_CNT * X)) + (instr_dec_CNT * (CNT - 1))) + ((1 - ((first_step' + reg_write_X_CNT) + instr_dec_CNT)) * CNT));
pc' = ((1 - first_step') * (((instr_jmpz * ((XIsZero * instr_jmpz_param_l) + ((1 - XIsZero) * (pc + 1)))) + (instr_jmp * instr_jmp_param_l)) + ((1 - (instr_jmpz + instr_jmp)) * (pc + 1))));
pol constant p_line = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] + [10]*;
pol commit X_free_value(i) query match pc { 0 => ("input", 1), 3 => ("input", (CNT + 1)), 7 => ("input", 0), };
pol constant p_X_const = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_X_read_free = [1, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0] + [0]*;
pol constant p_instr_assert_zero = [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0] + [0]*;
pol constant p_instr_dec_CNT = [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_jmp = [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1] + [1]*;
pol constant p_instr_jmp_param_l = [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 9] + [9]*;
pol constant p_instr_jmpz = [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_jmpz_param_l = [0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_read_X_A = [0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0] + [0]*;
pol constant p_read_X_CNT = [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_read_X_pc = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_reg_write_X_A = [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0] + [0]*;
pol constant p_reg_write_X_CNT = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
{ pc, reg_write_X_A, reg_write_X_CNT, instr_jmpz, instr_jmpz_param_l, instr_jmp, instr_jmp_param_l, instr_dec_CNT, instr_assert_zero, X_const, X_read_free, read_X_A, read_X_CNT, read_X_pc } in { p_line, p_reg_write_X_A, p_reg_write_X_CNT, p_instr_jmpz, p_instr_jmpz_param_l, p_instr_jmp, p_instr_jmp_param_l, p_instr_dec_CNT, p_instr_assert_zero, p_X_const, p_X_read_free, p_read_X_A, p_read_X_CNT, p_read_X_pc };

"#;
        let file_name = "../test_data/asm/simple_sum.asm";
        let contents = fs::read_to_string(file_name).unwrap();
        let pil = compile::<GoldilocksField>(Some(file_name), &contents).unwrap();
        assert_eq!(format!("{pil}").trim(), expectation.trim());
    }
}
