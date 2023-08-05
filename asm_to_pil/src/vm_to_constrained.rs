//! Compilation from powdr assembly to PIL

use std::collections::{BTreeMap, BTreeSet, HashMap};

use ast::{
    asm_analysis::{
        AssignmentStatement, Batch, DebugDirective, FunctionStatement,
        InstructionDefinitionStatement, InstructionStatement, LabelStatement,
        LinkDefinitionStatement, Machine, PilBlock, RegisterDeclarationStatement, RegisterTy, Rom,
    },
    parsed::{
        asm::{InstructionBody, InstructionBodyElement, PlookupOperator},
        build::{
            build_add, build_binary_expr, build_mul, build_number, build_sub, direct_reference,
            next_reference,
        },
        postvisit_expression_in_statement_mut, ArrayExpression, BinaryOperator, Expression,
        FunctionDefinition, PilStatement, PolynomialName, SelectedExpressions, UnaryOperator,
    },
};

use number::FieldElement;

use crate::common::{instruction_flag, return_instruction, RETURN_NAME};

pub fn convert_machine<T: FieldElement>(machine: Machine<T>, rom: Option<Rom<T>>) -> Machine<T> {
    let output_count = machine
        .operations()
        .map(|f| {
            f.params
                .outputs
                .as_ref()
                .map(|list| list.params.len())
                .unwrap_or(0)
        })
        .max()
        .unwrap_or_default();
    ASMPILConverter::with_output_count(output_count).convert_machine(machine, rom)
}

pub enum Input {
    Register(String),
    Literal(String, LiteralKind),
}

pub enum LiteralKind {
    Label,
    SignedConstant,
    UnsignedConstant,
}

#[derive(Default)]
struct ASMPILConverter<T> {
    pil: Vec<PilStatement<T>>,
    pc_name: Option<String>,
    registers: BTreeMap<String, Register<T>>,
    instructions: BTreeMap<String, Instruction>,
    code_lines: Vec<CodeLine<T>>,
    /// Pairs of columns that are used in the connecting plookup
    line_lookup: Vec<(String, String)>,
    /// Names of fixed columns that contain the rom.
    rom_constant_names: Vec<String>,
    /// the maximum number of inputs in all functions
    output_count: usize,
}

impl<T: FieldElement> ASMPILConverter<T> {
    fn with_output_count(output_count: usize) -> Self {
        Self {
            output_count,
            ..Default::default()
        }
    }

    fn convert_machine(mut self, mut input: Machine<T>, rom: Option<Rom<T>>) -> Machine<T> {
        if !input.has_pc() {
            assert!(rom.is_none());
            return input;
        }

        // turn registers into constraints
        for reg in input.registers.drain(..) {
            self.handle_register_declaration(reg);
        }

        // turn internal instructions into constraints and external ones into links
        input.links.extend(
            input
                .instructions
                .drain(..)
                .filter_map(|instr| self.handle_instruction_def(instr)),
        );

        // introduce `return` instruction
        assert!(
            self.handle_instruction_def(InstructionDefinitionStatement {
                start: 0,
                name: RETURN_NAME.into(),
                instruction: self.return_instruction()
            })
            .is_none(),
            "return instruction cannot link to an external function"
        );

        let assignment_registers = self
            .assignment_register_names()
            .cloned()
            .collect::<Vec<_>>();
        for reg in assignment_registers {
            self.create_constraints_for_assignment_reg(reg);
        }

        // introduce `first_step` which is used for register updates
        self.pil.push(PilStatement::PolynomialConstantDefinition(
            0,
            "first_step".to_string(),
            FunctionDefinition::Array(
                ArrayExpression::value(vec![build_number(1u64)]).pad_with_zeroes(),
            ),
        ));

        self.pil.extend(
            self.registers
                .iter()
                .filter_map(|(name, reg)| {
                    reg.update_expression().map(|rhs| {
                        let lhs = next_reference(name);
                        use RegisterTy::*;
                        let (lhs, rhs) = match reg.ty {
                            // Force pc to zero on first row.
                            Pc => (
                                lhs,
                                build_mul(
                                    build_sub(build_number(1u64), next_reference("first_step")),
                                    rhs,
                                ),
                            ),
                            // Unconstrain read-only registers when calling `_reset`
                            ReadOnly => {
                                let not_reset =
                                    build_sub(build_number(1u64), direct_reference("instr__reset"));
                                (build_mul(not_reset.clone(), lhs), build_mul(not_reset, rhs))
                            }
                            //
                            _ => (lhs, rhs),
                        };

                        PilStatement::PolynomialIdentity(0, build_sub(lhs, rhs))
                    })
                })
                .collect::<Vec<_>>(),
        );

        for batch in rom.unwrap().statements.into_iter_batches() {
            self.handle_batch(batch);
        }

        input.latch = Some(instruction_flag(RETURN_NAME));

        self.translate_code_lines();

        self.pil.push(PilStatement::PlookupIdentity(
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

        if !self.pil.is_empty() {
            input.constraints.push(PilBlock {
                start: 0,
                statements: self.pil,
            });
        }

        input
    }

    fn handle_batch(&mut self, batch: Batch<T>) {
        let code_line = batch
            .statements
            .into_iter()
            .map(|s| self.handle_statement(s))
            .reduce(|mut acc, e| {
                // we write to the union of the target registers.
                assert!(acc.write_regs.is_empty());
                acc.write_regs.extend(e.write_regs);
                // we write the union of the written values.
                assert!(acc.value.is_empty());
                acc.value.extend(e.value);
                // we use the union of the used instructions.
                assert!(acc.instructions.is_empty());
                acc.instructions.extend(e.instructions);
                // we use the union of the labels
                acc.labels.extend(e.labels);
                // we use the union of debug directives
                acc.debug_directives.extend(e.debug_directives);
                acc
            })
            .expect("unexpected empty batch");

        self.code_lines.push(code_line);
    }

    fn handle_statement(&mut self, statement: FunctionStatement<T>) -> CodeLine<T> {
        match statement {
            FunctionStatement::Assignment(AssignmentStatement {
                start,
                lhs,
                using_reg,
                rhs,
            }) => match *rhs {
                Expression::FunctionCall(c) => {
                    self.handle_functional_instruction(lhs, using_reg.unwrap(), c.id, c.arguments)
                }
                _ => self.handle_assignment(start, lhs, using_reg, *rhs),
            },
            FunctionStatement::Instruction(InstructionStatement {
                instruction,
                inputs,
                ..
            }) => self.handle_instruction(instruction, inputs),
            FunctionStatement::Label(LabelStatement { name, .. }) => CodeLine {
                labels: [name].into(),
                ..Default::default()
            },
            FunctionStatement::DebugDirective(d) => CodeLine {
                debug_directives: vec![d],
                ..Default::default()
            },
            FunctionStatement::Return(r) => self.handle_instruction(RETURN_NAME.into(), r.values),
        }
    }

    fn handle_register_declaration(
        &mut self,
        RegisterDeclarationStatement { start, ty, name }: RegisterDeclarationStatement,
    ) {
        let mut conditioned_updates = vec![];
        let mut default_update = None;
        match ty {
            RegisterTy::Pc => {
                assert_eq!(self.pc_name, None);
                self.pc_name = Some(name.to_string());
                self.line_lookup
                    .push((name.to_string(), "p_line".to_string()));
                default_update = Some(build_add(direct_reference(&name), build_number(1u64)));
            }
            RegisterTy::Assignment => {
                // no default update as this is transient
            }
            RegisterTy::ReadOnly => {
                // default update to be kept constant
                default_update = Some(direct_reference(&name))
            }
            RegisterTy::Write => {
                let assignment_regs = self
                    .assignment_register_names()
                    .cloned()
                    .collect::<Vec<_>>();
                // TODO do this at the same place where we set up the read flags.
                for reg in assignment_regs {
                    let write_flag = format!("reg_write_{reg}_{name}");
                    self.create_witness_fixed_pair(start, &write_flag);
                    conditioned_updates
                        .push((direct_reference(&write_flag), direct_reference(&reg)));
                }
                default_update = Some(direct_reference(&name));
            }
        };
        self.registers.insert(
            name.to_string(),
            Register {
                conditioned_updates,
                default_update,
                ty,
            },
        );
        self.pil.push(witness_column(start, name, None));
    }

    fn handle_instruction_def(
        &mut self,
        s: InstructionDefinitionStatement<T>,
    ) -> Option<LinkDefinitionStatement<T>> {
        let instruction_name = s.name.clone();
        let instruction_flag = format!("instr_{instruction_name}");
        self.create_witness_fixed_pair(s.start, &instruction_flag);

        let inputs: Vec<_> = s
            .instruction
            .params
            .clone()
            .inputs
            .params
            .into_iter()
            .map(|param| match param.ty {
                Some(ty) if ty == "label" => Input::Literal(param.name, LiteralKind::Label),
                Some(ty) if ty == "signed" => {
                    Input::Literal(param.name, LiteralKind::SignedConstant)
                }
                Some(ty) if ty == "unsigned" => {
                    Input::Literal(param.name, LiteralKind::UnsignedConstant)
                }
                None => Input::Register(param.name),
                Some(_) => unreachable!(),
            })
            .collect();

        let outputs = s
            .instruction
            .params
            .clone()
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

        let instruction = Instruction { inputs, outputs };

        // First transform into PIL so that we can apply macro expansion.

        let res = match s.instruction.body {
            InstructionBody::Local(body) => {
                let mut statements = body
                    .into_iter()
                    .map(|el| match el {
                        InstructionBodyElement::PolynomialIdentity(left, right) => {
                            PilStatement::PolynomialIdentity(s.start, build_sub(left, right))
                        }
                        InstructionBodyElement::PlookupIdentity(left, op, right) => {
                            assert!(
                        left.selector.is_none(),
                        "LHS selector not supported, could and-combine with instruction flag later."
                    );
                            match op {
                                PlookupOperator::In => {
                                    PilStatement::PlookupIdentity(s.start, left, right)
                                }
                                PlookupOperator::Is => {
                                    PilStatement::PermutationIdentity(s.start, left, right)
                                }
                            }
                        }
                        InstructionBodyElement::FunctionCall(c) => {
                            PilStatement::FunctionCall(s.start, c.id, c.arguments)
                        }
                    })
                    .collect::<Vec<_>>();

                // Substitute parameter references by the column names
                let substitutions = instruction
                    .literal_arg_names()
                    .map(|arg_name| {
                        let param_col_name = format!("instr_{instruction_name}_param_{arg_name}");
                        self.create_witness_fixed_pair(s.start, &param_col_name);
                        (arg_name.clone(), param_col_name)
                    })
                    .collect::<HashMap<_, _>>();
                statements.iter_mut().for_each(|s| {
                    postvisit_expression_in_statement_mut(s, &mut |e| {
                        if let Expression::PolynomialReference(r) = e {
                            if let Some(sub) = substitutions.get(r.name()) {
                                *r.name_mut() = sub.to_string();
                            }
                        }
                        std::ops::ControlFlow::Continue::<()>(())
                    });
                });

                // Expand macros and analyze resulting statements.
                for mut statement in statements {
                    if let PilStatement::PolynomialIdentity(_start, expr) = statement {
                        match extract_update(expr) {
                            (Some(var), expr) => {
                                let reference = direct_reference(&instruction_flag);

                                self.registers
                                    .get_mut(&var)
                                    .unwrap()
                                    .conditioned_updates
                                    .push((reference, expr));
                            }
                            (None, expr) => self.pil.push(PilStatement::PolynomialIdentity(
                                0,
                                build_mul(direct_reference(&instruction_flag), expr.clone()),
                            )),
                        }
                    } else {
                        match &mut statement {
                            PilStatement::PermutationIdentity(_, left, _)
                            | PilStatement::PlookupIdentity(_, left, _) => {
                                assert!(
                            left.selector.is_none(),
                            "LHS selector not supported, could and-combine with instruction flag later."
                        );
                                left.selector = Some(direct_reference(&instruction_flag));
                                self.pil.push(statement)
                            }
                            _ => {
                                panic!("Invalid statement for instruction body: {statement}");
                            }
                        }
                    }
                }
                None
            }
            InstructionBody::CallableRef(to) => Some(LinkDefinitionStatement {
                start: s.start,
                flag: direct_reference(instruction_flag),
                params: s.instruction.params,
                to,
            }),
        };
        self.instructions.insert(instruction_name, instruction);
        res
    }

    fn handle_assignment(
        &mut self,
        _start: usize,
        write_regs: Vec<String>,
        assign_reg: Option<String>,
        value: Expression<T>,
    ) -> CodeLine<T> {
        assert!(write_regs.len() <= 1);
        assert!(
            assign_reg.is_some(),
            "Implicit assign register not yet supported."
        );
        let assign_reg = assign_reg.unwrap();
        let value = self.process_assignment_value(value);
        CodeLine {
            write_regs: [(assign_reg.clone(), write_regs)].into_iter().collect(),
            value: [(assign_reg, value)].into(),
            ..Default::default()
        }
    }

    fn handle_functional_instruction(
        &mut self,
        write_regs: Vec<String>,
        assign_reg: String,
        instr_name: String,
        args: Vec<Expression<T>>,
    ) -> CodeLine<T> {
        assert!(write_regs.len() == 1);
        let instr = &self
            .instructions
            .get(&instr_name)
            .unwrap_or_else(|| panic!("Instruction not found: {instr_name}"));
        assert_eq!(instr.outputs.len(), 1);
        let output = instr.outputs[0].clone();
        assert!(
            output == assign_reg,
            "The instruction {instr_name} uses the assignment register {output}, but the caller uses {assign_reg} to further process the value.",
        );

        let mut args = args;
        args.push(direct_reference(write_regs.first().unwrap().clone()));
        self.handle_instruction(instr_name, args)
    }

    fn handle_instruction(&mut self, instr_name: String, args: Vec<Expression<T>>) -> CodeLine<T> {
        let instr = &self
            .instructions
            .get(&instr_name)
            .unwrap_or_else(|| panic!("Instruction not found: {instr_name}"));
        assert_eq!(
            instr.inputs.len() + instr.outputs.len(),
            args.len(),
            "Called instruction {} with the wrong number of arguments",
            instr_name
        );

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
                        Input::Literal(_, LiteralKind::Label) => {
                            if let Expression::PolynomialReference(r) = a {
                                instruction_literal_arg
                                    .push(InstructionLiteralArg::LabelRef(r.name().into()));
                            } else {
                                panic!();
                            }
                        }
                        Input::Literal(_, LiteralKind::UnsignedConstant) => {
                            // TODO evaluate expression
                            if let Expression::Number(n) = a {
                                assert!(n.is_in_lower_half(), "Number passed to unsigned parameter is negative or too large: {n}");
                                instruction_literal_arg.push(InstructionLiteralArg::Number(n));
                            } else {
                                panic!("expected unsigned number, received {}", a);
                            }
                        }
                        Input::Literal(_, LiteralKind::SignedConstant) => {
                            // TODO evaluate expression
                            if let Expression::Number(n) = a {
                                instruction_literal_arg.push(InstructionLiteralArg::Number(n));
                            } else if let Expression::UnaryOperation(UnaryOperator::Minus, expr) = a
                            {
                                if let Expression::Number(n) = *expr {
                                    instruction_literal_arg.push(InstructionLiteralArg::Number(-n));
                                } else {
                                    panic!();
                                }
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
                    assert!(!r.shift());
                    assert!(r.index().is_none());
                    (reg.clone(), vec![r.name().into()])
                } else {
                    panic!("Expected direct register to assign to in instruction call.");
                }
            })
            .collect();

        assert_eq!(write_regs.len(), instr.outputs.len());

        CodeLine {
            write_regs,
            instructions: vec![(instr_name.to_string(), instruction_literal_args)],
            value,
            ..Default::default()
        }
    }

    fn process_assignment_value(
        &self,
        value: Expression<T>,
    ) -> Vec<(T, AffineExpressionComponent<T>)> {
        match value {
            Expression::Constant(_) => panic!(),
            Expression::PublicReference(_) => panic!(),
            Expression::FunctionCall(_) => panic!(),
            Expression::PolynomialReference(reference) => {
                assert!(reference.namespace().is_none());
                assert!(reference.index().is_none());
                assert!(!reference.shift());
                // TODO check it actually is a register
                vec![(
                    1.into(),
                    AffineExpressionComponent::Register(reference.name().into()),
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
        // we can read from write registers, pc and read-only registers
        let read_registers = self
            .write_register_names()
            .chain(self.pc_register_names())
            .chain(self.read_only_register_names())
            .cloned()
            .collect::<Vec<_>>();
        let assign_constraint = read_registers
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
        self.pil.push(PilStatement::PolynomialIdentity(
            0,
            build_sub(direct_reference(register), assign_constraint.unwrap()),
        ));
    }

    /// Translates the code lines to fixed column but also fills
    /// the query hints for the free inputs.
    fn translate_code_lines(&mut self) {
        self.pil.push(PilStatement::PolynomialConstantDefinition(
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
        let mut rom_constants = self
            .rom_constant_names
            .iter()
            .map(|n| (n, vec![T::from(0); self.code_lines.len()]))
            .collect::<BTreeMap<_, _>>();
        let mut free_value_query_arms = self
            .assignment_register_names()
            .map(|r| (r.clone(), vec![]))
            .collect::<BTreeMap<_, _>>();

        let label_positions = self.compute_label_positions();
        for (i, line) in self.code_lines.iter().enumerate() {
            for (assign_reg, writes) in &line.write_regs {
                for reg in writes {
                    rom_constants
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
                            rom_constants
                                .get_mut(&format!("p_read_{assign_reg}_{reg}"))
                                .unwrap_or_else(|| {
                                    panic!("Register combination <={assign_reg}= {reg} not found.")
                                })[i] += *coeff;
                        }
                        AffineExpressionComponent::Constant => {
                            rom_constants
                                .get_mut(&format!("p_{assign_reg}_const"))
                                .unwrap()[i] += *coeff
                        }
                        AffineExpressionComponent::FreeInput(expr) => {
                            // The rom just stores that we read a free input, the actual value
                            // is part of the execution trace that generates the witness.
                            rom_constants
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
            for (instr, literal_args) in &line.instructions {
                for (reg, writes) in &line.write_regs {
                    if !writes.is_empty() {
                        // If an instruction stores a value, assume that the assignment register is
                        // assigned in inline pil. We need to allow for "wiggle room" by setting
                        // the free input to 1.
                        // TODO This is horrible and needs to be fixed by a proper mechanism
                        // that enforces that the assignment register is actually properly constrained.
                        rom_constants
                            .get_mut(&format!("p_{reg}_read_free"))
                            .unwrap()[i] = 1.into();
                    }
                }
                rom_constants.get_mut(&format!("p_instr_{instr}")).unwrap()[i] = 1.into();
                for (arg, param) in literal_args
                    .iter()
                    .zip(self.instructions[instr].literal_arg_names())
                {
                    rom_constants
                        .get_mut(&format!("p_instr_{instr}_param_{}", param.clone()))
                        .unwrap()[i] = match arg {
                        InstructionLiteralArg::LabelRef(name) => (*label_positions
                            .get(name)
                            .unwrap_or_else(|| panic!("{name} not found in labels"))
                            as u64)
                            .into(),
                        InstructionLiteralArg::Number(n) => *n,
                    };
                }
            }
        }
        let pc_name = self.pc_name.clone();
        let free_value_pil = self
            .assignment_register_names()
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
        for (name, values) in rom_constants {
            self.pil.push(PilStatement::PolynomialConstantDefinition(
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
            .flat_map(|(i, line)| line.labels.iter().map(|l| (l, i)).collect::<Vec<_>>())
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
        self.rom_constant_names.push(fixed_name);
    }

    fn assignment_register_names(&self) -> impl Iterator<Item = &String> {
        self.registers
            .iter()
            .filter_map(|(n, r)| r.ty.is_assignment().then_some(n))
    }

    fn write_register_names(&self) -> impl Iterator<Item = &String> {
        self.registers
            .iter()
            .filter_map(|(n, r)| r.ty.is_write().then_some(n))
    }

    fn pc_register_names(&self) -> impl Iterator<Item = &String> {
        self.registers
            .iter()
            .filter_map(|(n, r)| r.ty.is_pc().then_some(n))
    }

    fn read_only_register_names(&self) -> impl Iterator<Item = &String> {
        self.registers
            .iter()
            .filter_map(|(n, r)| r.ty.is_read_only().then_some(n))
    }

    fn return_instruction(&self) -> ast::asm_analysis::Instruction<T> {
        return_instruction(self.output_count, self.pc_name.as_ref().unwrap())
    }
}

struct Register<T> {
    /// Constraints to update this register, first item being the
    /// condition, second item the value.
    /// TODO check that condition is bool
    conditioned_updates: Vec<(Expression<T>, Expression<T>)>,
    default_update: Option<Expression<T>>,
    ty: RegisterTy,
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

struct Instruction {
    inputs: Vec<Input>,
    outputs: Vec<String>,
}

impl Instruction {
    fn literal_arg_names(&self) -> impl Iterator<Item = &String> {
        self.inputs.iter().filter_map(|input| match input {
            Input::Literal(name, _) => Some(name),
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
    labels: BTreeSet<String>,
    instructions: Vec<(String, Vec<InstructionLiteralArg<T>>)>,
    debug_directives: Vec<DebugDirective>,
}

enum AffineExpressionComponent<T> {
    Register(String),
    Constant,
    FreeInput(Expression<T>),
}

enum InstructionLiteralArg<T> {
    LabelRef(String),
    Number(T),
}

fn witness_column<S: Into<String>, T>(
    start: usize,
    name: S,
    def: Option<FunctionDefinition<T>>,
) -> PilStatement<T> {
    PilStatement::PolynomialCommitDeclaration(
        start,
        vec![PolynomialName {
            name: name.into(),
            array_size: None,
        }],
        def,
    )
}

fn extract_update<T: FieldElement>(expr: Expression<T>) -> (Option<String>, Expression<T>) {
    // TODO check that there are no other "next" references in the expression
    if let Expression::BinaryOperation(left, BinaryOperator::Sub, right) = expr {
        match *left {
            Expression::PolynomialReference(r) if r.shift() => {
                assert!(r.namespace().is_none());
                assert!(r.index().is_none());
                (Some(r.name().into()), *right)
            }
            _ => (None, build_binary_expr(*left, BinaryOperator::Sub, *right)),
        }
    } else {
        (None, expr)
    }
}
