use std::fmt::{Display, Formatter, Result};

use crate::parsed::{BinaryOperator, UnaryOperator};

use super::{asm::*, *};

// TODO indentation

impl<T: Display> Display for PILFile<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for s in &self.0 {
            writeln!(f, "{s}")?;
        }
        Ok(())
    }
}

impl<T: Display> Display for ASMFile<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for m in &self.machines {
            writeln!(f, "{m}")?;
        }
        Ok(())
    }
}

impl<T: Display> Display for Machine<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "machine {} {{", self.name)?;
        for s in &self.statements {
            writeln!(f, "{s}")?;
        }
        writeln!(f, "}}")
    }
}

impl<T: Display> Display for InstructionBody<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            InstructionBody::Local(elements) => write!(
                f,
                "{}",
                elements
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(",\n")
            ),
            InstructionBody::External(machine, instr) => write!(f, "{}.{}", machine, instr),
        }
    }
}

impl<T: Display> Display for MachineStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            MachineStatement::Degree(_, degree) => write!(f, "degree {};", degree),
            MachineStatement::Submachine(_, ty, name) => write!(f, "{ty} {name}"),
            MachineStatement::RegisterDeclaration(_, name, flag) => write!(
                f,
                "reg {}{};",
                name,
                flag.as_ref()
                    .map(|flag| format!("[{flag}]"))
                    .unwrap_or_default()
            ),
            MachineStatement::InstructionDeclaration(_, name, params, body) => {
                write!(f, "instr {}{} {{{}}}", name, params, body,)
            }
            MachineStatement::InlinePil(_, statements) => {
                write!(
                    f,
                    "pil{{\n{}\n}}",
                    statements
                        .iter()
                        .map(|s| format!("{}", s))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            MachineStatement::Program(_, statements) => write!(
                f,
                "program {{\n{}\n}}",
                statements
                    .iter()
                    .map(|s| format!("{}", s))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
        }
    }
}

impl<T: Display> Display for ProgramStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ProgramStatement::Assignment(_, write_regs, assignment_reg, expression) => write!(
                f,
                "{} <={}= {};",
                write_regs.join(", "),
                assignment_reg
                    .as_ref()
                    .map(ToString::to_string)
                    .unwrap_or_default(),
                expression
            ),
            ProgramStatement::Instruction(_, name, inputs) => write!(
                f,
                "{}{};",
                name,
                if inputs.is_empty() {
                    "".to_string()
                } else {
                    format!(
                        " {}",
                        inputs
                            .iter()
                            .map(|i| i.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            ),
            ProgramStatement::Label(_, name) => write!(f, "{name}::"),
            ProgramStatement::DebugDirective(_start, dir) => write!(f, "{dir}"),
        }
    }
}

impl Display for DebugDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            DebugDirective::File(nr, path, file) => {
                write!(f, "debug file {nr} {} {};", quote(path), quote(file))
            }
            DebugDirective::Loc(file, line, col) => {
                write!(f, "debug loc {file} {line} {col};")
            }
        }
    }
}

impl Display for RegisterFlag {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            RegisterFlag::IsPC => write!(f, "@pc"),
            RegisterFlag::IsAssignment => write!(f, "<="),
        }
    }
}

impl Display for InstructionParams {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}{}",
            if self.inputs.params.len() + self.outputs.as_ref().map(|o| o.params.len()).unwrap_or(0)
                == 0
            {
                ""
            } else {
                " "
            },
            self.inputs,
            self.outputs
                .as_ref()
                .map(|outputs| format!(" -> {}", outputs))
                .unwrap_or_default()
        )
    }
}

impl Display for InstructionParamList {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            self.params
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl<T: Display> Display for FunctionCall<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}({})", self.id, format_expressions(&self.arguments))
    }
}

impl<T: Display> Display for InstructionBodyElement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            InstructionBodyElement::Expression(e) => write!(f, "{e}"),
            InstructionBodyElement::PlookupIdentity(left, operator, right) => {
                write!(f, "{left} {operator} {right}")
            }
            InstructionBodyElement::FunctionCall(c) => {
                write!(f, "{c}")
            }
        }
    }
}

impl Display for PlookupOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            PlookupOperator::In => write!(f, "in"),
            PlookupOperator::Is => write!(f, "is"),
        }
    }
}

impl Display for InstructionParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}",
            self.name,
            self.ty
                .as_ref()
                .map(|ty| format!(": {}", ty))
                .unwrap_or_default()
        )
    }
}

pub fn quote(input: &str) -> String {
    format!("\"{}\"", input.replace('\\', "\\\\").replace('"', "\\\""))
}

impl<T: Display> Display for PilStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            PilStatement::Include(_, path) => write!(f, "include {};", quote(path)),
            PilStatement::Namespace(_, name, poly_length) => {
                write!(f, "namespace {name}({poly_length});")
            }
            PilStatement::PolynomialDefinition(_, name, value) => {
                write!(f, "pol {name} = {value};")
            }
            PilStatement::PublicDeclaration(_, name, poly, index) => {
                write!(f, "public {name} = {poly}({index});")
            }
            PilStatement::PolynomialConstantDeclaration(_, names) => {
                write!(f, "pol constant {};", format_names(names))
            }
            PilStatement::PolynomialConstantDefinition(_, name, definition) => {
                write!(f, "pol constant {name}{definition};")
            }
            PilStatement::PolynomialCommitDeclaration(_, names, value) => {
                write!(
                    f,
                    "pol commit {}{};",
                    format_names(names),
                    value.as_ref().map(|v| format!("{v}")).unwrap_or_default()
                )
            }
            PilStatement::PolynomialIdentity(_, expression) => {
                if let Expression::BinaryOperation(left, BinaryOperator::Sub, right) = expression {
                    write!(f, "{left} = {right};")
                } else {
                    write!(f, "{expression} = 0;")
                }
            }
            PilStatement::PlookupIdentity(_, left, right) => write!(f, "{left} in {right};"),
            PilStatement::PermutationIdentity(_, left, right) => write!(f, "{left} is {right};"),
            PilStatement::ConnectIdentity(_, left, right) => write!(
                f,
                "{{ {} }} connect {{ {} }};",
                format_expressions(left),
                format_expressions(right)
            ),
            PilStatement::ConstantDefinition(_, name, value) => {
                write!(f, "constant {name} = {value};")
            }
            PilStatement::MacroDefinition(_, name, params, statements, expression) => {
                let statements = statements
                    .iter()
                    .map(|s| format!("{s}"))
                    .chain(expression.iter().map(|e| format!("{e}")))
                    .collect::<Vec<_>>();
                let body = if statements.len() <= 1 {
                    format!(" {} ", statements.join(""))
                } else {
                    format!("\n    {}\n", statements.join("\n    "))
                };
                write!(f, "macro {name}({}) {{{body}}};", params.join(", "))
            }
            PilStatement::FunctionCall(_, name, args) => {
                write!(f, "{name}({});", format_expressions(args))
            }
        }
    }
}

fn format_names<T: Display>(names: &[PolynomialName<T>]) -> String {
    names
        .iter()
        .map(|n| format!("{n}"))
        .collect::<Vec<_>>()
        .join(", ")
}

impl<T: Display> Display for ArrayExpression<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ArrayExpression::Value(expressions) => {
                write!(f, "[{}]", format_expressions(expressions))
            }
            ArrayExpression::RepeatedValue(expressions) => {
                write!(f, "[{}]*", format_expressions(expressions))
            }
            ArrayExpression::Concat(left, right) => write!(f, "{left} + {right}"),
        }
    }
}

impl<T: Display> Display for FunctionDefinition<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FunctionDefinition::Mapping(params, body) => {
                write!(f, "({}) {{ {body} }}", params.join(", "))
            }
            FunctionDefinition::Array(array_expression) => {
                write!(f, " = {array_expression}")
            }
            FunctionDefinition::Query(params, value) => {
                write!(f, "({}) query {value}", params.join(", "),)
            }
        }
    }
}

impl<T: Display> Display for SelectedExpressions<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{{ {} }}",
            self.selector
                .as_ref()
                .map(|s| format!("{s} "))
                .unwrap_or_default(),
            format_expressions(&self.expressions)
        )
    }
}

fn format_expressions<T: Display>(expressions: &[Expression<T>]) -> String {
    expressions
        .iter()
        .map(|e| format!("{e}"))
        .collect::<Vec<_>>()
        .join(", ")
}

impl<T: Display> Display for Expression<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expression::Constant(name) => write!(f, "{name}"),
            Expression::PolynomialReference(reference) => write!(f, "{reference}"),
            Expression::PublicReference(name) => write!(f, "{name}"),
            Expression::Number(value) => write!(f, "{value}"),
            Expression::String(value) => write!(f, "\"{value}\""), // TODO quote?
            Expression::Tuple(items) => write!(f, "({})", format_expressions(items)),
            Expression::BinaryOperation(left, op, right) => write!(f, "({left} {op} {right})"),
            Expression::UnaryOperation(op, exp) => write!(f, "{op}{exp}"),
            Expression::FunctionCall(c) => write!(f, "{c}"),
            Expression::FreeInput(input) => write!(f, "${{ {input} }}"),
            Expression::MatchExpression(scrutinee, arms) => write!(
                f,
                "match {scrutinee} {{ {} }}",
                arms.iter()
                    .map(|(n, e)| format!(
                        "{} => {e},",
                        n.as_ref()
                            .map(|n| n.to_string())
                            .unwrap_or_else(|| "_".to_string())
                    ))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
        }
    }
}

impl<T: Display> Display for PolynomialName<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}",
            self.name,
            self.array_size
                .as_ref()
                .map(|s| format!("[{s}]"))
                .unwrap_or_default()
        )
    }
}

impl Display for FunctionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FunctionId::Literal(s) => write!(f, "{s}"),
            FunctionId::Member(instance, id) => write!(f, "{instance}.{id}"),
        }
    }
}

impl<T: Display> Display for PolynomialReference<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}{}{}",
            self.namespace
                .as_ref()
                .map(|n| format!("{n}."))
                .unwrap_or_default(),
            self.name,
            self.index
                .as_ref()
                .map(|s| format!("[{s}]"))
                .unwrap_or_default(),
            if self.next { "'" } else { "" }
        )
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOperator::Add => "+",
                BinaryOperator::Sub => "-",
                BinaryOperator::Mul => "*",
                BinaryOperator::Div => "/",
                BinaryOperator::Mod => "%",
                BinaryOperator::Pow => "**",
                BinaryOperator::BinaryAnd => "&",
                BinaryOperator::BinaryXor => "^",
                BinaryOperator::BinaryOr => "|",
                BinaryOperator::ShiftLeft => "<<",
                BinaryOperator::ShiftRight => ">>",
            }
        )
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOperator::Minus => "-",
                UnaryOperator::Plus => "+",
            }
        )
    }
}
