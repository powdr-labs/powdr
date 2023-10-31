use std::fmt::{Display, Formatter, Result};

use itertools::Itertools;

use crate::{
    parsed::{BinaryOperator, UnaryOperator},
    write_items, write_items_indented,
};

use super::{asm::*, *};

// TODO indentation

impl<T: Display> Display for PILFile<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write_items(f, &self.0)
    }
}

impl<T: Display> Display for ASMProgram<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.main)
    }
}

impl<T: Display> Display for ASMModule<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write_items(f, &self.statements)
    }
}

impl<T: Display> Display for ModuleStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ModuleStatement::SymbolDefinition(SymbolDefinition { name, value }) => match value {
                SymbolValue::Machine(m) => {
                    write!(f, "machine {name} {m}")
                }
                SymbolValue::Import(i) => {
                    write!(f, "{i} as {name}")
                }
                SymbolValue::Module(m) => {
                    write!(f, "mod {name} {m}")
                }
            },
        }
    }
}

impl<T: Display> Display for Module<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Module::External(name) => write!(f, "{name};"),
            Module::Local(module) => {
                writeln!(f, "{{")?;
                write_items_indented(f, &module.statements)?;
                write!(f, "}}")
            }
        }
    }
}

impl Display for Import {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "use {}", self.path)
    }
}

impl Display for SymbolPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.parts.iter().format("::"))
    }
}

impl Display for AbsoluteSymbolPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.parts.iter().format("::"))
    }
}

impl Display for Part {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Part::Super => write!(f, "super"),
            Part::Named(name) => write!(f, "{name}"),
        }
    }
}

impl<T: Display> Display for Machine<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "{{")?;
        write_items_indented(f, &self.statements)?;
        write!(f, "}}")
    }
}

impl<T: Display> Display for InstructionBody<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            InstructionBody::Local(elements) => write!(f, "{{ {} }}", elements.iter().format(", ")),
            InstructionBody::CallableRef(r) => write!(f, " = {r};"),
        }
    }
}

impl<T: Display> Display for Instruction<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}",
            self.params.prepend_space_if_non_empty(),
            self.body
        )
    }
}

impl<T: Display> Display for LinkDeclaration<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "link {}{} = {};",
            self.flag,
            self.params.prepend_space_if_non_empty(),
            self.to
        )
    }
}

impl Display for CallableRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}.{}", self.instance, self.callable)
    }
}

impl<T: Display> Display for MachineStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            MachineStatement::Degree(_, degree) => write!(f, "degree {};", degree),
            MachineStatement::Pil(_, statement) => write!(f, "{statement};"),
            MachineStatement::Submachine(_, ty, name) => write!(f, "{ty} {name};"),
            MachineStatement::RegisterDeclaration(_, name, flag) => write!(
                f,
                "reg {}{};",
                name,
                flag.as_ref()
                    .map(|flag| format!("[{flag}]"))
                    .unwrap_or_default()
            ),
            MachineStatement::InstructionDeclaration(_, name, instruction) => {
                write!(f, "instr {}{}", name, instruction)
            }
            MachineStatement::LinkDeclaration(link) => {
                write!(f, "{link}")
            }
            MachineStatement::FunctionDeclaration(_, name, params, statements) => {
                write!(
                    f,
                    "function {name}{} {{\n{}\n}}",
                    params.prepend_space_if_non_empty(),
                    statements.iter().format("\n")
                )
            }
            MachineStatement::OperationDeclaration(_, name, operation_id, params) => {
                let params_str = params.prepend_space_if_non_empty();
                write!(f, "operation {name}{operation_id}{params_str};")
            }
        }
    }
}

impl<T: Display> Display for OperationId<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.id {
            Some(id) => write!(f, "<{id}>"),
            None => write!(f, ""),
        }
    }
}

impl Display for AssignmentRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            match self {
                Self::Register(r) => r.to_string(),
                Self::Wildcard => "_".to_string(),
            }
        )
    }
}

impl<T: Display> Display for FunctionStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FunctionStatement::Assignment(_, write_regs, assignment_reg, expression) => write!(
                f,
                "{} <={}= {};",
                write_regs.join(", "),
                assignment_reg
                    .as_ref()
                    .map(|s| s.iter().format(", ").to_string())
                    .unwrap_or_default(),
                expression
            ),
            FunctionStatement::Instruction(_, name, inputs) => write!(
                f,
                "{}{};",
                name,
                if inputs.is_empty() {
                    "".to_string()
                } else {
                    format!(" {}", inputs.iter().format(", "))
                }
            ),
            FunctionStatement::Label(_, name) => write!(f, "{name}::"),
            FunctionStatement::DebugDirective(_, dir) => write!(f, "{dir}"),
            FunctionStatement::Return(_, values) => write!(
                f,
                "return{};",
                if values.is_empty() {
                    "".to_string()
                } else {
                    format!(" {}", values.iter().format(", "))
                }
            ),
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
            RegisterFlag::IsReadOnly => write!(f, "@r"),
        }
    }
}

impl Display for Params {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}",
            self.inputs,
            self.outputs
                .as_ref()
                .map(|outputs| format!(
                    "{}-> {}",
                    if self.inputs.params.is_empty() {
                        ""
                    } else {
                        " "
                    },
                    outputs
                ))
                .unwrap_or_default()
        )
    }
}

impl Display for ParamList {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.params.iter().format(", "))
    }
}

impl<T: Display, Ref: Display> Display for IndexAccess<T, Ref> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}[{}]", self.array, self.index)
    }
}

impl<T: Display, Ref: Display> Display for FunctionCall<T, Ref> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}({})", self.id, format_expressions(&self.arguments))
    }
}

impl<T: Display, Ref: Display> Display for MatchArm<T, Ref> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} => {},", self.pattern, self.value,)
    }
}

impl<T: Display, Ref: Display> Display for MatchPattern<T, Ref> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            MatchPattern::CatchAll => write!(f, "_"),
            MatchPattern::Pattern(p) => write!(f, "{p}"),
        }
    }
}

impl Display for Param {
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
            PilStatement::LetStatement(_, name, None) => write!(f, "let {name};"),
            PilStatement::LetStatement(_, name, Some(expr)) => write!(f, "let {name} = {expr};"),
            PilStatement::PolynomialDefinition(_, name, value) => {
                write!(f, "pol {name} = {value};")
            }
            PilStatement::PublicDeclaration(_, name, poly, array_index, index) => {
                write!(
                    f,
                    "public {name} = {poly}{}({index});",
                    array_index
                        .as_ref()
                        .map(|i| format!("[{i}]"))
                        .unwrap_or_default()
                )
            }
            PilStatement::PolynomialConstantDeclaration(_, names) => {
                write!(f, "pol constant {};", names.iter().format(", "))
            }
            PilStatement::PolynomialConstantDefinition(_, name, definition) => {
                write!(f, "pol constant {name}{definition};")
            }
            PilStatement::PolynomialCommitDeclaration(_, names, value) => {
                write!(
                    f,
                    "pol commit {}{};",
                    names.iter().format(", "),
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
            FunctionDefinition::Expression(e) => {
                write!(f, " = {e}")
            }
        }
    }
}

pub fn format_expressions<T: Display, Ref: Display>(expressions: &[Expression<T, Ref>]) -> String {
    format!("{}", expressions.iter().format(", "))
}

impl<T: Display, Ref: Display> Display for Expression<T, Ref> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expression::Reference(reference) => write!(f, "{reference}"),
            Expression::PublicReference(name) => write!(f, ":{name}"),
            Expression::Number(value) => write!(f, "{value}"),
            Expression::String(value) => write!(f, "\"{value}\""), // TODO quote?
            Expression::Tuple(items) => write!(f, "({})", format_expressions(items)),
            Expression::LambdaExpression(lambda) => write!(f, "{}", lambda),
            Expression::ArrayLiteral(array) => write!(f, "{array}"),
            Expression::BinaryOperation(left, op, right) => write!(f, "({left} {op} {right})"),
            Expression::UnaryOperation(op, exp) => {
                if op.is_prefix() {
                    write!(f, "{op}{exp}")
                } else {
                    write!(f, "{exp}{op}")
                }
            }
            Expression::IndexAccess(index_access) => write!(f, "{index_access}"),
            Expression::FunctionCall(fun_call) => write!(f, "{fun_call}"),
            Expression::FreeInput(input) => write!(f, "${{ {input} }}"),
            Expression::MatchExpression(scrutinee, arms) => {
                write!(f, "match {scrutinee} {{ {} }}", arms.iter().format(" "))
            }
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

impl Display for NamespacedPolynomialReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}",
            self.namespace
                .as_ref()
                .map(|n| format!("{n}."))
                .unwrap_or_default(),
            self.name
        )
    }
}

impl<T: Display, Ref: Display> Display for LambdaExpression<T, Ref> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "|{}| {}", self.params.iter().format(", "), self.body)
    }
}

impl<T: Display, Ref: Display> Display for ArrayLiteral<T, Ref> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "[{}]", self.items.iter().format(", "))
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
                BinaryOperator::LogicalOr => "||",
                BinaryOperator::LogicalAnd => "&&",
                BinaryOperator::Less => "<",
                BinaryOperator::LessEqual => "<=",
                BinaryOperator::Equal => "==",
                BinaryOperator::NotEqual => "!=",
                BinaryOperator::GreaterEqual => ">=",
                BinaryOperator::Greater => ">",
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
                UnaryOperator::LogicalNot => "!",
                UnaryOperator::Next => "'",
            }
        )
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn params() {
        let p = Param {
            name: "abc".into(),
            ty: Some("ty".into()),
        };
        assert_eq!(p.to_string(), "abc: ty");
        let l = ParamList { params: vec![p] };
        assert_eq!(l.to_string(), "abc: ty");
        let empty = Params::default();
        assert_eq!(empty.to_string(), "");
        assert_eq!(empty.prepend_space_if_non_empty(), "");
        let in_out = Params {
            inputs: ParamList {
                params: vec![
                    Param {
                        name: "abc".into(),
                        ty: Some("ty0".into()),
                    },
                    Param {
                        name: "def".into(),
                        ty: Some("ty1".into()),
                    },
                ],
            },
            outputs: Some(ParamList {
                params: vec![
                    Param {
                        name: "abc".into(),
                        ty: Some("ty0".into()),
                    },
                    Param {
                        name: "def".into(),
                        ty: Some("ty1".into()),
                    },
                ],
            }),
        };
        assert_eq!(
            in_out.to_string(),
            "abc: ty0, def: ty1 -> abc: ty0, def: ty1"
        );
        assert_eq!(
            in_out.prepend_space_if_non_empty(),
            " abc: ty0, def: ty1 -> abc: ty0, def: ty1"
        );
        let out = Params {
            inputs: ParamList { params: vec![] },
            outputs: Some(ParamList {
                params: vec![Param {
                    name: "abc".into(),
                    ty: Some("ty".into()),
                }],
            }),
        };
        assert_eq!(out.to_string(), "-> abc: ty");
        assert_eq!(out.prepend_space_if_non_empty(), " -> abc: ty");
        let _in = Params {
            inputs: ParamList {
                params: vec![Param {
                    name: "abc".into(),
                    ty: Some("ty".into()),
                }],
            },
            outputs: None,
        };
        assert_eq!(_in.to_string(), "abc: ty");
        assert_eq!(_in.prepend_space_if_non_empty(), " abc: ty");
    }
}
