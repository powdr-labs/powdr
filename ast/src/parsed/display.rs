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
                SymbolValue::Machine(
                    m @ Machine {
                        arguments:
                            MachineArguments {
                                latch,
                                operation_id,
                            },
                        ..
                    },
                ) => match (latch, operation_id) {
                    (None, None) => write!(f, "machine {name} {m}"),
                    (Some(latch), None) => write!(f, "machine {name}({latch}, _) {m}"),
                    (None, Some(op_id)) => write!(f, "machine {name}(_, {op_id}) {m}"),
                    (Some(latch), Some(op_id)) => write!(f, "machine {name}({latch}, {op_id}) {m}"),
                },
                SymbolValue::Import(i) => {
                    write!(f, "{i} as {name};")
                }
                SymbolValue::Module(m @ Module::External(_)) => {
                    write!(f, "mod {m}")
                }
                SymbolValue::Module(m @ Module::Local(_)) => {
                    write!(f, "mod {name} {m}")
                }
                SymbolValue::Expression(ExpressionWithTypeScheme { e, type_scheme }) => {
                    write!(
                        f,
                        "let{} {name}{} = {e};",
                        type_scheme
                            .as_ref()
                            .map(|ts| ts.type_vars_to_string())
                            .unwrap_or_default(),
                        type_scheme
                            .as_ref()
                            .map(|t| format!(": {}", t.type_name))
                            .unwrap_or_default()
                    )
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
            InstructionBody::Local(elements) => write!(
                f,
                "{{ {} }}",
                elements
                    .iter()
                    .map(format_instruction_statement)
                    .format(", ")
            ),
            InstructionBody::CallableRef(r) => write!(f, " = {r};"),
        }
    }
}

fn format_instruction_statement<T: Display>(stmt: &PilStatement<T>) -> String {
    match stmt {
        PilStatement::Expression(_, _)
        | PilStatement::PlookupIdentity(_, _, _)
        | PilStatement::PermutationIdentity(_, _, _)
        | PilStatement::ConnectIdentity(_, _, _) => {
            // statements inside instruction definition don't end in semicolon
            let mut s = format!("{stmt}");
            assert_eq!(s.pop(), Some(';'));
            s
        }
        _ => panic!("invalid statement inside instruction body: {}", stmt),
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
            "link {}{} => {};",
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
            MachineStatement::Pil(_, statement) => write!(f, "{statement}"),
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
            MachineStatement::LinkDeclaration(_, link) => {
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
            FunctionStatement::Label(_, name) => write!(f, "{name}:"),
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
                write!(f, ".debug file {nr} {} {};", quote(path), quote(file))
            }
            DebugDirective::Loc(file, line, col) => {
                write!(f, ".debug loc {file} {line} {col};")
            }
            DebugDirective::OriginalInstruction(insn) => {
                write!(f, ".debug insn \"{insn}\";")
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

impl<T: Display> Display for Params<T> {
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

impl<T: Display> Display for ParamList<T> {
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
        write!(
            f,
            "{}({})",
            self.function,
            format_expressions(&self.arguments)
        )
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

impl<T: Display, Ref: Display> Display for IfExpression<T, Ref> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "if {} {{ {} }} else {{ {} }}",
            self.condition, self.body, self.else_body
        )
    }
}

impl<T: Display> Display for Param<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}{}",
            self.name,
            self.index
                .as_ref()
                .map(|i| format!("[{i}]"))
                .unwrap_or_default(),
            self.ty
                .as_ref()
                .map(|ty| format!(": {}", ty))
                .unwrap_or_default()
        )
    }
}

pub fn quote(input: &str) -> String {
    format!("\"{}\"", input.escape_default())
}

impl<T: Display> Display for PilStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            PilStatement::Include(_, path) => write!(f, "include {};", quote(path)),
            PilStatement::Namespace(_, name, poly_length) => {
                write!(f, "namespace {name}({poly_length});")
            }
            PilStatement::LetStatement(_, name, type_scheme, value) => {
                write!(
                    f,
                    "    let{} {name}{}",
                    type_scheme
                        .as_ref()
                        .map(|ts| ts.type_vars_to_string())
                        .unwrap_or_default(),
                    type_scheme
                        .as_ref()
                        .map(|ts| format!(": {}", ts.type_name))
                        .unwrap_or_default()
                )?;
                if let Some(value) = &value {
                    write!(f, " = {value}")?;
                }
                write!(f, ";")
            }
            PilStatement::PolynomialDefinition(_, name, value) => {
                write!(f, "    pol {name} = {value};")
            }
            PilStatement::PublicDeclaration(_, name, poly, array_index, index) => {
                write!(
                    f,
                    "    public {name} = {poly}{}({index});",
                    array_index
                        .as_ref()
                        .map(|i| format!("[{i}]"))
                        .unwrap_or_default()
                )
            }
            PilStatement::PolynomialConstantDeclaration(_, names) => {
                write!(f, "    pol constant {};", names.iter().format(", "))
            }
            PilStatement::PolynomialConstantDefinition(_, name, definition) => {
                write!(f, "    pol constant {name}{definition};")
            }
            PilStatement::PolynomialCommitDeclaration(_, names, value) => {
                write!(
                    f,
                    "    pol commit {}{};",
                    names.iter().format(", "),
                    value.as_ref().map(|v| format!("{v}")).unwrap_or_default()
                )
            }
            PilStatement::PlookupIdentity(_, left, right) => write!(f, "    {left} in {right};"),
            PilStatement::PermutationIdentity(_, left, right) => {
                write!(f, "    {left} is {right};")
            }
            PilStatement::ConnectIdentity(_, left, right) => write!(
                f,
                "    {{ {} }} connect {{ {} }};",
                format_expressions(left),
                format_expressions(right)
            ),
            PilStatement::ConstantDefinition(_, name, value) => {
                write!(f, "    constant {name} = {value};")
            }
            PilStatement::Expression(_, e) => {
                write!(f, "    {e};")
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
            FunctionDefinition::Array(array_expression) => {
                write!(f, " = {array_expression}")
            }
            FunctionDefinition::Query(Expression::LambdaExpression(lambda)) => write!(
                f,
                "({}) query {}",
                lambda.params.iter().format(", "),
                lambda.body,
            ),
            FunctionDefinition::Query(e) => {
                write!(f, " query = {e}")
            }
            FunctionDefinition::Expression(Expression::LambdaExpression(lambda))
                if lambda.params.len() == 1 =>
            {
                write!(
                    f,
                    "({}) {{ {} }}",
                    lambda.params.iter().format(", "),
                    lambda.body,
                )
            }
            FunctionDefinition::Expression(e) => write!(f, " = {e}"),
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
            Expression::String(value) => write!(f, "{}", quote(value)),
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
            Expression::IfExpression(e) => write!(f, "{e}"),
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
        write!(f, "{}", self.path.to_dotted_string())
    }
}

impl<T: Display, Ref: Display> Display for LambdaExpression<T, Ref> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "(|{}| {})", self.params.iter().format(", "), self.body)
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
                BinaryOperator::Identity => "=",
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
                UnaryOperator::LogicalNot => "!",
                UnaryOperator::Next => "'",
            }
        )
    }
}

impl<E: Display> Display for TypeName<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            TypeName::Bool => write!(f, "bool"),
            TypeName::Int => write!(f, "int"),
            TypeName::Fe => write!(f, "fe"),
            TypeName::String => write!(f, "string"),
            TypeName::Col => write!(f, "col"),
            TypeName::Expr => write!(f, "expr"),
            TypeName::Constr => write!(f, "constr"),
            TypeName::Array(array) => write!(f, "{array}"),
            TypeName::Tuple(tuple) => write!(f, "{tuple}"),
            TypeName::Function(fun) => write!(f, "{fun}"),
            TypeName::TypeVar(name) => write!(f, "{name}"),
        }
    }
}

impl<E: Display> Display for ArrayTypeName<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.base.needs_parentheses() {
            write!(f, "({})", self.base)
        } else {
            write!(f, "{}", self.base)
        }?;
        write!(
            f,
            "[{}]",
            self.length
                .as_ref()
                .map(|l| l.to_string())
                .unwrap_or_default()
        )
    }
}

impl<E: Display> Display for TupleTypeName<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({})", self.items.iter().format(", "))
    }
}

impl<E: Display> Display for FunctionTypeName<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let params = self
            .params
            .iter()
            .map(|x| {
                if x.needs_parentheses() {
                    format!("({x})")
                } else {
                    format!("{x}")
                }
            })
            .join(", ")
            + if self.params.is_empty() { "" } else { " " };

        write!(
            f,
            "{params}-> {}",
            if self.value.needs_parentheses() {
                format!("({})", self.value)
            } else {
                format!("{}", self.value)
            }
        )
    }
}

impl Display for TypeBounds {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|(v, b)| {
                    format!(
                        "{v}{}",
                        if b.is_empty() {
                            String::new()
                        } else {
                            format!(": {}", b.iter().join(" + "))
                        }
                    )
                })
                .format(", ")
        )
    }
}

#[cfg(test)]
mod tests {

    use powdr_number::GoldilocksField;

    use super::*;

    #[test]
    fn params() {
        let p = Param::<GoldilocksField> {
            name: "abc".into(),
            index: None,
            ty: Some("ty".into()),
        };
        assert_eq!(p.to_string(), "abc: ty");
        let l = ParamList { params: vec![p] };
        assert_eq!(l.to_string(), "abc: ty");
        let empty = Params::<GoldilocksField>::default();
        assert_eq!(empty.to_string(), "");
        assert_eq!(empty.prepend_space_if_non_empty(), "");
        let in_out = Params::<GoldilocksField> {
            inputs: ParamList {
                params: vec![
                    Param {
                        name: "abc".into(),
                        index: Some(7.into()),
                        ty: Some("ty0".into()),
                    },
                    Param {
                        name: "def".into(),
                        index: None,
                        ty: Some("ty1".into()),
                    },
                ],
            },
            outputs: Some(ParamList {
                params: vec![
                    Param {
                        name: "abc".into(),
                        index: None,
                        ty: Some("ty0".into()),
                    },
                    Param {
                        name: "def".into(),
                        index: Some(2.into()),
                        ty: Some("ty1".into()),
                    },
                ],
            }),
        };
        assert_eq!(
            in_out.to_string(),
            "abc[7]: ty0, def: ty1 -> abc: ty0, def[2]: ty1"
        );
        assert_eq!(
            in_out.prepend_space_if_non_empty(),
            " abc[7]: ty0, def: ty1 -> abc: ty0, def[2]: ty1"
        );
        let out = Params::<GoldilocksField> {
            inputs: ParamList { params: vec![] },
            outputs: Some(ParamList {
                params: vec![Param {
                    name: "abc".into(),
                    index: None,
                    ty: Some("ty".into()),
                }],
            }),
        };
        assert_eq!(out.to_string(), "-> abc: ty");
        assert_eq!(out.prepend_space_if_non_empty(), " -> abc: ty");
        let _in = Params::<GoldilocksField> {
            inputs: ParamList {
                params: vec![Param {
                    name: "abc".into(),
                    index: None,
                    ty: Some("ty".into()),
                }],
            },
            outputs: None,
        };
        assert_eq!(_in.to_string(), "abc: ty");
        assert_eq!(_in.prepend_space_if_non_empty(), " abc: ty");
    }

    #[test]
    fn symbol_paths() {
        let s = SymbolPath::from_parts(vec![
            Part::Named("x".to_string()),
            Part::Super,
            Part::Named("y".to_string()),
        ]);
        assert_eq!(s.to_string(), "x::super::y");
        let p = parse_absolute_path("::abc");
        assert_eq!(p.to_string(), "::abc");

        assert_eq!(p.with_part("t").to_string(), "::abc::t");

        assert_eq!(p.clone().join(s.clone()).to_string(), "::abc::y");
        assert_eq!(SymbolPath::from(p.join(s)).to_string(), "::abc::y");
    }
}
