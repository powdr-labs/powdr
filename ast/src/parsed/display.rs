use std::fmt::{Display, Formatter, Result};

use itertools::Itertools;

use crate::{
    indent,
    parsed::{BinaryOperator, UnaryOperator},
    write_indented_by, write_items, write_items_indented, writeln_indented,
};

use self::types::{ArrayType, FunctionType, TupleType, TypeBounds};

use super::{asm::*, *};

impl Display for PILFile {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for statement in &self.0 {
            match statement {
                PilStatement::Namespace(..) => writeln!(f, "{statement}")?,
                _ => writeln_indented(f, statement.to_string())?,
            }
        }
        Ok(())
    }
}

impl Display for ASMProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.main)
    }
}

impl Display for ASMModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write_items(f, &self.statements)
    }
}

impl Display for ModuleStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ModuleStatement::SymbolDefinition(symbol_def) => write!(f, "{symbol_def}"),
            ModuleStatement::PilStatement(s) => write!(f, "{s}"),
        }
    }
}

impl Display for SymbolDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let SymbolDefinition { name, value } = self;
        match value {
            SymbolValue::Machine(m) => {
                write!(f, "machine {name}{m}")
            }
            SymbolValue::Import(i) => {
                write!(f, "{i} as {name};")
            }
            SymbolValue::Module(m @ Module::External(_)) => {
                write!(f, "mod {m}")
            }
            SymbolValue::Module(m @ Module::Local(_)) => {
                write!(f, "mod {name} {m}")
            }
        }
    }
}

impl Display for Module {
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

impl Display for Machine {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "{}{} {{", &self.params, &self.properties)?;
        write_items_indented(f, &self.statements)?;
        write!(f, "}}")
    }
}

impl Display for MachineParams {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let args = self.0.iter().join(", ");
        if !args.is_empty() {
            write!(f, "({args})")?;
        }
        Ok(())
    }
}

impl Display for MachineProperties {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let props = self
            .degree
            .as_ref()
            .map(|s| format!("degree: {s}"))
            .into_iter()
            .chain(self.min_degree.as_ref().map(|s| format!("min_degree: {s}")))
            .chain(self.max_degree.as_ref().map(|s| format!("max_degree: {s}")))
            .chain(self.latch.as_ref().map(|s| format!("latch: {s}")))
            .chain(
                self.operation_id
                    .as_ref()
                    .map(|s| format!("operation_id: {s}")),
            )
            .chain(
                self.call_selectors
                    .as_ref()
                    .map(|s| format!("call_selectors: {s}")),
            )
            .join(", ");
        if !props.is_empty() {
            write!(f, " with {props}")?;
        }
        Ok(())
    }
}

impl Display for InstructionBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{{ {} }}",
            self.0.iter().map(format_instruction_statement).format(", ")
        )
    }
}

fn format_instruction_statement(stmt: &PilStatement) -> String {
    if let PilStatement::Expression(_, _) = stmt {
        // statements inside instruction definition don't end in semicolon
        let mut s = format!("{stmt}");
        assert_eq!(s.pop(), Some(';'));
        s
    } else {
        panic!("invalid statement inside instruction body: {stmt}")
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}{}",
            self.params.prepend_space_if_non_empty(),
            if self.links.is_empty() {
                "".to_string()
            } else {
                " ".to_string() + &self.links.iter().join(" ")
            },
            self.body
        )
    }
}

impl Display for LinkDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "link {}{} {}",
            if self.flag == 1.into() {
                "".to_string()
            } else {
                format!("if {} ", self.flag)
            },
            if self.is_permutation { "~>" } else { "=>" },
            self.link,
        )
    }
}

impl Display for CallableRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}.{}({})",
            match &self.params.outputs[..] {
                [] => "".to_string(),
                [output] => format!("{output} = "),
                outputs => format!("({}) = ", outputs.iter().join(", ")),
            },
            self.instance,
            self.callable,
            self.params.inputs.iter().join(", ")
        )
    }
}

impl Display for MachineStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            MachineStatement::Pil(_, statement) => write!(f, "{statement}"),
            MachineStatement::Submachine(_, ty, name, args) => {
                let mut args = args.iter().join(", ");
                if !args.is_empty() {
                    args = format!("({args})");
                }
                write!(f, "{ty} {name}{args};")
            }
            MachineStatement::RegisterDeclaration(_, name, flag) => write!(
                f,
                "reg {}{};",
                name,
                flag.as_ref()
                    .map(|flag| format!("[{flag}]"))
                    .unwrap_or_default()
            ),
            MachineStatement::InstructionDeclaration(_, name, instruction) => {
                write!(f, "instr {name}{instruction}")
            }
            MachineStatement::LinkDeclaration(_, link) => {
                write!(f, "{link};")
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

impl Display for OperationId {
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

impl Display for FunctionStatement {
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
            "{}{}{}",
            self.inputs.iter().format(", "),
            if self.outputs.is_empty() {
                ""
            } else if self.inputs.is_empty() {
                "-> "
            } else {
                " -> "
            },
            self.outputs.iter().format(", ")
        )
    }
}

impl<E: Display> Display for IndexAccess<Expression<E>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.array.precedence().is_none() {
            write!(f, "{}[{}]", self.array, self.index)
        } else {
            write!(f, "({})[{}]", self.array, self.index)
        }
    }
}

impl<E: Display> Display for FunctionCall<Expression<E>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.function.precedence().is_none() {
            write!(f, "{}({})", self.function, format_list(&self.arguments))
        } else {
            write!(f, "({})({})", self.function, format_list(&self.arguments))
        }
    }
}

impl<E: Display> Display for MatchArm<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} => {},", self.pattern, self.value,)
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Pattern::CatchAll(_) => write!(f, "_"),
            Pattern::Ellipsis(_) => write!(f, ".."),
            Pattern::Number(_, n) => write!(f, "{n}"),
            Pattern::String(_, s) => write!(f, "{}", quote(s)),
            Pattern::Tuple(_, t) => write!(f, "({})", t.iter().format(", ")),
            Pattern::Array(_, a) => write!(f, "[{}]", a.iter().format(", ")),
            Pattern::Variable(_, v) => write!(f, "{v}"),
            Pattern::Enum(_, name, fields) => write!(
                f,
                "{name}{}",
                fields
                    .as_ref()
                    .map(|fields| format!("({})", fields.iter().format(", ")))
                    .unwrap_or_default()
            ),
        }
    }
}

impl<E: Display> Display for IfExpression<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "if {} {} else {}",
            self.condition, self.body, self.else_body
        )
    }
}

impl<E: Display> Display for StatementInsideBlock<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            StatementInsideBlock::LetStatement(s) => write!(f, "{s}"),
            StatementInsideBlock::Expression(e) => write!(f, "{e};"),
        }
    }
}

impl<E: Display> Display for LetStatementInsideBlock<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "let {}", self.pattern)?;
        if let Some(ty) = &self.ty {
            write!(f, ": {ty}")?;
        }
        if let Some(v) = &self.value {
            write!(f, " = {v};")
        } else {
            write!(f, ";")
        }
    }
}

impl Display for Param {
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
                .map(|ty| format!(": {ty}"))
                .unwrap_or_default()
        )
    }
}

pub fn quote(input: &str) -> String {
    format!("\"{}\"", input.escape_default())
}

impl Display for NamespaceDegree {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.min == self.max {
            write!(f, "{}", self.min)
        } else {
            write!(f, "{}..{}", self.min, self.max)
        }
    }
}

impl Display for PilStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            PilStatement::Include(_, path) => write!(f, "include {};", quote(path)),
            PilStatement::Namespace(_, name, poly_length) => {
                write!(f, "namespace")?;
                let name = name.to_string();
                match poly_length {
                    None if name.is_empty() => {
                        write!(f, ";")
                    }
                    None => {
                        write!(f, " {name};")
                    }
                    Some(poly_length) => {
                        write!(f, " {name}({poly_length});")
                    }
                }
            }
            PilStatement::LetStatement(_, pattern, type_scheme, value) => write!(
                f,
                "let{}{};",
                format_type_scheme_around_name(pattern, type_scheme),
                value
                    .as_ref()
                    .map(|value| format!(" = {value}"))
                    .unwrap_or_default()
            ),
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
            PilStatement::PolynomialConstantDefinition(_, name, definition) => {
                write!(f, "pol constant {name}{definition};")
            }
            PilStatement::PolynomialCommitDeclaration(_, stage, names, value) => write!(
                f,
                "pol commit {}{}{};",
                stage
                    .and_then(|s| (s > 0).then(|| format!("stage({s}) ")))
                    .unwrap_or_default(),
                names.iter().format(", "),
                value.as_ref().map(|v| format!("{v}")).unwrap_or_default()
            ),
            PilStatement::Expression(_, e) => write!(f, "{e};"),
            PilStatement::EnumDeclaration(_, enum_decl) => write!(f, "{enum_decl}"),
            PilStatement::StructDeclaration(_, struct_decl) => write!(f, "{struct_decl}"),
            PilStatement::TraitImplementation(_, trait_impl) => write!(f, "{trait_impl}"),
            PilStatement::TraitDeclaration(_, trait_decl) => write!(f, "{trait_decl}"),
        }
    }
}

impl<Ref: Display> Display for ArrayExpression<Ref> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ArrayExpression::Value(expressions) => {
                write!(f, "[{}]", format_list(expressions))
            }
            ArrayExpression::RepeatedValue(expressions) => {
                write!(f, "[{}]*", format_list(expressions))
            }
            ArrayExpression::Concat(left, right) => write!(f, "{left} + {right}"),
        }
    }
}

impl Display for FunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FunctionDefinition::Array(array_expression) => {
                write!(f, " = {array_expression}")
            }
            FunctionDefinition::Expression(Expression::LambdaExpression(_, lambda))
                if lambda.params.len() == 1 =>
            {
                write!(
                    f,
                    "({}) {}{}",
                    format_list(&lambda.params),
                    match lambda.kind {
                        FunctionKind::Pure => "".into(),
                        _ => format!("{} ", &lambda.kind),
                    },
                    lambda.body
                )
            }
            FunctionDefinition::Expression(e) => write!(f, " = {e}"),
            FunctionDefinition::PublicDeclaration(poly, array_index, index) => {
                write!(
                    f,
                    " = {poly}{}({index});",
                    array_index
                        .as_ref()
                        .map(|i| format!("[{i}]"))
                        .unwrap_or_default()
                )
            }
            FunctionDefinition::TraitDeclaration(_) | FunctionDefinition::TypeDeclaration(_) => {
                panic!("Should not use this formatting function.")
            }
        }
    }
}

impl<E: Display> Display for TraitDeclaration<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.to_string_with_name(&self.name))
    }
}

impl<E: Display> TraitDeclaration<E> {
    /// Formats the trait declaration, exchanging its name by the provided one.
    pub fn to_string_with_name(&self, name: &str) -> String {
        format!(
            "trait {name}<{}> {{\n{}}}",
            self.type_vars.iter().format(", "),
            indent(
                self.functions.iter().map(|m| format!("{m},\n")).format(""),
                1
            )
        )
    }
}

impl<E: Display> Display for NamedType<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl<E: Display> Display for EnumDeclaration<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.to_string_with_name(&self.name))
    }
}

impl<E: Display> EnumDeclaration<E> {
    /// Formats the enum declaration, exchanging its name by the provided one.
    pub fn to_string_with_name(&self, name: &str) -> String {
        let type_vars = if self.type_vars.is_empty() {
            Default::default()
        } else {
            format!("<{}>", self.type_vars)
        };
        format!(
            "enum {name}{type_vars} {{\n{}}}",
            indent(
                self.variants.iter().map(|v| format!("{v},\n")).format(""),
                1
            )
        )
    }
}

impl<E: Display> Display for TraitImplementation<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let type_vars = if self.type_scheme.vars.is_empty() {
            Default::default()
        } else {
            format!("<{}>", self.type_scheme.vars)
        };

        let Type::Tuple(TupleType { items }) = &self.type_scheme.ty else {
            panic!("Type from trait scheme is not a tuple.")
        };

        write!(
            f,
            "impl{type_vars} {trait_name}{type_args} {{\n{methods}}}",
            trait_name = self.name,
            type_args = format_type_args(items),
            methods = indent(
                self.functions.iter().map(|m| format!("{m},\n")).format(""),
                1
            )
        )
    }
}

impl<Expr: Display> Display for SelectedExpressions<Expr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}",
            self.selector
                .as_ref()
                .map(|s| format!("{s} $ "))
                .unwrap_or_default(),
            self.expressions
        )
    }
}

impl<E: Display> Display for StructDeclaration<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.to_string_with_name(&self.name))
    }
}

impl<E: Display> StructDeclaration<E> {
    /// Formats the struct declaration, exchanging its name by the provided one.
    pub fn to_string_with_name(&self, name: &str) -> String {
        format!(
            "struct {name}{} {{\n{}}}",
            type_vars_to_string(&self.type_vars),
            indent(
                self.fields
                    .iter()
                    .map(|named| format!("{}: {},\n", named.name, named.ty))
                    .format(""),
                1
            )
        )
    }
}

impl<E: Display> Display for StructExpression<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}",
            self.name,
            if self.fields.is_empty() {
                "{}".to_string()
            } else {
                format!("{{ {} }}", self.fields.iter().join(", "))
            }
        )
    }
}

impl<E: Display> Display for NamedExpression<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: {}", self.name, self.body)
    }
}

impl<E: Display> Display for EnumVariant<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.name)?;
        if let Some(fields) = &self.fields {
            write!(
                f,
                "({})",
                fields.iter().map(format_type_with_parentheses).format(", ")
            )?;
        }
        Ok(())
    }
}

fn format_list<L: IntoIterator<Item = I>, I: Display>(list: L) -> String {
    format!("{}", list.into_iter().format(", "))
}

impl<Ref: Display> Display for Expression<Ref> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expression::Reference(_, reference) => write!(f, "{reference}"),
            Expression::Number(_, n) => write!(f, "{n}"),
            Expression::String(_, value) => write!(f, "{}", quote(value)),
            Expression::Tuple(_, items) => write!(f, "({})", format_list(items)),
            Expression::LambdaExpression(_, lambda) => write!(f, "{lambda}"),
            Expression::ArrayLiteral(_, array) => write!(f, "{array}"),
            Expression::BinaryOperation(_, binaryop) => {
                write!(f, "{binaryop}")
            }
            Expression::UnaryOperation(_, unaryop) => {
                write!(f, "{unaryop}")
            }
            Expression::IndexAccess(_, index_access) => write!(f, "{index_access}"),
            Expression::FunctionCall(_, fun_call) => write!(f, "{fun_call}"),
            Expression::FreeInput(_, input) => write!(f, "${{ {input} }}"),
            Expression::MatchExpression(_, match_expr) => {
                write!(f, "{match_expr}")
            }
            Expression::IfExpression(_, e) => write!(f, "{e}"),
            Expression::BlockExpression(_, block_expr) => {
                write!(f, "{block_expr}")
            }
            Expression::StructExpression(_, s) => write!(f, "{s}"),
        }
    }
}

impl Display for PolynomialName {
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
        if let Some(type_args) = &self.type_args {
            write!(f, "{}::{}", self.path, format_type_args(type_args))
        } else {
            write!(f, "{}", self.path)
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let Number { value, type_ } = self;
        write!(f, "{value}")?;
        match type_ {
            Some(ty @ (Type::Int | Type::Fe | Type::Expr)) => write!(f, "_{ty}"),
            Some(Type::TypeVar(_)) | None => Ok(()),
            Some(_) => unreachable!(),
        }
    }
}

impl<E> Display for LambdaExpression<E>
where
    E: Display + Precedence,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let prefix = match self.kind {
            FunctionKind::Pure => "".into(),
            _ => format!("{} ", &self.kind),
        };
        let params = format_list(&self.params);

        if self.body.precedence() < self.precedence() {
            write!(f, "{}|{}| {}", prefix, params, self.body)
        } else {
            write!(f, "{}|{}| ({})", prefix, params, self.body)
        }
    }
}

impl<E: Display> Display for MatchExpression<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "match {} {{", self.scrutinee)?;
        write_items_indented(f, &self.arms)?;
        write!(f, "}}")
    }
}

impl Display for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::Pure => write!(f, "pure"),
            FunctionKind::Constr => write!(f, "constr"),
            FunctionKind::Query => write!(f, "query"),
        }
    }
}

impl<E: Display> Display for ArrayLiteral<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "[{}]", format_list(&self.items))
    }
}

impl<E> Display for BinaryOperation<E>
where
    E: Display + Precedence,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let force_parentheses = matches!(self.op, BinaryOperator::Pow);

        let op_precedence = self.op.precedence().unwrap();
        let use_left_parentheses = match self.left.precedence() {
            Some(left_precedence) => {
                force_parentheses
                    || left_precedence > op_precedence
                    || (left_precedence == op_precedence
                        && self.op.associativity() != BinaryOperatorAssociativity::Left)
            }
            None => false,
        };

        let use_right_parentheses = match self.right.precedence() {
            Some(right_precedence) => {
                force_parentheses
                    || right_precedence > op_precedence
                    || (right_precedence == op_precedence
                        && self.op.associativity() != BinaryOperatorAssociativity::Right)
            }
            None => false,
        };

        let left_string = if use_left_parentheses {
            format!("({})", self.left)
        } else {
            format!("{}", self.left)
        };
        let right_string = if use_right_parentheses {
            format!("({})", self.right)
        } else {
            format!("{}", self.right)
        };

        write!(f, "{left_string} {} {right_string}", self.op)
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
                BinaryOperator::In => "in",
                BinaryOperator::Is => "is",
                BinaryOperator::Connect => "connect",
                BinaryOperator::Select => "$",
            }
        )
    }
}

impl<E> Display for UnaryOperation<E>
where
    E: Display + Precedence,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let exp_string = match (self.op.precedence(), self.expr.precedence()) {
            (Some(precedence), Some(inner_precedence)) if precedence < inner_precedence => {
                format!("({})", self.expr)
            }
            _ => {
                format!("{}", self.expr)
            }
        };

        if self.op.is_prefix() {
            write!(f, "{}{exp_string}", self.op)
        } else {
            write!(f, "{exp_string}{}", self.op)
        }
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

impl<E: Display> Display for BlockExpression<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.statements.is_empty() {
            if let Some(expr) = &self.expr {
                write!(f, "{{ {expr} }}")
            } else {
                write!(f, "{{ }}")
            }
        } else {
            writeln!(f, "{{")?;
            write_items_indented(f, &self.statements)?;
            if let Some(expr) = &self.expr {
                write_indented_by(f, expr, 1)?;
                writeln!(f)?;
            }
            write!(f, "}}")
        }
    }
}

impl<E: Display> Display for Type<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Type::Bottom => write!(f, "!"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Fe => write!(f, "fe"),
            Type::String => write!(f, "string"),
            Type::Col => write!(f, "col"),
            Type::Inter => write!(f, "inter"),
            Type::Expr => write!(f, "expr"),
            Type::Array(array) => write!(f, "{array}"),
            Type::Tuple(tuple) => write!(f, "{tuple}"),
            Type::Function(fun) => write!(f, "{fun}"),
            Type::TypeVar(name) => write!(f, "{name}"),
            Type::NamedType(name, Some(args)) => {
                write!(f, "{name}{}", format_type_args(args))
            }
            Type::NamedType(name, None) => write!(f, "{name}"),
        }
    }
}

impl<E: Display> Display for ArrayType<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}[{}]",
            format_type_with_parentheses(&self.base),
            self.length.iter().format("")
        )
    }
}

impl<E: Display> Display for TupleType<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({})", format_list_of_types(&self.items))
    }
}

impl<E: Display> Display for FunctionType<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}-> {}",
            format_list_of_types(&self.params),
            if self.params.is_empty() { "" } else { " " },
            format_type_with_parentheses(&self.value)
        )
    }
}

fn format_type_with_parentheses<E: Display>(name: &Type<E>) -> String {
    if name.needs_parentheses() {
        format!("({name})")
    } else {
        name.to_string()
    }
}

fn format_list_of_types<E: Display>(types: &[Type<E>]) -> String {
    types
        .iter()
        .map(format_type_with_parentheses)
        .format(", ")
        .to_string()
}

/// Formats a list of types to be used as values for type arguments
/// and puts them in angle brackets.
/// It might add parentheses and spaces to avoid parser problems.
pub fn format_type_args<E: Display>(args: &[Type<E>]) -> String {
    format!(
        "<{}>",
        args.iter()
            .rev()
            .enumerate()
            .map(|(i, t)| match t {
                // Function types need parentheses if at the outermost level,
                // because of the '>' in '->'
                Type::Function(_) => format!("({t})"),
                // For generic types we add a space to avoid '>>' at the end.
                Type::NamedType(_, Some(_)) if i == 0 => format!("{t} "),
                _ => format!("{t}"),
            })
            .rev()
            .join(", ")
    )
}

pub fn format_type_scheme_around_name<E: Display, N: Display>(
    name: &N,
    type_scheme: &Option<TypeScheme<E>>,
) -> String {
    if let Some(type_scheme) = type_scheme {
        format!(
            "{} {name}: {}",
            type_scheme.type_vars_to_string(),
            type_scheme.ty
        )
    } else {
        format!(" {name}")
    }
}

pub fn type_vars_to_string(type_vars: &TypeBounds) -> String {
    if type_vars.is_empty() {
        Default::default()
    } else {
        format!("<{type_vars}>")
    }
}

impl Display for TypeBounds {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            self.bounds()
                .map(|(var, bounds)| TypeBounds::format_var_bound(var, bounds))
                .format(", ")
        )
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn params() {
        let p = Param {
            source: SourceRef::unknown(),
            name: "abc".into(),
            index: None,
            ty: "ty".parse().ok(),
        };
        assert_eq!(p.to_string(), "abc: ty");
        let empty = Params::<Param>::default();
        assert_eq!(empty.to_string(), "");
        assert_eq!(empty.prepend_space_if_non_empty(), "");
        let in_out = Params {
            inputs: vec![
                Param {
                    source: SourceRef::unknown(),
                    name: "abc".into(),
                    index: Some(7u32.into()),
                    ty: "ty0".parse().ok(),
                },
                Param {
                    source: SourceRef::unknown(),
                    name: "def".into(),
                    index: None,
                    ty: "ty1".parse().ok(),
                },
            ],
            outputs: vec![
                Param {
                    source: SourceRef::unknown(),
                    name: "abc".into(),
                    index: None,
                    ty: "ty0".parse().ok(),
                },
                Param {
                    source: SourceRef::unknown(),
                    name: "def".into(),
                    index: Some(2u32.into()),
                    ty: "ty1".parse().ok(),
                },
            ],
        };
        assert_eq!(
            in_out.to_string(),
            "abc[7]: ty0, def: ty1 -> abc: ty0, def[2]: ty1"
        );
        assert_eq!(
            in_out.prepend_space_if_non_empty(),
            " abc[7]: ty0, def: ty1 -> abc: ty0, def[2]: ty1"
        );
        let out = Params {
            inputs: vec![],
            outputs: vec![Param {
                source: SourceRef::unknown(),
                name: "abc".into(),
                index: None,
                ty: "ty".parse().ok(),
            }],
        };
        assert_eq!(out.to_string(), "-> abc: ty");
        assert_eq!(out.prepend_space_if_non_empty(), " -> abc: ty");
        let _in = Params {
            inputs: vec![Param {
                source: SourceRef::unknown(),
                name: "abc".into(),
                index: None,
                ty: "ty".parse().ok(),
            }],
            outputs: vec![],
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

    #[cfg(test)]
    mod parentheses {
        use powdr_parser::parse;
        use powdr_parser_util::UnwrapErrToStderr;
        use pretty_assertions::assert_eq;
        use test_log::test;

        type TestCase = (&'static str, &'static str);

        fn test_paren(test_case: &TestCase) {
            let (input, expected) = test_case;
            let parsed = parse(None, input).unwrap_err_to_stderr();
            let printed = parsed.to_string();
            assert_eq!(expected.trim(), printed.trim());
            let re_parsed = parse(None, printed.as_str()).unwrap_err_to_stderr();
            assert_eq!(parsed, re_parsed);
        }

        #[test]
        fn binary_op() {
            let test_cases: Vec<TestCase> = vec![
                // Complete line
                ("let t = ((x + y) * z);", "let t = (x + y) * z;"),
                // Don't add extra
                ("-x + y * !z;", "-x + y * !z;"),
                ("x = (y <= z);", "x = (y <= z);"),
                ("(x = y) <= z;", "(x = y) <= z;"),
                ("x + y + z;", "x + y + z;"),
                ("x * y * z;", "x * y * z;"),
                ("x / y / z;", "x / y / z;"),
                // Remove unneeded
                ("(-x) + y * (!z);", "-x + y * !z;"),
                ("(x * y) * z;", "x * y * z;"),
                ("(x / y) / z;", "x / y / z;"),
                ("(x ** (y ** z));", "x ** (y ** z);"),
                ("(x - (y + z));", "x - (y + z);"),
                // Observe associativity
                ("x * (y * z);", "x * (y * z);"),
                ("x / (y / z);", "x / (y / z);"),
                ("x ** (y ** z);", "x ** (y ** z);"),
                ("(x ** y) ** z;", "(x ** y) ** z;"),
                // Don't remove needed
                ("(x + y) * z;", "(x + y) * z;"),
                ("((x + y) * z);", "(x + y) * z;"),
                ("-(x + y);", "-(x + y);"),
                // function call
                ("(a + b)(2);", "(a + b)(2);"),
                // Index access
                ("(a + b)[2];", "(a + b)[2];"),
                ("(i < 7) && (6 >= -i);", "i < 7 && 6 >= -i;"),
                // Power test
                ("(-x) ** (-y);", "(-x) ** (-y);"),
                ("2 ** x';", "2 ** (x');"),
                ("(2 ** x)';", "(2 ** x)';"),
            ];

            for test_case in test_cases {
                test_paren(&test_case);
            }
        }

        #[test]
        fn lambda_ex() {
            let test_cases: Vec<TestCase> = vec![
                ("let x = 1 + (|i| i + 2);", "let x = 1 + (|i| i + 2);"),
                ("let x = 1 + (|i| i) + 2;", "let x = 1 + (|i| i) + 2;"),
                ("let x = 1 + (|i| (i + 2));", "let x = 1 + (|i| i + 2);"),
                ("let x = (1 + (|i| i)) + 2;", "let x = 1 + (|i| i) + 2;"),
                ("let x = (1 + (|i| (i + 2)));", "let x = 1 + (|i| i + 2);"),
                ("let x = (1 + (|i| i + 2));", "let x = 1 + (|i| i + 2);"),
                // Index access
                ("(|i| i)[j];", "(|i| i)[j];"),
            ];

            for test_case in test_cases {
                test_paren(&test_case);
            }
        }
        #[test]
        fn lambda_parentheses() {
            let test_cases: Vec<TestCase> = vec![
                // Nested lambdas
                ("|x| (|y| y) + x;", "|x| (|y| y) + x;"),
                ("|x| (|y| y + x);", "|x| (|y| y + x);"),
                ("|x| |y| y + x;", "|x| (|y| y + x);"),
                ("|x| |y| (y + x);", "|x| (|y| y + x);"),
                ("|x| (|y| |z| z + y) + x;", "|x| (|y| (|z| z + y)) + x;"),
                ("|x| |y| (|z| z) + y + x;", "|x| (|y| (|z| z) + y + x);"),
                ("|x| |y| |z| x + y + z;", "|x| (|y| (|z| x + y + z));"),
                // Lambda application
                ("1 + (|x| x)(2);", "1 + (|x| x)(2);"),
                // Lambda application with nested lambdas
                ("(|x| |y| y + x)(5);", "(|x| (|y| y + x))(5);"),
                ("|x| (|y| y)(x) + 1;", "|x| (|y| y)(x) + 1;"),
                ("|x| (|y| x + y)(5);", "|x| (|y| x + y)(5);"),
                ("|x| |y| y(x) + 1;", "|x| (|y| y(x) + 1);"),
                ("(|x| |y| x * y)(2)(3);", "(|x| (|y| x * y))(2)(3);"),
                ("(|x| x + 1)(|y| y * 2);", "(|x| x + 1)(|y| y * 2);"),
                (
                    "(|x| |y| x + y)(|z| z * 2);",
                    "(|x| (|y| x + y))(|z| z * 2);",
                ),
                // Binary operations between lambdas
                ("(|x| x) + (|y| y);", "(|x| x) + (|y| y);"),
                ("|x| x * (|y| y);", "|x| x * (|y| y);"),
                ("|x| x + 1 * (|y| y - 2);", "|x| x + 1 * (|y| y - 2);"),
                ("(|x| x + 1) - (|y| y) * -1;", "(|x| x + 1) - (|y| y) * -1;"),
                (
                    "|x| (|y| y)(x) + (|z| z)(x);",
                    "|x| (|y| y)(x) + (|z| z)(x);",
                ),
                ("|x| |y| y(x) + (|z| z)(x);", "|x| (|y| y(x) + (|z| z)(x));"),
                (
                    "|x| (|y| y(x) + (|z| z))(x);",
                    "|x| (|y| y(x) + (|z| z))(x);",
                ),
                (
                    "(|x| |y| x + y) + (|z| z * 2);",
                    "(|x| (|y| x + y)) + (|z| z * 2);",
                ),
            ];

            for test_case in test_cases {
                test_paren(&test_case);
            }
        }
        #[test]
        fn complex() {
            let test_cases: Vec<TestCase> = vec![
            // Don't change concise expression
            (
                "a | b * (c << d + e) & (f ^ g) = h * (i + g);",
                "a | b * (c << d + e) & (f ^ g) = h * (i + g);",
            ),
            // Remove extra parentheses
            (
                "(a | ((b * (c << (d + e))) & (f ^ g))) = (h * ((i + g)));",
                "a | b * (c << d + e) & (f ^ g) = h * (i + g);",
            ),
            (
                "instr_or $ [0, X, Y, Z] is (main_bin::latch * main_bin::sel[0]) $ [main_bin::operation_id, main_bin::A, main_bin::B, main_bin::C];",
                "instr_or $ [0, X, Y, Z] is main_bin::latch * main_bin::sel[0] $ [main_bin::operation_id, main_bin::A, main_bin::B, main_bin::C];",
            ),
            (
                "instr_or $ [0, X, Y, Z] is main_bin::latch * main_bin::sel[0] $ [main_bin::operation_id, main_bin::A, main_bin::B, main_bin::C];",
                "instr_or $ [0, X, Y, Z] is main_bin::latch * main_bin::sel[0] $ [main_bin::operation_id, main_bin::A, main_bin::B, main_bin::C];",
            ),
            (
                "pc' = (1 - first_step') * ((((instr__jump_to_operation * _operation_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1)));",
                "pc' = (1 - first_step') * (instr__jump_to_operation * _operation_id + instr__loop * pc + instr_return * 0 + (1 - (instr__jump_to_operation + instr__loop + instr_return)) * (pc + 1));",
            ),
            (
                "let root_of_unity_for_log_degree: int -> fe = |n| root_of_unity ** (2**(32 - n));",
                "let root_of_unity_for_log_degree: int -> fe = |n| root_of_unity ** (2 ** (32 - n));",
            ),
        ];

            for test_case in test_cases {
                test_paren(&test_case);
            }
        }

        #[test]
        fn index_access_parentheses() {
            let test_cases: Vec<TestCase> = vec![
                ("(x')(2);", "(x')(2);"),
                ("x[2](2);", "x[2](2);"),
                ("(x')[2];", "(x')[2];"),
                ("-x[2];", "-x[2];"),
                ("(-x)[2];", "(-x)[2];"),
                ("-(x[2]);", "-x[2];"),
                ("1 + x[2];", "1 + x[2];"),
                ("1 + x(2);", "1 + x(2);"),
            ];

            for test_case in test_cases {
                test_paren(&test_case);
            }
        }
    }
}
