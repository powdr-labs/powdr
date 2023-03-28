use std::fmt::{Display, Formatter, Result};

use crate::utils::quote;

use super::ast::*;

// TODO indentation

impl Display for PILFile {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for s in &self.0 {
            writeln!(f, "{s}")?;
        }
        Ok(())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::Include(_, path) => write!(f, "include {};", quote(path)),
            Statement::Namespace(_, name, poly_length) => {
                write!(f, "namespace {name}({poly_length});")
            }
            Statement::PolynomialDefinition(_, name, value) => {
                write!(f, "pol {name} = {value};")
            }
            Statement::PublicDeclaration(_, name, poly, index) => {
                write!(f, "public {name} = {poly}({index});")
            }
            Statement::PolynomialConstantDeclaration(_, names) => {
                write!(f, "pol constant {};", format_names(names))
            }
            Statement::PolynomialConstantDefinition(_, name, definition) => {
                write!(f, "pol constant {name}{definition};")
            }
            Statement::PolynomialCommitDeclaration(_, names, value) => {
                write!(
                    f,
                    "pol commit {}{};",
                    format_names(names),
                    value.as_ref().map(|v| format!("{v}")).unwrap_or_default()
                )
            }
            Statement::PolynomialIdentity(_, expression) => {
                if let Expression::BinaryOperation(left, BinaryOperator::Sub, right) = expression {
                    write!(f, "{left} = {right};")
                } else {
                    write!(f, "{expression} = 0;")
                }
            }
            Statement::PlookupIdentity(_, left, right) => write!(f, "{left} in {right};"),
            Statement::PermutationIdentity(_, left, right) => write!(f, "{left} is {right};"),
            Statement::ConnectIdentity(_, left, right) => write!(
                f,
                "{{ {} }} connect {{ {} }};",
                format_expressions(left),
                format_expressions(right)
            ),
            Statement::ConstantDefinition(_, name, value) => {
                write!(f, "constant {name} = {value};")
            }
            Statement::MacroDefinition(_, name, params, statements, expression) => {
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
            Statement::FunctionCall(_, name, args) => {
                write!(f, "{name}({});", format_expressions(args))
            }
        }
    }
}

fn format_names(names: &[PolynomialName]) -> String {
    names
        .iter()
        .map(|n| format!("{n}"))
        .collect::<Vec<_>>()
        .join(", ")
}

impl Display for ArrayExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ArrayExpression::Value(expressions) => {
                write!(f, "[{}]", format_expressions(expressions))
            }
            ArrayExpression::RepeatedValue(expressions) => {
                write!(f, "[{}]*", format_expressions(expressions))
            }
            ArrayExpression::Concat(left, right) => write!(f, "({} + {})", left, right),
        }
    }
}

impl Display for FunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FunctionDefinition::Mapping(params, body) => {
                write!(f, "({}) {{ {body} }}", params.join(", "))
            }
            FunctionDefinition::Array(array_expression) => {
                write!(f, " = {}", array_expression)
            }
            FunctionDefinition::Query(params, value) => {
                write!(f, "({}) query {value}", params.join(", "),)
            }
        }
    }
}

impl Display for SelectedExpressions {
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

fn format_expressions(expressions: &[Expression]) -> String {
    expressions
        .iter()
        .map(|e| format!("{e}"))
        .collect::<Vec<_>>()
        .join(", ")
}

impl Display for Expression {
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
            Expression::FunctionCall(fun, args) => write!(f, "{fun}({})", format_expressions(args)),
            Expression::FreeInput(input) => write!(f, "${{ {input} }}"),
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

impl Display for PolynomialReference {
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

#[cfg(test)]
mod test {
    use crate::parser;

    #[test]
    fn reparse() {
        let input = r#"
constant %N = 16;
namespace Fibonacci(%N);
constant %last_row = (%N - 1);
macro bool(X) { (X * (1 - X)) = 0; };
macro is_nonzero(X) { (X / X) };
macro is_zero(X) { (1 - is_nonzero(X)) };
macro is_equal(A, B) { is_zero((A - B)) };
macro is_one(X) { is_equal(X, 1) };
macro ite(C, A, B) { ((is_nonzero(C) * A) + (is_zero(C) * B)) };
macro one_hot(i, index) { ite(is_equal(i, index), 1, 0) };
pol constant ISLAST(i) { one_hot(i, %last_row) };
pol commit x, y;
macro constrain_equal_expr(A, B) { (A - B) };
macro force_equal_on_last_row(poly, value) { (ISLAST * constrain_equal_expr(poly, value)) = 0; };
force_equal_on_last_row(x', 1);
force_equal_on_last_row(y', 1);
macro on_regular_row(cond) { ((1 - ISLAST) * cond) = 0; };
on_regular_row(constrain_equal_expr(x', y));
on_regular_row(constrain_equal_expr(y', (x + y)));
public out = y(%last_row);"#;
        let printed = format!("{}", parser::parse(Some("input"), input).unwrap());
        assert_eq!(input.trim(), printed.trim());
    }

    #[test]
    fn reparse_witness_query() {
        let input = r#"pol commit wit(i) query (x(i), y(i));"#;
        let printed = format!("{}", parser::parse(Some("input"), input).unwrap());
        assert_eq!(input.trim(), printed.trim());
    }

    #[test]
    fn reparse_strings_and_tuples() {
        let input = r#"constant %N = ("abc", 3);"#;
        let printed = format!("{}", parser::parse(Some("input"), input).unwrap());
        assert_eq!(input.trim(), printed.trim());
    }
}
