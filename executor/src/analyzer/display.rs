use std::fmt::{Display, Formatter, Result};

use super::*;

impl Display for Identity {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.kind {
            IdentityKind::Polynomial => {
                let expression = self.left.selector.as_ref().unwrap();
                if let Expression::BinaryOperation(left, BinaryOperator::Sub, right) = expression {
                    write!(f, "{left} = {right};")
                } else {
                    write!(f, "{expression} = 0;")
                }
            }
            IdentityKind::Plookup => write!(f, "{} in {};", self.left, self.right),
            IdentityKind::Permutation => write!(f, "{} is {};", self.left, self.right),
            IdentityKind::Connect => write!(f, "{} connect {};", self.left, self.right),
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
            Expression::LocalVariableReference(index) => write!(f, "${index}"),
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

fn format_expressions(expressions: &[Expression]) -> String {
    expressions
        .iter()
        .map(|e| format!("{e}"))
        .collect::<Vec<_>>()
        .join(", ")
}

impl Display for PolynomialReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}{}",
            self.name,
            self.index
                .as_ref()
                .map(|s| format!("[{s}]"))
                .unwrap_or_default(),
            if self.next { "'" } else { "" }
        )
    }
}
