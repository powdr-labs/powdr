use std::collections::HashMap;

use ast::{
    analyzed::{Expression, FunctionValueDefinition, Reference, Symbol, SymbolKind},
    evaluate_binary_operation, evaluate_unary_operation,
    parsed::{FunctionCall, MatchArm, MatchPattern},
};
use number::FieldElement;

/// Evaluates an expression to a single value.
pub fn evaluate_expression<T: FieldElement>(
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    intermediate_columns: &HashMap<String, (Symbol, Expression<T>)>,
    expression: &Expression<T>,
) -> Result<T, String> {
    Evaluator {
        definitions,
        intermediate_columns,
        function_cache: &Default::default(),
        variables: &[],
    }
    .evaluate(expression)
}

/// Returns a HashMap of all symbols that have a constant single value.
pub fn compute_constants<T: FieldElement>(
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    intermediate_columns: &HashMap<String, (Symbol, Expression<T>)>,
) -> HashMap<String, T> {
    definitions
        .iter()
        .filter_map(|(name, (symbol, value))| {
            (symbol.kind == SymbolKind::Constant()).then(|| {
                let Some(FunctionValueDefinition::Expression(value)) = value else {
                    panic!()
                };
                (
                    name.to_owned(),
                    evaluate_expression(definitions, intermediate_columns, value).unwrap(),
                )
            })
        })
        .collect()
}

pub struct Evaluator<'a, T> {
    pub definitions: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    pub intermediate_columns: &'a HashMap<String, (Symbol, Expression<T>)>,
    /// Contains full value tables of functions (columns) we already evaluated.
    pub function_cache: &'a HashMap<&'a str, Vec<T>>,
    pub variables: &'a [T],
}

impl<'a, T: FieldElement> Evaluator<'a, T> {
    pub fn evaluate(&self, expr: &Expression<T>) -> Result<T, String> {
        match expr {
            Expression::Reference(Reference::LocalVar(i, _name)) => Ok(self.variables[*i as usize]),
            Expression::Reference(Reference::Poly(poly)) => {
                if !poly.next && poly.index.is_none() {
                    if let Some((_, value)) = self.definitions.get(&poly.name.to_string()) {
                        match value {
                            Some(FunctionValueDefinition::Expression(value)) => {
                                self.evaluate(value)
                            }
                            _ => Err("Cannot evaluate function-typed values".to_string()),
                        }
                    } else if let Some((_, value)) =
                        self.intermediate_columns.get(&poly.name.to_string())
                    {
                        self.evaluate(value)
                    } else {
                        unreachable!()
                    }
                } else {
                    Err("Cannot evaluate arrays or next references.".to_string())
                }
            }
            Expression::PublicReference(r) => Err(format!("Cannot evaluate public reference: {r}")),
            Expression::Number(n) => Ok(*n),
            Expression::String(_) => Err("Cannot evaluate string literal.".to_string()),
            Expression::Tuple(_) => Err("Cannot evaluate tuple.".to_string()),
            Expression::ArrayLiteral(_) => Err("Cannot evaluate array literal.".to_string()),
            Expression::BinaryOperation(left, op, right) => Ok(evaluate_binary_operation(
                self.evaluate(left)?,
                *op,
                self.evaluate(right)?,
            )),
            Expression::UnaryOperation(op, expr) => {
                Ok(evaluate_unary_operation(*op, self.evaluate(expr)?))
            }
            Expression::LambdaExpression(_) => {
                Err("Cannot evaluate lambda expression.".to_string())
            }
            Expression::FunctionCall(FunctionCall { id, arguments }) => {
                let arg_values = arguments
                    .iter()
                    .map(|a| self.evaluate(a))
                    .collect::<Result<Vec<_>, _>>()?;
                assert!(arg_values.len() == 1);
                let values = &self.function_cache[id.as_str()];
                Ok(values[arg_values[0].to_degree() as usize % values.len()])
            }
            Expression::MatchExpression(scrutinee, arms) => {
                let v = self.evaluate(scrutinee);
                arms.iter()
                    .find_map(|MatchArm { pattern, value }| match pattern {
                        MatchPattern::Pattern(p) => {
                            (self.evaluate(p) == v).then(|| self.evaluate(value))
                        }
                        MatchPattern::CatchAll => Some(self.evaluate(value)),
                    })
                    .expect("No arm matched the value {v}")
            }
            Expression::FreeInput(_) => Err("Cannot evaluate free input.".to_string()),
        }
    }
}
