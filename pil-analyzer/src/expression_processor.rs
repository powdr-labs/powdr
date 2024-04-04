use core::panic;
use std::collections::{HashMap, HashSet};

use powdr_ast::{
    analyzed::{Expression, PolynomialReference, Reference, RepeatedArray},
    parsed::{
        self, ArrayExpression, ArrayLiteral, IfExpression, LambdaExpression,
        LetStatementInsideBlock, MatchArm, NamespacedPolynomialReference, Pattern,
        SelectedExpressions, StatementInsideBlock,
    },
};
use powdr_number::DegreeType;

use crate::{type_processor::TypeProcessor, AnalysisDriver};

/// The ExpressionProcessor turns parsed expressions into analyzed expressions.
/// Its main job is to resolve references:
/// It turns simple references into fully namespaced references and resolves local function variables.
pub struct ExpressionProcessor<'a, D: AnalysisDriver> {
    driver: D,
    type_vars: &'a HashSet<&'a String>,
    local_variables: HashMap<String, u64>,
    local_variable_counter: u64,
}

impl<'a, D: AnalysisDriver> ExpressionProcessor<'a, D> {
    pub fn new(driver: D, type_vars: &'a HashSet<&'a String>) -> Self {
        Self {
            driver,
            type_vars,
            local_variables: Default::default(),
            local_variable_counter: 0,
        }
    }

    pub fn process_selected_expressions(
        &mut self,
        expr: SelectedExpressions<parsed::Expression>,
    ) -> SelectedExpressions<Expression> {
        SelectedExpressions {
            selector: expr.selector.map(|e| self.process_expression(e)),
            expressions: self.process_expressions(expr.expressions),
        }
    }

    pub fn process_array_expression(
        &mut self,
        array_expression: ::powdr_ast::parsed::ArrayExpression,
        size: DegreeType,
    ) -> Vec<RepeatedArray> {
        match array_expression {
            ArrayExpression::Value(expressions) => {
                let values = self.process_expressions(expressions);
                let size = values.len() as DegreeType;
                vec![RepeatedArray::new(values, size)]
            }
            ArrayExpression::RepeatedValue(expressions) => {
                if size == 0 {
                    vec![]
                } else {
                    vec![RepeatedArray::new(
                        self.process_expressions(expressions),
                        size,
                    )]
                }
            }
            ArrayExpression::Concat(left, right) => self
                .process_array_expression(*left, size)
                .into_iter()
                .chain(self.process_array_expression(*right, size))
                .collect(),
        }
    }

    pub fn process_expressions(&mut self, exprs: Vec<parsed::Expression>) -> Vec<Expression> {
        exprs
            .into_iter()
            .map(|e| self.process_expression(e))
            .collect()
    }

    pub fn process_expression(&mut self, expr: parsed::Expression) -> Expression {
        use parsed::Expression as PExpression;
        match expr {
            PExpression::Reference(poly) => Expression::Reference(self.process_reference(poly)),
            PExpression::PublicReference(name) => Expression::PublicReference(name),
            PExpression::Number(n, t) => Expression::Number(n, t),
            PExpression::String(value) => Expression::String(value),
            PExpression::Tuple(items) => Expression::Tuple(self.process_expressions(items)),
            PExpression::ArrayLiteral(ArrayLiteral { items }) => {
                Expression::ArrayLiteral(ArrayLiteral {
                    items: self.process_expressions(items),
                })
            }
            PExpression::LambdaExpression(LambdaExpression { kind, params, body }) => {
                let body = Box::new(self.process_function(&params, *body));
                Expression::LambdaExpression(LambdaExpression { kind, params, body })
            }
            PExpression::BinaryOperation(left, op, right) => Expression::BinaryOperation(
                Box::new(self.process_expression(*left)),
                op,
                Box::new(self.process_expression(*right)),
            ),
            PExpression::UnaryOperation(op, value) => {
                Expression::UnaryOperation(op, Box::new(self.process_expression(*value)))
            }
            PExpression::IndexAccess(index_access) => {
                Expression::IndexAccess(parsed::IndexAccess {
                    array: Box::new(self.process_expression(*index_access.array)),
                    index: Box::new(self.process_expression(*index_access.index)),
                })
            }
            PExpression::FunctionCall(c) => Expression::FunctionCall(parsed::FunctionCall {
                function: Box::new(self.process_expression(*c.function)),
                arguments: self.process_expressions(c.arguments),
            }),
            PExpression::MatchExpression(scrutinee, arms) => Expression::MatchExpression(
                Box::new(self.process_expression(*scrutinee)),
                arms.into_iter()
                    .map(|MatchArm { pattern, value }| {
                        let vars = self.save_local_variables();
                        self.process_pattern(&pattern);
                        let value = self.process_expression(value);
                        self.reset_local_variables(vars);
                        MatchArm { pattern, value }
                    })
                    .collect(),
            ),
            PExpression::IfExpression(IfExpression {
                condition,
                body,
                else_body,
            }) => Expression::IfExpression(IfExpression {
                condition: Box::new(self.process_expression(*condition)),
                body: Box::new(self.process_expression(*body)),
                else_body: Box::new(self.process_expression(*else_body)),
            }),
            PExpression::BlockExpression(statements, expr) => {
                self.process_block_expression(statements, *expr)
            }
            PExpression::FreeInput(_) => panic!(),
        }
    }

    /// Processes a pattern, registering all variables bound in there.
    fn process_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::CatchAll | Pattern::Ellipsis | Pattern::Number(_) | Pattern::String(_) => {}
            Pattern::Tuple(items) | Pattern::Array(items) => {
                if matches!(pattern, Pattern::Array(_)) {
                    // If there is more than one Pattern::Ellipsis in items, it is an error
                    if items.iter().filter(|p| *p == &Pattern::Ellipsis).count() > 1 {
                        panic!("Only one \"..\"-item allowed in array pattern");
                    }
                }
                items.iter().for_each(|p| self.process_pattern(p));
            }
            Pattern::Variable(name) => {
                let id = self.local_variable_counter;
                if self.local_variables.insert(name.clone(), id).is_some() {
                    panic!("Variable already defined: {name}");
                }
                self.local_variable_counter += 1;
            }
        }
    }

    fn process_reference(&mut self, reference: NamespacedPolynomialReference) -> Reference {
        match reference.try_to_identifier() {
            Some(name) if self.local_variables.contains_key(name) => {
                let id = self.local_variables[name];
                Reference::LocalVar(id, name.to_string())
            }
            _ => Reference::Poly(self.process_namespaced_polynomial_reference(reference)),
        }
    }

    pub fn process_function(
        &mut self,
        params: &[Pattern],
        expression: ::powdr_ast::parsed::Expression,
    ) -> Expression {
        let previous_local_vars = self.save_local_variables();

        for param in params {
            if !param.is_irrefutable() {
                panic!("Function parameters must be irrefutable, but {param} is refutable.");
            }
            self.process_pattern(param);
        }
        let processed_value = self.process_expression(expression);

        self.reset_local_variables(previous_local_vars);
        processed_value
    }

    fn process_block_expression(
        &mut self,
        statements: Vec<StatementInsideBlock>,
        expr: ::powdr_ast::parsed::Expression,
    ) -> Expression {
        let vars = self.save_local_variables();

        let processed_statements = statements
            .into_iter()
            .map(|statement| match statement {
                StatementInsideBlock::LetStatement(LetStatementInsideBlock { pattern, value }) => {
                    if value.is_none() && !matches!(pattern, Pattern::Variable(_)) {
                        panic!("Let statement without value requires a single variable, but got {pattern}.");
                    }
                    if !pattern.is_irrefutable() {
                        panic!("Let statement requires an irrefutable pattern, but {pattern} is refutable.");
                    }
                    let value = value.map(|v| self.process_expression(v));
                    self.process_pattern(&pattern);
                    StatementInsideBlock::LetStatement(LetStatementInsideBlock { pattern, value })
                }
                StatementInsideBlock::Expression(expr) => {
                    StatementInsideBlock::Expression(self.process_expression(expr))
                }
            })
            .collect::<Vec<_>>();

        let processed_expr = self.process_expression(expr);
        self.reset_local_variables(vars);
        Expression::BlockExpression(processed_statements, Box::new(processed_expr))
    }

    pub fn process_namespaced_polynomial_reference(
        &mut self,
        reference: NamespacedPolynomialReference,
    ) -> PolynomialReference {
        let type_processor = TypeProcessor::new(self.driver, self.type_vars);
        let type_args = reference.type_args.map(|args| {
            args.into_iter()
                .map(|t| type_processor.process_type(t))
                .collect()
        });
        PolynomialReference {
            name: self.driver.resolve_value_ref(&reference.path),
            poly_id: None,
            type_args,
        }
    }

    fn save_local_variables(&self) -> LocalVariableState {
        LocalVariableState {
            local_variables: self.local_variables.clone(),
            local_variable_counter: self.local_variable_counter,
        }
    }

    fn reset_local_variables(&mut self, state: LocalVariableState) {
        self.local_variables = state.local_variables;
        self.local_variable_counter = state.local_variable_counter;
    }
}

struct LocalVariableState {
    pub local_variables: HashMap<String, u64>,
    pub local_variable_counter: u64,
}
