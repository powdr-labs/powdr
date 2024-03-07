use std::collections::HashMap;

use powdr_ast::{
    analyzed::{Expression, PolynomialReference, Reference, RepeatedArray},
    parsed::{
        self, asm::SymbolPath, ArrayExpression, ArrayLiteral, IfExpression, LambdaExpression,
        LetStatementInsideBlock, MatchArm, MatchPattern, NamespacedPolynomialReference,
        SelectedExpressions,
    },
};
use powdr_number::DegreeType;

use crate::AnalysisDriver;

/// The ExpressionProcessor turns parsed expressions into analyzed expressions.
/// Its main job is to resolve references:
/// It turns simple references into fully namespaced references and resolves local function variables.
pub struct ExpressionProcessor<D: AnalysisDriver> {
    driver: D,
    local_variables: HashMap<String, u64>,
    local_variable_counter: u64,
}

impl<D: AnalysisDriver> ExpressionProcessor<D> {
    pub fn new(driver: D) -> Self {
        Self {
            driver,
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
            PExpression::LambdaExpression(LambdaExpression { params, body }) => {
                let body = Box::new(self.process_function(&params, *body));
                Expression::LambdaExpression(LambdaExpression { params, body })
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
                    .map(|MatchArm { pattern, value }| MatchArm {
                        pattern: match pattern {
                            MatchPattern::CatchAll => MatchPattern::CatchAll,
                            MatchPattern::Pattern(e) => {
                                MatchPattern::Pattern(self.process_expression(e))
                            }
                        },
                        value: self.process_expression(value),
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

    fn process_reference(&mut self, reference: NamespacedPolynomialReference) -> Reference {
        match reference.try_to_identifier() {
            Some(name) if self.local_variables.contains_key(name) => {
                let id = self.local_variables[name];
                Reference::LocalVar(id, name.to_string())
            }
            _ => Reference::Poly(self.process_namespaced_polynomial_reference(&reference.path)),
        }
    }

    pub fn process_function(
        &mut self,
        params: &[String],
        expression: ::powdr_ast::parsed::Expression,
    ) -> Expression {
        let previous_local_vars = self.local_variables.clone();

        // Add the new local variables, potentially overwriting existing variables.
        self.local_variables.extend(
            params
                .iter()
                .zip(self.local_variable_counter..)
                .map(|(p, i)| (p.clone(), i)),
        );
        self.local_variable_counter += params.len() as u64;
        let processed_value = self.process_expression(expression);
        // Reset the local variable mapping.
        self.local_variables = previous_local_vars;
        self.local_variable_counter -= params.len() as u64;
        processed_value
    }

    fn process_block_expression(
        &mut self,
        statements: Vec<LetStatementInsideBlock>,
        expr: ::powdr_ast::parsed::Expression,
    ) -> Expression {
        let previous_local_vars = self.local_variables.clone();

        let processed_statements = statements
            .into_iter()
            .map(|LetStatementInsideBlock { name, value }| {
                let value = value.map(|v| self.process_expression(v));
                let id = self.local_variable_counter;
                self.local_variables.insert(name.clone(), id);
                self.local_variable_counter += 1;
                LetStatementInsideBlock { name, value }
            })
            .collect::<Vec<_>>();

        let processed_expr = self.process_expression(expr);
        self.local_variables = previous_local_vars;
        self.local_variable_counter -= processed_statements.len() as u64;
        Expression::BlockExpression(processed_statements, Box::new(processed_expr))
    }

    pub fn process_namespaced_polynomial_reference(
        &mut self,
        path: &SymbolPath,
    ) -> PolynomialReference {
        PolynomialReference {
            name: self.driver.resolve_value_ref(path),
            poly_id: None,
            // These will be filled by the type checker.
            // TODO at some point we should support the turbofish operator
            // in the parser.
            generic_args: Default::default(),
        }
    }
}
