use std::{collections::HashMap, marker::PhantomData};

use powdr_ast::{
    analyzed::{Expression, PolynomialReference, Reference, RepeatedArray},
    parsed::{
        self, asm::SymbolPath, ArrayExpression, ArrayLiteral, IfExpression, LambdaExpression,
        MatchArm, MatchPattern, NamespacedPolynomialReference, SelectedExpressions,
    },
};
use powdr_number::DegreeType;

use crate::AnalysisDriver;

/// The ExpressionProcessor turns parsed expressions into analyzed expressions.
/// Its main job is to resolve references:
/// It turns simple references into fully namespaced references and resolves local function variables.
pub struct ExpressionProcessor<T, D: AnalysisDriver<T>> {
    driver: D,
    local_variables: HashMap<String, u64>,
    _phantom: PhantomData<T>,
}

impl<T, D: AnalysisDriver<T>> ExpressionProcessor<T, D> {
    pub fn new(driver: D) -> Self {
        Self {
            driver,
            local_variables: Default::default(),
            _phantom: PhantomData,
        }
    }

    pub fn process_selected_expressions(
        &mut self,
        expr: SelectedExpressions<parsed::Expression<T>>,
    ) -> SelectedExpressions<Expression<T>> {
        SelectedExpressions {
            selector: expr.selector.map(|e| self.process_expression(e)),
            expressions: self.process_expressions(expr.expressions),
        }
    }

    pub fn process_array_expression(
        &mut self,
        array_expression: ::powdr_ast::parsed::ArrayExpression<T>,
        size: DegreeType,
    ) -> Vec<RepeatedArray<T>> {
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

    pub fn process_expressions(&mut self, exprs: Vec<parsed::Expression<T>>) -> Vec<Expression<T>> {
        exprs
            .into_iter()
            .map(|e| self.process_expression(e))
            .collect()
    }

    pub fn process_expression(&mut self, expr: parsed::Expression<T>) -> Expression<T> {
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
        expression: ::powdr_ast::parsed::Expression<T>,
    ) -> Expression<T> {
        let previous_local_vars = std::mem::take(&mut self.local_variables);

        assert!(self.local_variables.is_empty());
        self.local_variables = params
            .iter()
            .enumerate()
            .map(|(i, p)| (p.clone(), i as u64))
            .collect();
        // Re-add the outer local variables if we do not overwrite them
        // and increase their index by the number of parameters.
        for (name, index) in &previous_local_vars {
            self.local_variables
                .entry(name.clone())
                .or_insert(index + params.len() as u64);
        }
        let processed_value = self.process_expression(expression);
        self.local_variables = previous_local_vars;
        processed_value
    }

    pub fn process_namespaced_polynomial_reference(
        &mut self,
        path: &SymbolPath,
    ) -> PolynomialReference {
        PolynomialReference {
            name: self.driver.resolve_ref(path),
            poly_id: None,
            // These will be filled by the type checker.
            // TODO at some point we should support the turbofish operator
            // in the parser.
            generic_args: Default::default(),
        }
    }
}
