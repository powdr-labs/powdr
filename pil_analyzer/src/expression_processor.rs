use std::collections::HashMap;

use ast::{
    analyzed::{Expression, PolynomialReference, Reference, RepeatedArray},
    parsed::{
        self, ArrayExpression, ArrayLiteral, IfExpression, LambdaExpression, MatchArm,
        MatchPattern, NamespacedPolynomialReference, SelectedExpressions,
    },
};
use number::DegreeType;

/// The ExpressionProcessor turns parsed expressions into analyzed expressions.
/// Its main job is to resolve references:
/// It turns simple references into fully namespaced references and resolves local function variables.
/// It also evaluates expressions that are required to be compile-time constant.
pub struct ExpressionProcessor<R: ReferenceResolver> {
    resolver: R,
    local_variables: HashMap<String, u64>,
}

pub trait ReferenceResolver {
    /// Turns a reference to a name with an optional namespace to an absolute name.
    fn resolve(&self, namespace: &Option<String>, name: &str) -> String;
}

impl<R: ReferenceResolver> ExpressionProcessor<R> {
    pub fn new(resolver: R) -> Self {
        Self {
            resolver,
            local_variables: Default::default(),
        }
    }

    pub fn process_selected_expression<T>(
        &mut self,
        expr: SelectedExpressions<parsed::Expression<T>>,
    ) -> SelectedExpressions<Expression<T>> {
        SelectedExpressions {
            selector: expr.selector.map(|e| self.process_expression(e)),
            expressions: self.process_expressions(expr.expressions),
        }
    }

    pub fn process_array_expression<T>(
        &mut self,
        array_expression: ::ast::parsed::ArrayExpression<T>,
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

    pub fn process_expressions<T>(
        &mut self,
        exprs: Vec<parsed::Expression<T>>,
    ) -> Vec<Expression<T>> {
        exprs
            .into_iter()
            .map(|e| self.process_expression(e))
            .collect()
    }

    pub fn process_expression<T>(&mut self, expr: parsed::Expression<T>) -> Expression<T> {
        use parsed::Expression as PExpression;
        match expr {
            PExpression::Reference(poly) => Expression::Reference(self.process_reference(poly)),
            PExpression::PublicReference(name) => Expression::PublicReference(name),
            PExpression::Number(n) => Expression::Number(n),
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
        if reference.namespace.is_none() && self.local_variables.contains_key(&reference.name) {
            let id = self.local_variables[&reference.name];
            Reference::LocalVar(id, reference.name.to_string())
        } else {
            Reference::Poly(self.process_namespaced_polynomial_reference(reference))
        }
    }

    pub fn process_function<T>(
        &mut self,
        params: &[String],
        expression: ::ast::parsed::Expression<T>,
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
        poly: ::ast::parsed::NamespacedPolynomialReference,
    ) -> PolynomialReference {
        PolynomialReference {
            name: self.resolver.resolve(&poly.namespace, &poly.name),
            poly_id: None,
        }
    }
}
