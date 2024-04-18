use core::panic;
use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

use powdr_ast::{
    analyzed::{Expression, PolynomialReference, Reference, RepeatedArray},
    parsed::{
        self, asm::SymbolPath, ArrayExpression, ArrayLiteral, IfExpression, LambdaExpression,
        LetStatementInsideBlock, MatchArm, NamespacedPolynomialReference, Pattern,
        SelectedExpressions, StatementInsideBlock, SymbolCategory,
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
            PExpression::LambdaExpression(lambda_expression) => {
                Expression::LambdaExpression(self.process_lambda_expression(lambda_expression))
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
                        let pattern = self.process_pattern(pattern);
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
    /// It also changes EnumPatterns consisting of a single identifier that does not resolve
    /// to anything into Variable patterns.
    fn process_pattern(&mut self, pattern: Pattern) -> Pattern {
        match pattern {
            Pattern::CatchAll | Pattern::Ellipsis | Pattern::Number(_) | Pattern::String(_) => {
                pattern
            }
            Pattern::Array(items) => {
                // If there is more than one Pattern::Ellipsis in items, it is an error
                if items.iter().filter(|p| *p == &Pattern::Ellipsis).count() > 1 {
                    panic!("Only one \"..\"-item allowed in array pattern");
                }
                Pattern::Array(self.process_pattern_vec(items))
            }
            Pattern::Tuple(items) => Pattern::Tuple(self.process_pattern_vec(items)),
            Pattern::Variable(name) => self.process_variable_pattern(name),
            Pattern::Enum(name, None) => {
                // The parser cannot distinguish between Enum and Variable patterns.
                // So if "name" is a single identifier that does not resolve to an enum variant,
                // it is a variable pattern.

                if let Some((resolved_name, category)) = self.driver.try_resolve_ref(&name) {
                    if category.compatible_with_request(SymbolCategory::TypeConstructor) {
                        self.process_enum_pattern(resolved_name, None)
                    } else if let Some(identifier) = name.try_to_identifier() {
                        // It's a single identifier that does not resolve to an enum variant.
                        self.process_variable_pattern(identifier.clone())
                    } else {
                        panic!("Expected enum variant but got {category}: {resolved_name}");
                    }
                } else if let Some(identifier) = name.try_to_identifier() {
                    // It's a single identifier that does not resolve to an enum variant.
                    self.process_variable_pattern(identifier.clone())
                } else {
                    panic!("Symbol not found: {name}");
                }
            }
            Pattern::Enum(name, fields) => {
                self.process_enum_pattern(self.driver.resolve_value_ref(&name), fields)
            }
        }
    }

    fn process_pattern_vec(&mut self, patterns: Vec<Pattern>) -> Vec<Pattern> {
        patterns
            .into_iter()
            .map(|p| self.process_pattern(p))
            .collect()
    }

    fn process_variable_pattern(&mut self, name: String) -> Pattern {
        let id = self.local_variable_counter;
        if self.local_variables.insert(name.clone(), id).is_some() {
            panic!("Variable already defined: {name}");
        }
        self.local_variable_counter += 1;
        Pattern::Variable(name)
    }

    fn process_enum_pattern(&mut self, name: String, fields: Option<Vec<Pattern>>) -> Pattern {
        Pattern::Enum(
            SymbolPath::from_str(&name).unwrap(),
            fields.map(|fields| {
                fields
                    .into_iter()
                    .map(|p| self.process_pattern(p))
                    .collect()
            }),
        )
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

    pub fn process_lambda_expression(
        &mut self,
        LambdaExpression { kind, params, body }: LambdaExpression,
    ) -> LambdaExpression<Expression> {
        let previous_local_vars = self.save_local_variables();

        let params = params
            .into_iter()
            .map(|p| self.process_pattern(p))
            .collect::<Vec<_>>();

        for param in &params {
            if !param.is_irrefutable() {
                panic!("Function parameters must be irrefutable, but {param} is refutable.");
            }
        }
        let body = Box::new(self.process_expression(*body));

        self.reset_local_variables(previous_local_vars);
        LambdaExpression { kind, params, body }
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
                    let value = value.map(|v| self.process_expression(v));
                    let pattern = self.process_pattern(pattern);
                    if value.is_none() && !matches!(pattern, Pattern::Variable(_)) {
                        panic!("Let statement without value requires a single variable, but got {pattern}.");
                    }
                    if !pattern.is_irrefutable() {
                        panic!("Let statement requires an irrefutable pattern, but {pattern} is refutable.");
                    }
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
