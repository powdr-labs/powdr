use core::panic;
use powdr_ast::{
    analyzed::{Expression, PolynomialReference, Reference},
    parsed::{
        self, asm::SymbolPath, types::Type, ArrayExpression, ArrayLiteral, BinaryOperation,
        BlockExpression, IfExpression, LambdaExpression, LetStatementInsideBlock, MatchArm,
        MatchExpression, NamedExpression, NamespacedPolynomialReference, Number, Pattern,
        SourceReference, StatementInsideBlock, StructExpression, SymbolCategory, UnaryOperation,
    },
};

use powdr_parser_util::{Error, SourceRef};
use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

use crate::{type_processor::TypeProcessor, AnalysisDriver};

/// The ExpressionProcessor turns parsed expressions into analyzed expressions.
///
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

    pub fn process_array_expression(
        &mut self,
        array_expression: ::powdr_ast::parsed::ArrayExpression,
    ) -> Result<ArrayExpression<Reference>, Error> {
        match array_expression {
            ArrayExpression::Value(expressions) => {
                let values = self.process_expressions(expressions)?;
                Ok(ArrayExpression::Value(values))
            }
            ArrayExpression::RepeatedValue(expressions) => Ok(ArrayExpression::RepeatedValue(
                self.process_expressions(expressions)?,
            )),
            ArrayExpression::Concat(left, right) => Ok(ArrayExpression::Concat(
                Box::new(self.process_array_expression(*left)?),
                Box::new(self.process_array_expression(*right)?),
            )),
        }
    }

    pub fn process_expressions(
        &mut self,
        exprs: Vec<parsed::Expression>,
    ) -> Result<Vec<Expression>, Error> {
        exprs
            .into_iter()
            .map(|e| self.process_expression(e))
            .collect::<Result<Vec<_>, _>>()
    }

    pub fn process_expression(&mut self, expr: parsed::Expression) -> Result<Expression, Error> {
        use parsed::Expression as PExpression;
        Ok(match expr {
            PExpression::Reference(src, poly) => {
                let reference = self
                    .process_reference(poly)
                    .map_err(|e| src.with_error(e))?;
                Expression::Reference(src, reference)
            }
            PExpression::Number(src, Number { value: n, type_: t }) => {
                Expression::Number(src, Number { value: n, type_: t })
            }
            PExpression::String(src, value) => Expression::String(src, value),
            PExpression::Tuple(src, items) => {
                Expression::Tuple(src, self.process_expressions(items)?)
            }
            PExpression::ArrayLiteral(src, ArrayLiteral { items }) => Expression::ArrayLiteral(
                src,
                ArrayLiteral {
                    items: self.process_expressions(items)?,
                },
            ),
            PExpression::LambdaExpression(src, lambda_expression) => {
                self.process_lambda_expression(src, lambda_expression)?
            }
            PExpression::BinaryOperation(
                src,
                BinaryOperation {
                    left: l,
                    op,
                    right: r,
                },
            ) => Expression::BinaryOperation(
                src,
                BinaryOperation {
                    left: Box::new(self.process_expression(*l)?),
                    op,
                    right: Box::new(self.process_expression(*r)?),
                },
            ),
            PExpression::UnaryOperation(src, UnaryOperation { op, expr: value }) => {
                Expression::UnaryOperation(
                    src,
                    UnaryOperation {
                        op,
                        expr: Box::new(self.process_expression(*value)?),
                    },
                )
            }
            PExpression::IndexAccess(src, index_access) => Expression::IndexAccess(
                src,
                parsed::IndexAccess {
                    array: Box::new(self.process_expression(*index_access.array)?),
                    index: Box::new(self.process_expression(*index_access.index)?),
                },
            ),
            PExpression::FunctionCall(src, c) => Expression::FunctionCall(
                src,
                parsed::FunctionCall {
                    function: Box::new(self.process_expression(*c.function)?),
                    arguments: self.process_expressions(c.arguments)?,
                },
            ),
            PExpression::MatchExpression(src, MatchExpression { scrutinee, arms }) => {
                Expression::MatchExpression(
                    src,
                    MatchExpression {
                        scrutinee: Box::new(self.process_expression(*scrutinee)?),
                        arms: arms
                            .into_iter()
                            .map(|MatchArm { pattern, value }| {
                                let vars = self.save_local_variables();
                                let pattern = self.process_pattern(pattern);
                                let value = self.process_expression(value);
                                self.reset_local_variables(vars);

                                Ok(MatchArm {
                                    pattern: pattern?,
                                    value: value?,
                                })
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                    },
                )
            }
            PExpression::IfExpression(
                src,
                IfExpression {
                    condition,
                    body,
                    else_body,
                },
            ) => Expression::IfExpression(
                src,
                IfExpression {
                    condition: Box::new(self.process_expression(*condition)?),
                    body: Box::new(self.process_expression(*body)?),
                    else_body: Box::new(self.process_expression(*else_body)?),
                },
            ),
            PExpression::BlockExpression(src, BlockExpression { statements, expr }) => {
                self.process_block_expression(statements, expr, src)?
            }
            PExpression::FreeInput(_, _) => panic!(),
            PExpression::StructExpression(src, StructExpression { name, fields }) => {
                let type_args = name
                    .type_args
                    .map(|args| args.into_iter().map(|t| self.process_type(t)).collect());

                let name = self
                    .driver
                    .resolve_ref(&name.path, SymbolCategory::Struct)
                    .map_err(|msg| src.with_error(msg))?;

                Expression::StructExpression(
                    src,
                    StructExpression {
                        name: Reference::Poly(PolynomialReference { name, type_args }),
                        fields: fields
                            .into_iter()
                            .map(
                                |named_expr| -> Result<NamedExpression<Box<Expression>>, Error> {
                                    Ok(NamedExpression {
                                        name: named_expr.name,
                                        body: Box::new(self.process_expression(*named_expr.body)?),
                                    })
                                },
                            )
                            .collect::<Result<Vec<_>, _>>()?,
                    },
                )
            }
        })
    }

    /// Processes a pattern, registering all variables bound in there.
    /// It also changes EnumPatterns consisting of a single identifier that does not resolve
    /// to anything into Variable patterns.
    fn process_pattern(&mut self, pattern: Pattern) -> Result<Pattern, Error> {
        match pattern {
            Pattern::CatchAll(_)
            | Pattern::Ellipsis(_)
            | Pattern::Number(_, _)
            | Pattern::String(_, _) => Ok(pattern),
            Pattern::Array(source_ref, items) => {
                // If there is more than one Pattern::Ellipsis in items, it is an error
                if items
                    .iter()
                    .filter(|p| matches!(p, Pattern::Ellipsis(_)))
                    .count()
                    > 1
                {
                    return Err(source_ref
                        .with_error("Only one \"..\"-item allowed in array pattern".to_string()));
                }
                Ok(Pattern::Array(source_ref, self.process_pattern_vec(items)?))
            }
            Pattern::Tuple(source_ref, items) => {
                Ok(Pattern::Tuple(source_ref, self.process_pattern_vec(items)?))
            }
            Pattern::Variable(source_ref, name) => self.process_variable_pattern(source_ref, name),
            Pattern::Enum(source_ref, name, None) => {
                // The parser cannot distinguish between Enum and Variable patterns.
                // So if "name" is a single identifier that does not resolve to an enum variant,
                // it is a variable pattern.

                if let Some((resolved_name, category)) = self.driver.try_resolve_ref(&name) {
                    if category.compatible_with_request(SymbolCategory::TypeConstructor) {
                        self.process_enum_pattern(source_ref, resolved_name, None)
                    } else if let Some(identifier) = name.try_to_identifier() {
                        // It's a single identifier that does not resolve to an enum variant.
                        self.process_variable_pattern(source_ref, identifier.clone())
                    } else {
                        return Err(source_ref.with_error(format!(
                            "Expected enum variant but got {category}: {resolved_name}"
                        )));
                    }
                } else if let Some(identifier) = name.try_to_identifier() {
                    // It's a single identifier that does not resolve to an enum variant.
                    self.process_variable_pattern(source_ref, identifier.clone())
                } else {
                    return Err(source_ref.with_error(format!("Symbol not found: {name}")));
                }
            }
            Pattern::Enum(source_ref, name, fields) => {
                let name = self
                    .driver
                    .resolve_value_ref(&name)
                    .map_err(|msg| source_ref.with_error(msg))?;
                self.process_enum_pattern(source_ref, name, fields)
            }
        }
    }

    fn process_pattern_vec(&mut self, patterns: Vec<Pattern>) -> Result<Vec<Pattern>, Error> {
        patterns
            .into_iter()
            .map(|p| self.process_pattern(p))
            .collect()
    }

    fn process_variable_pattern(
        &mut self,
        source_ref: SourceRef,
        name: String,
    ) -> Result<Pattern, Error> {
        let id = self.local_variable_counter;
        if self.local_variables.insert(name.clone(), id).is_some() {
            return Err(source_ref.with_error(format!("Variable already defined: {name}")));
        }
        self.local_variable_counter += 1;
        Ok(Pattern::Variable(source_ref, name))
    }

    fn process_enum_pattern(
        &mut self,
        source_ref: SourceRef,
        name: String,
        fields: Option<Vec<Pattern>>,
    ) -> Result<Pattern, Error> {
        let fields = fields
            .map(|fields_vec| {
                fields_vec
                    .into_iter()
                    .map(|p| self.process_pattern(p))
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?;

        Ok(Pattern::Enum(
            source_ref,
            SymbolPath::from_str(&name).unwrap(),
            fields,
        ))
    }

    fn process_reference(
        &mut self,
        reference: NamespacedPolynomialReference,
    ) -> Result<Reference, String> {
        match reference.try_to_identifier() {
            Some(name) if self.local_variables.contains_key(name) => {
                let id = self.local_variables[name];
                Ok(Reference::LocalVar(id, name.to_string()))
            }
            _ => Ok(Reference::Poly(
                self.process_namespaced_polynomial_reference(reference)?,
            )),
        }
    }

    pub fn process_lambda_expression(
        &mut self,
        source_ref: SourceRef,
        LambdaExpression {
            kind, params, body, ..
        }: LambdaExpression,
    ) -> Result<Expression, Error> {
        let previous_local_vars = self.save_local_variables();

        let params = params
            .into_iter()
            .map(|p| self.process_pattern(p))
            .collect::<Result<Vec<_>, Error>>()?;

        for param in &params {
            if !param.is_irrefutable() {
                return Err(source_ref.with_error(format!(
                    "Function parameters must be irrefutable, but {param} is refutable."
                )));
            }
        }
        let body = Box::new(self.process_expression(*body)?);

        self.reset_local_variables(previous_local_vars);
        Ok(Expression::LambdaExpression(
            source_ref,
            LambdaExpression {
                kind,
                params,
                body,
                param_types: vec![],
            },
        ))
    }

    fn process_block_expression(
        &mut self,
        statements: Vec<StatementInsideBlock>,
        expr: Option<Box<::powdr_ast::parsed::Expression>>,
        src: SourceRef,
    ) -> Result<Expression, Error> {
        let vars = self.save_local_variables();

        let processed_statements = statements
            .into_iter()
            .map(|statement| {
                match statement {
                    StatementInsideBlock::LetStatement(LetStatementInsideBlock {
                        pattern,
                        ty,
                        value,
                    }) => {
                        let value = match value {
                            Some(v) => Some(self.process_expression(v)?),
                            None => None,
                        };
                        let pattern = self.process_pattern(pattern)?;
                        let ty = ty.map(|ty| self.process_number_type(ty));

                        if value.is_none() && !matches!(pattern, Pattern::Variable(_, _)) {
                            return Err(src.with_error(format!(
                                "Let statement without value requires a single variable, but got {pattern}."
                            )));
                        }
                        if !pattern.is_irrefutable() {
                            return Err(src.with_error(format!(
                                "Let statement requires an irrefutable pattern, but {pattern} is refutable."
                            )));
                        }
                        Ok(StatementInsideBlock::LetStatement(LetStatementInsideBlock {
                            pattern,
                            ty,
                            value,
                        }))
                    }
                    StatementInsideBlock::Expression(expr) => {
                        Ok(StatementInsideBlock::Expression(self.process_expression(expr)?))
                    }
                }
            })
            .collect::<Result<Vec<_>, Error>>()?;

        let processed_expr = match expr {
            Some(expr) => {
                let src = expr.source_reference().clone();
                match self.process_expression(*expr) {
                    Ok(e) => Some(Box::new(e)),
                    Err(e) => return Err(src.with_error(e.to_string())),
                }
            }
            None => None,
        };
        self.reset_local_variables(vars);
        Ok(Expression::BlockExpression(
            src,
            BlockExpression {
                statements: processed_statements,
                expr: processed_expr,
            },
        ))
    }

    pub fn process_namespaced_polynomial_reference(
        &mut self,
        reference: NamespacedPolynomialReference,
    ) -> Result<PolynomialReference, String> {
        let type_args = reference
            .type_args
            .map(|args| args.into_iter().map(|t| self.process_type(t)).collect());

        let name = self.driver.resolve_value_ref(&reference.path)?;
        Ok(PolynomialReference { name, type_args })
    }

    fn process_type(&self, ty: Type<parsed::Expression>) -> Type<u64> {
        TypeProcessor::new(self.driver, self.type_vars).process_type(ty)
    }

    fn process_number_type(&self, ty: Type<u64>) -> Type<u64> {
        TypeProcessor::new(self.driver, self.type_vars).process_number_type(ty)
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
