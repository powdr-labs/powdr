use std::collections::{BTreeSet, HashMap};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{Expression, PolynomialReference, Reference},
    parsed::{
        display::format_type_scheme_around_name,
        types::{ArrayType, FunctionType, TupleType, Type, TypeBounds, TypeScheme},
        visitor::ExpressionVisitable,
        ArrayLiteral, BinaryOperation, BlockExpression, FunctionCall, FunctionKind, IndexAccess,
        LambdaExpression, LetStatementInsideBlock, MatchArm, MatchExpression, Number, Pattern,
        SourceReference, StatementInsideBlock, UnaryOperation,
    },
};
use powdr_parser_util::{Error, SourceRef};

use crate::{
    call_graph::sort_called_first,
    type_builtins::{
        binary_operator_scheme, builtin_schemes, constr_function_statement_type,
        type_for_reference, unary_operator_scheme,
    },
    type_unifier::Unifier,
};

/// Infers types on all definitions and checks type-correctness for isolated
/// expressions (from identities and arrays) where the expected type is given.
/// The parameter `statement_type` is the expected type for expressions at statement level.
/// Sets the generic arguments for references and the literal types in all expressions.
/// Returns the types for symbols without explicit type.
pub fn infer_types(
    definitions: HashMap<String, (Option<TypeScheme>, Option<&mut Expression>)>,
    expressions: &mut [(&mut Expression, ExpectedType)],
) -> Result<Vec<(String, Type)>, Vec<Error>> {
    TypeChecker::new().infer_types(definitions, expressions)
}

/// A type to expect and a flag that says if arrays of that type are also fine.
#[derive(Clone)]
pub struct ExpectedType {
    pub ty: Type,
    /// If true, arrays of `ty` are also allowed.
    pub allow_array: bool,
    /// If true, the empty tuple is also allowed.
    pub allow_empty: bool,
}

impl From<Type> for ExpectedType {
    fn from(ty: Type) -> Self {
        ExpectedType {
            ty,
            allow_array: false,
            allow_empty: false,
        }
    }
}

struct TypeChecker {
    /// Types for local variables, might contain type variables.
    local_var_types: Vec<Type>,
    /// Declared types for all symbols and their source references.
    /// Contains the unmodified type scheme for symbols with generic types and newly
    /// created type variables for symbols without declared type.
    declared_types: HashMap<String, (SourceRef, TypeScheme)>,
    /// Current mapping of declared type vars to type. Reset before checking each definition.
    declared_type_vars: HashMap<String, Type>,
    unifier: Unifier,
    /// Keeps track of the kind of lambda we are currently type-checking.
    lambda_kind: FunctionKind,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            local_var_types: Default::default(),
            declared_types: Default::default(),
            declared_type_vars: Default::default(),
            unifier: Default::default(),
            lambda_kind: FunctionKind::Constr,
        }
    }

    /// Infers and checks types for all provided definitions and expressions and
    /// returns the types for symbols without explicit type.
    pub fn infer_types(
        mut self,
        mut definitions: HashMap<String, (Option<TypeScheme>, Option<&mut Expression>)>,
        expressions: &mut [(&mut Expression, ExpectedType)],
    ) -> Result<Vec<(String, Type)>, Vec<Error>> {
        let type_var_mapping = self
            .infer_types_inner(&mut definitions, expressions)
            .map_err(|e| vec![e])?;
        self.update_type_args(&mut definitions, expressions, &type_var_mapping)?;
        Ok(definitions
            .into_iter()
            .filter(|(_, (ty, _))| ty.is_none())
            .map(|(name, _)| {
                let (_, mut scheme) = self.declared_types.remove(&name).unwrap();
                assert!(scheme.vars.is_empty());
                self.substitute(&mut scheme.ty);
                assert!(scheme.ty.is_concrete_type());
                (name, scheme.ty)
            })
            .collect())
    }

    /// Returns, for each name declared with a type scheme, a mapping from
    /// the type variables used by the type checker to those used in the declaration.
    fn infer_types_inner(
        &mut self,
        definitions: &mut HashMap<String, (Option<TypeScheme>, Option<&mut Expression>)>,
        expressions: &mut [(&mut Expression, ExpectedType)],
    ) -> Result<HashMap<String, HashMap<String, Type>>, Error> {
        // TODO in order to fix type inference on recursive functions, we need to:
        // - collect all groups of functions that call each other recursively
        // - analyze each such group in an environment, where their type schemes
        //   are instantiated once at the start and not anymore for the symbol lookup.

        // Sort the names such that called names occur first.
        let names = sort_called_first(
            definitions
                .iter()
                .map(|(n, (_, v))| (n.as_str(), v.as_deref())),
        );

        self.setup_declared_types(definitions);

        // These are the inferred types for symbols that are declared
        // as type schemes. They are compared to the declared types
        // at the end.
        let mut inferred_types: HashMap<String, Type> = Default::default();

        // Now go through all symbols and derive types for the expressions.
        // While analyzing a symbol, we ignore its declared type (unless the
        // symbol is referenced). Unifying the declared type with the inferred
        // type is done at the end.
        for name in names {
            // Ignore builtins (removed from definitions) and definitions without value.
            let Some((_, Some(value))) = definitions.get_mut(&name) else {
                continue;
            };

            let (_, declared_type) = self.declared_types[&name].clone();
            if declared_type.vars.is_empty() {
                self.declared_type_vars.clear();
                self.process_concrete_symbol(declared_type.ty.clone(), value)?;
            } else {
                self.declared_type_vars = declared_type
                    .vars
                    .vars()
                    .map(|v| (v.clone(), self.unifier.new_type_var()))
                    .collect();
                self.infer_type_of_expression(value).map(|ty| {
                    inferred_types.insert(name.to_string(), ty);
                })?;
            }
        }
        self.declared_type_vars.clear();

        self.check_expressions(expressions)?;

        // From this point on, the substitutions are fixed.

        // Now we check for all symbols that are not declared as a type scheme that they
        // can resolve to a concrete type.
        for (name, (source_ref, declared_type)) in &self.declared_types {
            if declared_type.vars.is_empty() {
                // It is not a type scheme, see if we were able to derive a concrete type.
                let inferred = self.type_into_substituted(declared_type.ty.clone());
                if !inferred.is_concrete_type() {
                    let inferred_scheme = self.to_type_scheme(inferred);
                    return Err(source_ref.with_error(
                        format!(
                            "Could not derive a concrete type for symbol {name}.\nInferred type scheme: {}\n",
                            format_type_scheme_around_name(
                                name,
                                &Some(inferred_scheme),
                            )
                        )));
                }
            }
        }

        // We check type schemes last, because only at this point do we know
        // that other types that should be concrete do not occur as type variables in the
        // inferred type scheme any more.
        // This also computes and returns a mapping from the internal names of the type vars
        // in the type scheme to the type vars of the declarations.
        self.verify_type_schemes(inferred_types)
    }

    /// Fills self.declared_types and checks that declared builtins have the correct type.
    fn setup_declared_types(
        &mut self,
        definitions: &mut HashMap<String, (Option<TypeScheme>, Option<&mut Expression>)>,
    ) {
        // Add types from declarations. Type schemes are added without instantiating.
        self.declared_types = definitions
            .iter()
            .map(|(name, (type_scheme, value))| {
                let source_ref = value
                    .as_ref()
                    .map(|v| v.source_reference())
                    .cloned()
                    .unwrap_or_default();
                // Check if it is a builtin symbol.
                let ty = match (builtin_schemes().get(name), type_scheme) {
                    (Some(builtin), declared) => {
                        if let Some(declared) = declared {
                            assert_eq!(
                                builtin,
                                declared,
                                "Invalid type for built-in scheme. Got {} but expected {}",
                                format_type_scheme_around_name(name, &Some(declared.clone())),
                                format_type_scheme_around_name(name, &Some(builtin.clone()))
                            );
                        };
                        builtin.clone()
                    }
                    // Store an (uninstantiated) type scheme for symbols with a declared polymorphic type.
                    (None, Some(type_scheme)) => type_scheme.clone(),
                    // Store a new (unquantified) type variable for symbols without declared type.
                    // This forces a single concrete type for them.
                    (None, None) => self.unifier.new_type_var().into(),
                };
                (name.clone(), (source_ref, ty))
            })
            .collect();

        // Add builtin schemes if they are not already there and also remove them from the definitions
        // (because we ignore the defined value).
        for (name, scheme) in builtin_schemes() {
            self.declared_types
                .entry(name.clone())
                .or_insert_with(|| (SourceRef::unknown(), scheme.clone()));
            definitions.remove(name);
        }
    }

    /// Processes the definition of a symbol that is expected to have a concrete type.
    fn process_concrete_symbol(
        &mut self,
        declared_type: Type,
        value: &mut Expression,
    ) -> Result<(), Error> {
        match &declared_type {
            Type::Col => {
                // This is a column. It means we prefer `int -> fe`, but `int -> int`
                // is also OK if it can be derived directly.
                let return_type = self.unifier.new_type_var_name();
                let fun_type = Type::Function(FunctionType {
                    params: vec![Type::Int],
                    value: Box::new(Type::TypeVar(return_type.clone())),
                });
                self.expect_type_allow_fe_or_int(&fun_type, value, &return_type)
            }
            Type::Array(ArrayType { base, length: _ }) if base.as_ref() == &Type::Col => {
                // An array of columns. We prefer `(int -> fe)[]`, but we also allow `(int -> int)[]`.
                // Also we ignore the length.
                let return_type = self.unifier.new_type_var_name();
                let fun_type = Type::Function(FunctionType {
                    params: vec![Type::Int],
                    value: Box::new(Type::TypeVar(return_type.clone())),
                });
                let arr = Type::Array(ArrayType {
                    base: fun_type.into(),
                    length: None,
                });
                self.expect_type_allow_fe_or_int(&arr, value, &return_type)
            }
            Type::Inter => {
                // Values of intermediate columns have type `expr`
                self.expect_type(&Type::Expr, value)
            }
            Type::Array(ArrayType { base, length: _ }) if base.as_ref() == &Type::Inter => {
                // An array of intermediate columns with fixed length. We ignore the length.
                // The condenser will have to check the actual length.
                let arr = Type::Array(ArrayType {
                    base: Type::Expr.into(),
                    length: None,
                });
                self.expect_type(&arr, value)
            }
            t => self.expect_type(t, value),
        }
    }

    /// Performs type inference on `expr` expecting it to have type `expected_type`.
    /// The `flexible_var` is a type variable that occurs inside `expected_type`
    /// and it is fine to resolve either to `int` or `fe`.
    fn expect_type_allow_fe_or_int(
        &mut self,
        expected_type: &Type,
        expr: &mut Expression,
        flexible_var: &str,
    ) -> Result<(), Error> {
        self.expect_type(expected_type, expr)?;
        match self.type_into_substituted(Type::TypeVar(flexible_var.to_string())) {
            Type::Int => Ok(()),
            t => self
                .unifier
                .unify_types(t.clone(), Type::Fe)
                .map_err(|err| {
                    let substitute_flexible = |s: Type| {
                        let mut t = expected_type.clone();
                        t.substitute_type_vars(&[(flexible_var.to_string(), s)].into());
                        self.type_into_substituted(t)
                    };

                    expr.source_reference().with_error(format!(
                        "Expected either {} or {}, but got: {}.\n{err}",
                        substitute_flexible(Type::Int),
                        substitute_flexible(Type::Fe),
                        substitute_flexible(t)
                    ))
                }),
        }
    }

    /// Updates generic arguments and literal annotations with the proper resolved types.
    /// `type_var_mapping` is a mapping (for each generic symbol) from
    /// the type variable names used by the type checker to those from the declaration.
    fn update_type_args(
        &mut self,
        definitions: &mut HashMap<String, (Option<TypeScheme>, Option<&mut Expression>)>,
        expressions: &mut [(&mut Expression, ExpectedType)],
        type_var_mapping: &HashMap<String, HashMap<String, Type>>,
    ) -> Result<(), Vec<Error>> {
        let mut errors = vec![];
        definitions
            .iter_mut()
            .filter_map(|(name, (_, expr))| expr.as_mut().map(|expr| (name, expr)))
            .for_each(|(name, expr)| {
                let empty_mapping = Default::default();
                let var_mapping = type_var_mapping.get(name).unwrap_or(&empty_mapping);
                expr.post_visit_expressions_mut(&mut |e| {
                    if let Err(e) = self.update_type_args_for_expression(e, var_mapping) {
                        errors.push(e)
                    }
                });
            });

        for (expr, _) in expressions {
            expr.post_visit_expressions_mut(&mut |e| {
                // There should be no generic types in identities.
                if let Err(e) = self.update_type_args_for_expression(e, &Default::default()) {
                    errors.push(e)
                }
            });
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Updates the type annotations in the literals and the generic arguments.
    fn update_type_args_for_expression(
        &self,
        e: &mut Expression,
        type_var_mapping: &HashMap<String, Type>,
    ) -> Result<(), Error> {
        match e {
            Expression::Number(
                source_ref,
                Number {
                    value: n,
                    type_: annotated_type,
                },
            ) => match annotated_type {
                Some(Type::Int) | Some(Type::Fe) | Some(Type::Expr) => {}
                Some(Type::TypeVar(tv)) => {
                    let mut ty = Type::TypeVar(tv.clone());
                    // Apply regular substitution obtained from unification.
                    self.substitute(&mut ty);
                    if !ty
                        .contained_type_vars()
                        .all(|tv| type_var_mapping.contains_key(tv))
                    {
                        return Err(source_ref.with_error(format!(
                            "Unable to derive concrete type for literal {n}."
                        )));
                    }
                    // Rename type vars (hopefully just a single one) to match the declaration scheme.
                    ty.substitute_type_vars(type_var_mapping);
                    if let Type::TypeVar(tv) = ty {
                        *annotated_type = Some(Type::TypeVar(tv.clone()));
                    } else {
                        match ty {
                            Type::Int => *annotated_type = Some(Type::Int),
                            Type::Fe => *annotated_type = Some(Type::Fe),
                            Type::Expr => *annotated_type = Some(Type::Expr),
                            t => panic!("Invalid resolved type literal number: {t}"),
                        }
                    }
                }
                _ => panic!("Invalid annotation for literal number."),
            },
            Expression::Reference(
                source_ref,
                Reference::Poly(PolynomialReference {
                    name,
                    poly_id: _,
                    type_args,
                }),
            ) => {
                for ty in type_args.as_mut().unwrap() {
                    if !self.update_local_type(ty, type_var_mapping) {
                        return Err(source_ref.with_error(format!(
                            "Unable to derive concrete type for reference to generic symbol {name}"
                        )));
                    }
                }
            }
            Expression::BlockExpression(
                source_ref,
                BlockExpression {
                    statements,
                    expr: _,
                },
            ) => {
                for statement in statements {
                    match statement {
                        StatementInsideBlock::LetStatement(LetStatementInsideBlock {
                            ty,
                            pattern,
                            value: _,
                        }) => {
                            if !self.update_local_type(ty.as_mut().unwrap(), type_var_mapping) {
                                // TODO better source ref
                                return Err(source_ref.with_error(format!(
                                    "Unable to derive concrete type for local declaration {pattern}"
                                )));
                            }
                        }
                        StatementInsideBlock::Expression(_) => {}
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Updates the given local type with the current type variable mapping.
    /// Return false if the type still contains other type variable references after substitution.
    fn update_local_type(&self, ty: &mut Type, type_var_mapping: &HashMap<String, Type>) -> bool {
        // Apply regular substitution obtained from unification.
        self.substitute(ty);
        // Check if the remaining type vars are all in the declaration scheme.
        let is_concrete = ty
            .contained_type_vars()
            .all(|tv| type_var_mapping.contains_key(tv));
        // Now rename remaining type vars to match the declaration scheme.
        ty.substitute_type_vars(type_var_mapping);
        is_concrete
    }

    /// Type-checks the isolated expressions.
    fn check_expressions(
        &mut self,
        expressions: &mut [(&mut Expression, ExpectedType)],
    ) -> Result<(), Error> {
        for (e, expected_type) in expressions {
            self.expect_type_with_flexibility(expected_type, e)?;
        }
        Ok(())
    }

    /// Process an expression, inferring its type and expecting either a certain type or potentially an array of that type.
    fn expect_type_with_flexibility(
        &mut self,
        expected_type: &ExpectedType,
        expr: &mut Expression,
    ) -> Result<(), Error> {
        update_type_if_literal(expr, &expected_type.ty);

        let ty = self.infer_type_of_expression(expr)?;
        let ty = self.type_into_substituted(ty);
        let expected_type = if expected_type.allow_array && matches!(ty, Type::Array(_)) {
            Type::Array(ArrayType {
                base: Box::new(expected_type.ty.clone()),
                length: None,
            })
        } else if expected_type.allow_empty && (ty == Type::empty_tuple()) {
            Type::empty_tuple()
        } else {
            expected_type.ty.clone()
        };

        self.unifier
            .unify_types(ty.clone(), expected_type.clone())
            .map_err(|err| {
                expr.source_reference().with_error(format!(
                    "Expected type {} but got type {}.\n{err}",
                    self.format_type_with_bounds(expected_type),
                    self.format_type_with_bounds(ty),
                ))
            })
    }

    /// Process an expression and return the type of the expression.
    fn infer_type_of_expression(&mut self, e: &mut Expression) -> Result<Type, Error> {
        Ok(match e {
            Expression::Reference(_, Reference::LocalVar(id, _name)) => self.local_var_type(*id),
            Expression::Reference(
                source_ref,
                Reference::Poly(PolynomialReference {
                    name,
                    poly_id: _,
                    type_args,
                }),
            ) => {
                let (ty, args) = self.instantiate_scheme(self.declared_types[name].1.clone());
                if let Some(requested_type_args) = type_args {
                    if requested_type_args.len() != args.len() {
                        return Err(source_ref.with_error(format!(
                            "Expected {} type arguments for symbol {name}, but got {}: {}",
                            args.len(),
                            requested_type_args.len(),
                            requested_type_args.iter().join(", ")
                        )));
                    }
                    for (requested, inferred) in requested_type_args.iter_mut().zip(&args) {
                        requested.substitute_type_vars(&self.declared_type_vars);
                        self.unifier
                            .unify_types(requested.clone(), inferred.clone())
                            .map_err(|err| source_ref.with_error(err))?;
                    }
                }
                *type_args = Some(args);
                type_for_reference(&ty)
            }
            Expression::PublicReference(_, _) => Type::Expr,
            Expression::Number(
                _,
                Number {
                    type_: annotated_type,
                    ..
                },
            ) => {
                let ty = match annotated_type {
                    Some(Type::Int) => Type::Int,
                    Some(Type::Fe) => Type::Fe,
                    Some(Type::Expr) => Type::Expr,
                    Some(Type::TypeVar(tv)) => Type::TypeVar(tv.clone()),
                    Some(t) => panic!("Type name annotation for number is not supported: {t}"),
                    None => {
                        let tv = self.unifier.new_type_var_name();
                        *annotated_type = Some(Type::TypeVar(tv.clone()));
                        Type::TypeVar(tv)
                    }
                };
                self.unifier
                    .ensure_bound(&ty, "FromLiteral".to_string())
                    .map_err(|err| e.source_reference().with_error(err))?;
                ty
            }
            Expression::String(_, _) => Type::String,
            Expression::Tuple(_, items) => Type::Tuple(TupleType {
                items: items
                    .iter_mut()
                    .map(|item| self.infer_type_of_expression(item))
                    .collect::<Result<_, _>>()?,
            }),
            Expression::LambdaExpression(
                _,
                LambdaExpression {
                    kind,
                    params,
                    body,
                    outer_var_references: _,
                },
            ) => {
                let old_len = self.local_var_types.len();
                let result = params
                    .iter()
                    .map(|p| self.infer_type_of_pattern(p))
                    .collect::<Result<Vec<_>, _>>()
                    .and_then(|param_types| {
                        let old_lambda_kind = self.lambda_kind;
                        self.lambda_kind = *kind;
                        let body_type = self.infer_type_of_expression(body);
                        self.lambda_kind = old_lambda_kind;
                        Ok((param_types, body_type?))
                    });
                self.local_var_types.truncate(old_len);
                let (param_types, body_type) = result?;
                Type::Function(FunctionType {
                    params: param_types,
                    value: Box::new(body_type),
                })
            }
            Expression::ArrayLiteral(_, ArrayLiteral { items }) => {
                let item_type = self.unifier.new_type_var();
                for e in items {
                    self.expect_type(&item_type, e)?;
                }

                Type::Array(ArrayType {
                    base: Box::new(item_type.clone()),
                    length: None,
                })
            }
            Expression::BinaryOperation(source_ref, BinaryOperation { left, op, right }) => {
                // TODO at some point, also store the generic args for operators
                let fun_type = self.instantiate_scheme(binary_operator_scheme(*op)).0;
                self.infer_type_of_function_call(
                    fun_type,
                    [left, right].into_iter().map(AsMut::as_mut),
                    || format!("applying operator {op}"),
                    source_ref,
                )?
            }
            Expression::UnaryOperation(source_ref, UnaryOperation { op, expr: inner }) => {
                // TODO at some point, also store the generic args for operators
                let fun_type = self.instantiate_scheme(unary_operator_scheme(*op)).0;
                self.infer_type_of_function_call(
                    fun_type,
                    [inner].into_iter().map(AsMut::as_mut),
                    || format!("applying unary {op}"),
                    source_ref,
                )?
            }
            Expression::IndexAccess(_, IndexAccess { array, index }) => {
                let result = self.unifier.new_type_var();
                self.expect_type(
                    &Type::Array(ArrayType {
                        base: Box::new(result.clone()),
                        length: None,
                    }),
                    array,
                )?;

                self.expect_type(&Type::Int, index)?;
                result
            }
            Expression::FunctionCall(
                source_ref,
                FunctionCall {
                    function,
                    arguments,
                },
            ) => {
                let ft = self.infer_type_of_expression(function)?;
                self.infer_type_of_function_call(
                    ft,
                    arguments.iter_mut(),
                    || format!("calling function {function}"),
                    source_ref,
                )?
            }
            Expression::FreeInput(_, _) => todo!(),
            Expression::MatchExpression(_, MatchExpression { scrutinee, arms }) => {
                let scrutinee_type = self.infer_type_of_expression(scrutinee)?;
                let result = self.unifier.new_type_var();
                for MatchArm { pattern, value } in arms {
                    let local_var_count = self.local_var_types.len();
                    self.expect_type_of_pattern(&scrutinee_type, pattern)?;
                    let result = self.expect_type(&result, value);
                    self.local_var_types.truncate(local_var_count);
                    result?;
                }
                result
            }
            Expression::IfExpression(_, if_expr) => {
                self.expect_type(&Type::Bool, &mut if_expr.condition)?;
                let result = self.infer_type_of_expression(&mut if_expr.body)?;
                self.expect_type(&result, &mut if_expr.else_body)?;
                result
            }
            Expression::BlockExpression(source_ref, BlockExpression { statements, expr }) => {
                let original_var_count = self.local_var_types.len();

                for statement in statements {
                    match statement {
                        StatementInsideBlock::LetStatement(LetStatementInsideBlock {
                            pattern,
                            ty,
                            value,
                        }) => {
                            match (&ty, value) {
                                (Some(ty), Some(value)) => {
                                    self.process_concrete_symbol(ty.clone(), value)?
                                }
                                (Some(ty), None) => {
                                    if *ty != Type::Col {
                                        // TODO better source ref
                                        return Err(source_ref.with_error("Let-declared variables without value must have type 'col'.".to_string()));
                                    }
                                }
                                (None, Some(value)) => {
                                    *ty = Some(self.infer_type_of_expression(value)?)
                                }
                                (None, None) => *ty = Some(Type::Col),
                            };
                            let var_type = type_for_reference(ty.as_ref().unwrap());
                            self.expect_type_of_pattern(&var_type, pattern)?;
                        }
                        StatementInsideBlock::Expression(expr) => {
                            self.expect_type_with_flexibility(&self.statement_type(), expr)?;
                        }
                    }
                }
                let result = match expr {
                    Some(expr) => self.infer_type_of_expression(expr),
                    None => Ok(Type::empty_tuple()),
                };

                self.local_var_types.truncate(original_var_count);
                result?
            }
        })
    }

    /// Returns the type expected at statement level, given the current function context.
    fn statement_type(&self) -> ExpectedType {
        if self.lambda_kind == FunctionKind::Constr {
            constr_function_statement_type()
        } else {
            Type::empty_tuple().into()
        }
    }

    /// Process a function call and return the type of the expression.
    /// The error message is used to clarify which kind of function call it is
    /// (it might be an operator).
    fn infer_type_of_function_call<'b>(
        &mut self,
        function_type: Type,
        arguments: impl ExactSizeIterator<Item = &'b mut Expression>,
        error_message: impl FnOnce() -> String,
        source_ref: &SourceRef,
    ) -> Result<Type, Error> {
        let arguments = arguments.collect::<Vec<_>>();
        let params = (0..arguments.len())
            .map(|_| self.unifier.new_type_var())
            .collect::<Vec<_>>();
        let result_type = self.unifier.new_type_var();
        let expected_function_type = Type::Function(FunctionType {
            params: params.clone(),
            value: Box::new(result_type.clone()),
        });
        self.unifier
            .unify_types(function_type.clone(), expected_function_type.clone())
            .map_err(|err| {
                source_ref.with_error(format!(
                    "Expected function of type `{}`, but got `{}` when {} on ({}):\n{err}",
                    self.format_type_with_bounds(expected_function_type),
                    self.format_type_with_bounds(function_type),
                    error_message(),
                    arguments.iter().format(", ")
                ))
            })?;

        for (arg, param) in arguments.into_iter().zip(params) {
            self.expect_type(&param, arg)?;
        }
        Ok(result_type)
    }

    /// Process the expression and unify it with the given type.
    /// This function should be preferred over `infer_type_of_expression` if an expected type is known
    /// because we can create better error messages.
    fn expect_type(&mut self, expected_type: &Type, expr: &mut Expression) -> Result<(), Error> {
        update_type_if_literal(expr, expected_type);

        let inferred_type = self.infer_type_of_expression(expr)?;
        self.unifier
            .unify_types(inferred_type.clone(), expected_type.clone())
            .map_err(|err| {
                expr.source_reference().with_error(format!(
                    "Expected type: {}\nInferred type: {}\n{err}",
                    self.format_type_with_bounds(expected_type.clone()),
                    self.format_type_with_bounds(inferred_type)
                ))
            })
    }

    /// Type-checks a pattern and adds local variables.
    fn expect_type_of_pattern(
        &mut self,
        expected_type: &Type,
        pattern: &Pattern,
    ) -> Result<(), Error> {
        let inferred_type = self.infer_type_of_pattern(pattern)?;
        self.unifier
            .unify_types(inferred_type.clone(), expected_type.clone())
            .map_err(|err| {
                pattern.source_reference().with_error(format!(
                    "Error checking pattern:\nExpected type: {}\nInferred type: {}\n{err}",
                    self.format_type_with_bounds(expected_type.clone()),
                    self.format_type_with_bounds(inferred_type)
                ))
            })
    }

    /// Type-checks a pattern and adds local variables.
    fn infer_type_of_pattern(&mut self, pattern: &Pattern) -> Result<Type, Error> {
        Ok(match pattern {
            Pattern::Ellipsis(_) => unreachable!("Should be handled higher up."),
            Pattern::CatchAll(_) => self.unifier.new_type_var(),
            Pattern::Number(source_ref, _) => {
                let ty = self.unifier.new_type_var();
                self.unifier
                    .ensure_bound(&ty, "FromLiteral".to_string())
                    .map_err(|e| source_ref.with_error(e))?;
                ty
            }
            Pattern::String(_, _) => Type::String,
            Pattern::Tuple(_, items) => Type::Tuple(TupleType {
                items: items
                    .iter()
                    .map(|p| self.infer_type_of_pattern(p))
                    .collect::<Result<_, _>>()?,
            }),
            Pattern::Array(_, items) => {
                let item_type = self.unifier.new_type_var();
                for item in items {
                    if !matches!(item, Pattern::Ellipsis(_)) {
                        self.expect_type_of_pattern(&item_type, item)?;
                    }
                }
                Type::Array(ArrayType {
                    base: Box::new(item_type),
                    length: None,
                })
            }
            Pattern::Variable(_, _) => {
                let ty = self.unifier.new_type_var();
                self.local_var_types.push(ty.clone());
                ty
            }
            Pattern::Enum(source_ref, name, data) => {
                // We just ignore the generic args here, storing them in the pattern
                // is not helpful because the type is obvious from the value.
                let (ty, _generic_args) = self
                    .instantiate_scheme(self.declared_types[&name.to_dotted_string()].1.clone());
                let ty = type_for_reference(&ty);

                match data {
                    Some(data) => {
                        let Type::Function(FunctionType { params, value }) = ty else {
                            return Err(source_ref.with_error(if matches!(ty, Type::NamedType(_, _)) {
                                format!("Enum variant {name} does not have fields, but is used with parentheses in pattern.")
                            } else {
                                format!("Expected enum variant for pattern but got {ty}")
                            }));
                        };
                        if !matches!(value.as_ref(), Type::NamedType(_, _)) {
                            return Err(source_ref.with_error(format!(
                                "Expected enum variant for pattern but got {value}"
                            )));
                        }
                        if params.len() != data.len() {
                            return Err(source_ref.with_error(format!(
                                "Invalid number of data fields for enum variant {name}. Expected {} but got {}.",
                                params.len(),
                                data.len()
                            )));
                        }
                        params
                            .iter()
                            .zip(data)
                            .try_for_each(|(ty, pat)| self.expect_type_of_pattern(ty, pat))?;
                        (*value).clone()
                    }
                    None => {
                        if let Type::NamedType(_, _) = ty {
                            ty
                        } else if matches!(ty, Type::Function(_)) {
                            return Err(source_ref.with_error(format!(
                                "Expected enum variant for pattern but got {ty} - maybe you forgot the parentheses?"
                            )));
                        } else {
                            return Err(source_ref.with_error(format!(
                                "Expected enum variant for pattern but got {ty}"
                            )));
                        }
                    }
                }
            }
        })
    }

    /// Returns, for each name declared with a type scheme, a mapping from
    /// the type variables used by the type checker to those used in the declaration.
    fn verify_type_schemes(
        &self,
        inferred_types: HashMap<String, Type>,
    ) -> Result<HashMap<String, HashMap<String, Type>>, Error> {
        inferred_types.into_iter().map(|(name, inferred_type)| {
            let (source_ref, declared_type) = self.declared_types[&name].clone();
            let inferred_type = self.type_into_substituted(inferred_type.clone());
            let inferred = self.to_type_scheme(inferred_type.clone());
            let declared = declared_type.clone().simplify_type_vars();
            if inferred != declared {
                return Err(source_ref.with_error(format!(
                    "Inferred type scheme for symbol {name} does not match the declared type.\nInferred: let{}\nDeclared: let{}",
                    format_type_scheme_around_name(&name, &Some(inferred)),
                    format_type_scheme_around_name(&name, &Some(declared_type),
                ))));
            }
            let declared_type_vars = declared_type.ty.contained_type_vars();
            let inferred_type_vars = inferred_type.contained_type_vars();
            Ok((name.clone(),
                inferred_type_vars
                    .into_iter()
                    .cloned()
                    .zip(declared_type_vars.into_iter().map(|tv| Type::TypeVar(tv.clone())))
                    .collect(),
            ))
        }).collect::<Result<_, Error>>()
    }

    fn type_into_substituted(&self, mut ty: Type) -> Type {
        self.substitute(&mut ty);
        ty
    }

    fn substitute(&self, ty: &mut Type) {
        self.unifier.substitute(ty);
    }

    /// Instantiates a type scheme by creating new type variables for the quantified
    /// type variables in the scheme and adds the required trait bounds for the
    /// new type variables.
    /// Returns the new type and a vector of the type variables used for those
    /// declared in the scheme.
    fn instantiate_scheme(&mut self, scheme: TypeScheme) -> (Type, Vec<Type>) {
        self.unifier.instantiate_scheme(scheme)
    }

    fn format_type_with_bounds(&self, ty: Type) -> String {
        let scheme = self.to_type_scheme(ty);
        let bounds = scheme.vars.format_vars_with_nonempty_bounds();
        if bounds.is_empty() {
            scheme.ty.to_string()
        } else if let Type::TypeVar(_) = &scheme.ty {
            bounds
        } else {
            format!("{} with {bounds}", scheme.ty,)
        }
    }

    /// Creates a type scheme out of a type by making all unsubstituted
    /// type variables generic.
    /// TODO this is wrong for mutually recursive generic functions.
    fn to_type_scheme(&self, ty: Type) -> TypeScheme {
        let ty = self.type_into_substituted(ty);
        let vars = TypeBounds::new(ty.contained_type_vars().map(|v| {
            (
                v.clone(),
                self.unifier
                    .type_var_bounds(v)
                    .into_iter()
                    .collect::<BTreeSet<_>>(),
            )
        }));
        TypeScheme { vars, ty }.simplify_type_vars()
    }

    pub fn local_var_type(&self, id: u64) -> Type {
        self.local_var_types[id as usize].clone()
    }
}

fn update_type_if_literal(
    expr: &mut powdr_ast::parsed::Expression<Reference>,
    expected_type: &Type,
) {
    // For literals, we try to store the type here already.
    // This avoids creating tons of type variables for large arrays.
    if let Expression::Number(
        _,
        Number {
            type_: annotated_type @ None,
            ..
        },
    ) = expr
    {
        match expected_type.clone() {
            Type::Int => *annotated_type = Some(Type::Int),
            Type::Fe => *annotated_type = Some(Type::Fe),
            Type::Expr => *annotated_type = Some(Type::Expr),
            Type::TypeVar(tv) => *annotated_type = Some(Type::TypeVar(tv.clone())),
            _ => {}
        };
    }
}
