use std::collections::HashMap;

use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, Reference, Symbol, SymbolKind, TypedExpression,
    },
    parsed::{
        types::Type, BlockExpression, FunctionCall, FunctionKind, LambdaExpression,
        StatementInsideBlock,
    },
};

use lazy_static::lazy_static;

/// Check that query functions are only referenced/defined in a query context
/// and that constr functions are only referenced/defined in a constr context.
pub fn check(
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    context: FunctionKind,
    e: &Expression,
) -> Result<(), String> {
    SideEffectChecker {
        definitions,
        context,
    }
    .check(e)
}

struct SideEffectChecker<'a> {
    definitions: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    context: FunctionKind,
}

impl<'a> SideEffectChecker<'a> {
    fn check(&mut self, e: &Expression) -> Result<(), String> {
        match e {
            Expression::Reference(_, Reference::Poly(r)) => {
                let kind = self.function_kind_of_symbol(&r.name);
                if kind != FunctionKind::Pure && kind != self.context {
                    return Err(format!(
                        "Referenced a {kind} function inside a {} context: {}",
                        self.context, r.name,
                    ));
                }
                Ok(())
            }
            Expression::LambdaExpression(
                _,
                LambdaExpression {
                    kind,
                    params: _,
                    body,
                    outer_var_references: _,
                },
            ) => {
                if *kind != FunctionKind::Pure && *kind != self.context {
                    return Err(format!(
                        "Used a {kind} lambda function inside a {} context: {e}",
                        self.context
                    ));
                }
                let old_context = self.context;
                let result = self.check(body);
                self.context = old_context;
                result
            }
            Expression::BlockExpression(_, BlockExpression { statements, .. }) => {
                for s in statements {
                    if let StatementInsideBlock::LetStatement(ls) = s {
                        if ls.value.is_none() && self.context != FunctionKind::Constr {
                            return Err(format!(
                                "Tried to create a witness column in a {} context: {ls}",
                                self.context
                            ));
                        } else if ls.ty == Some(Type::Col) && self.context != FunctionKind::Constr {
                            return Err(format!(
                                "Tried to create a fixed column in a {} context: {ls}",
                                self.context
                            ));
                        }
                    }
                }
                e.children().try_for_each(|e| self.check(e))
            }
            Expression::FunctionCall(
                _,
                FunctionCall {
                    function,
                    arguments,
                },
            ) if matches!(function.as_ref(), Expression::Reference(_, Reference::Poly(r)) if r.name == "std::prelude::set_hint") =>
            {
                // The function "set_hint" is special: It expects a "query" function as
                // second argument, so we switch context when descending into the second argument.
                self.check(function)?;
                match &arguments[..] {
                    [col, hint] => {
                        self.check(col)?;
                        assert_eq!(self.context, FunctionKind::Constr);
                        self.context = FunctionKind::Query;
                        let result = self.check(hint);
                        self.context = FunctionKind::Constr;
                        result
                    }
                    _ => {
                        // Not the correct number of arguments, will lead to a type error later.
                        arguments.iter().try_for_each(|e| self.check(e))
                    }
                }
            }
            _ => e.children().try_for_each(|e| self.check(e)),
        }
    }

    /// Returns the function kind of a referenced symbol.
    fn function_kind_of_symbol(&self, name: &str) -> FunctionKind {
        if let Some(kind) = BUILTIN_KINDS.get(name) {
            return *kind;
        }
        let (symbol, value) = self.definitions.get(name).unwrap();
        if symbol.kind != SymbolKind::Other() {
            // If referenced, columns are `expr`, so they are not functions and thus pure.
            return FunctionKind::Pure;
        }
        if let Some(FunctionValueDefinition::Expression(TypedExpression {
            type_scheme: _,
            e: Expression::LambdaExpression(_, LambdaExpression { kind, .. }),
        })) = value
        {
            *kind
        } else {
            FunctionKind::Pure
        }
    }
}

lazy_static! {
    static ref BUILTIN_KINDS: HashMap<&'static str, FunctionKind> = [
        ("std::array::len", FunctionKind::Pure),
        ("std::check::panic", FunctionKind::Pure),
        ("std::convert::expr", FunctionKind::Pure),
        ("std::convert::fe", FunctionKind::Pure),
        ("std::convert::int", FunctionKind::Pure),
        ("std::convert::expr", FunctionKind::Pure),
        ("std::debug::print", FunctionKind::Pure),
        ("std::field::modulus", FunctionKind::Pure),
        ("std::prelude::challenge", FunctionKind::Constr), // strictly, only new_challenge would need "constr"
        ("std::prover::min_degree", FunctionKind::Pure),
        ("std::prover::max_degree", FunctionKind::Pure),
        ("std::prover::degree", FunctionKind::Pure),
        ("std::prelude::set_hint", FunctionKind::Constr),
        ("std::prover::eval", FunctionKind::Query),
    ]
    .into_iter()
    .collect();
}
