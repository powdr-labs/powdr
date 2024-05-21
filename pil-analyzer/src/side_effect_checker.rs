use std::collections::HashMap;

use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, Reference, Symbol, SymbolKind, TypedExpression,
    },
    parsed::{FunctionKind, LambdaExpression, StatementInsideBlock},
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
            Expression::Reference(Reference::Poly(r)) => {
                let kind = self.function_kind_of_symbol(&r.name);
                if kind != FunctionKind::Pure && kind != self.context {
                    return Err(format!(
                        "Referenced a {kind} function inside a {} context: {}",
                        self.context, r.name,
                    ));
                }
                Ok(())
            }
            Expression::LambdaExpression(LambdaExpression {
                kind,
                params: _,
                body,
            }) => {
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
            Expression::BlockExpression(statements, _expr) => {
                for s in statements {
                    match s {
                        StatementInsideBlock::LetStatement(s) => {
                            if s.value.is_none() && self.context != FunctionKind::Constr {
                                return Err(format!(
                                    "Tried to create a witness column in a {} context: {s}",
                                    self.context
                                ));
                            }
                        }
                        StatementInsideBlock::Expression(expr) => {
                            if self.context != FunctionKind::Constr {
                                return Err(format!(
                                    "Tried to add a constraint in a {} context: {expr}",
                                    self.context
                                ));
                            }
                        }
                    }
                }
                e.children().try_for_each(|e| self.check(e))
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
            e: Expression::LambdaExpression(LambdaExpression { kind, .. }),
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
        ("std::prover::capture_stage", FunctionKind::Constr),
        ("std::prover::challenge", FunctionKind::Constr), // strictly, only new_challenge would need "constr"
        ("std::prover::degree", FunctionKind::Pure),
        ("std::prover::eval", FunctionKind::Query),
    ]
    .into_iter()
    .collect();
}
