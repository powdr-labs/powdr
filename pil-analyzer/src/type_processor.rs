use std::{collections::HashSet, str::FromStr};

use powdr_ast::parsed::{asm::SymbolPath, types::Type, visitor::Children, Expression};
use powdr_number::BigUint;
use powdr_parser_util::SourceRef;

use crate::{evaluator::EvalError, untyped_evaluator, AnalysisDriver};

/// The TypeProcessor turns parsed types into analyzed types, which means that
/// it resolves local type name references, replaces named types that actually
/// refer to type variables by actual type variables and evaluates array lengths.
/// It is is unrelated to type inference, which is done later.
pub struct TypeProcessor<'a, D: AnalysisDriver> {
    driver: D,
    type_vars: &'a HashSet<&'a String>,
}

impl<'a, D: AnalysisDriver> TypeProcessor<'a, D> {
    pub fn new(driver: D, type_vars: &'a HashSet<&'a String>) -> Self {
        Self { driver, type_vars }
    }

    /// Processes a type name by evaluating array lengths, changing named type references to type
    /// variables to actual type variables and resolving references to named types.
    pub fn process_type(&self, ty: Type<Expression>) -> Type {
        let ty = self.evaluate_array_lengths(ty.clone())
            .map_err(|e| panic!("Error evaluating expressions in type name \"{ty}\" to reduce it to a type:\n{e})"))
            .unwrap();
        self.process_number_type(ty)
    }

    /// Processes a type name by changing named type references to type variables to actual type
    /// variables and resolving references to named types.
    pub fn process_number_type(&self, mut ty: Type<u64>) -> Type {
        ty.map_to_type_vars(self.type_vars);
        ty.contained_named_types_mut().for_each(|n| {
            let name = self
                .driver
                .resolve_type_ref(&SourceRef::unknown(), n)
                .unwrap();
            *n = SymbolPath::from_str(&name).unwrap();
        });
        ty
    }

    /// Turns a Type<Expression> to a Type<u64> by evaluating the array length expressions.
    fn evaluate_array_lengths(&self, mut t: Type<Expression>) -> Result<Type, EvalError> {
        // Replace all expressions by number literals.
        // Any expression inside a type name has to be an array length,
        // so we expect an integer that fits u64.
        t.children_mut().try_for_each(|e: &mut Expression| {
            let v = untyped_evaluator::evaluate_expression_to_int(self.driver, e.clone())?;
            let v_u64: u64 = v.clone().try_into().map_err(|_| {
                EvalError::TypeError(format!("Number too large, expected u64, but got {v}"))
            })?;
            *e = BigUint::from(v_u64).into();
            Ok(())
        })?;
        Ok(t.into())
    }
}
