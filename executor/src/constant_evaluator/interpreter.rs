use std::{
    collections::{BTreeMap, HashMap},
    sync::{Arc, RwLock},
};

use powdr_ast::{
    analyzed::{Analyzed, FunctionValueDefinition, Symbol, TypedExpression},
    parsed::{
        types::{ArrayType, Type},
        IndexAccess,
    },
};
use powdr_number::{BigInt, BigUint, DegreeType, FieldElement};
use powdr_pil_analyzer::evaluator::{self, Definitions, SymbolLookup, Value};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

/// Evaluates the fixed polynomial `name` on all values from 0 to `degree - 1`
/// using an interpreter.
/// If `index` is `Some(i)`, evaluates the `i`-th element of the array.
pub fn generate_values<T: FieldElement>(
    analyzed: &Analyzed<T>,
    degree: DegreeType,
    name: &str,
    body: &FunctionValueDefinition,
    index: Option<u64>,
) -> Vec<T> {
    let symbols = CachedSymbols {
        symbols: &analyzed.definitions,
        cache: Arc::new(RwLock::new(Default::default())),
        degree,
    };
    let result = match body {
        FunctionValueDefinition::Expression(TypedExpression { e, type_scheme }) => {
            if let Some(type_scheme) = type_scheme {
                assert!(type_scheme.vars.is_empty());
                let ty = &type_scheme.ty;
                if ty == &Type::Col {
                    assert!(index.is_none());
                } else if let Type::Array(ArrayType { base, length: _ }) = ty {
                    assert!(index.is_some());
                    assert_eq!(base.as_ref(), &Type::Col);
                } else {
                    panic!("Invalid fixed column type: {ty}");
                }
            };
            let index_expr;
            let e = if let Some(index) = index {
                index_expr = IndexAccess {
                    array: e.clone().into(),
                    index: Box::new(BigUint::from(index).into()),
                }
                .into();
                &index_expr
            } else {
                e
            };
            let fun = evaluator::evaluate(e, &mut symbols.clone()).unwrap();
            (0..degree)
                .into_par_iter()
                .map(|i| {
                    evaluator::evaluate_function_call(
                        fun.clone(),
                        vec![Arc::new(Value::Integer(BigInt::from(i)))],
                        &mut symbols.clone(),
                    )?
                    .try_to_field_element()
                })
                .collect::<Result<Vec<_>, _>>()
        }
        FunctionValueDefinition::Array(values) => {
            assert!(index.is_none());
            values
                .to_repeated_arrays(degree)
                .map(|elements| {
                    let items = elements
                        .pattern()
                        .iter()
                        .map(|v| {
                            let mut symbols = symbols.clone();
                            evaluator::evaluate(v, &mut symbols)
                                .and_then(|v| v.try_to_field_element())
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    Ok(items
                        .into_iter()
                        .cycle()
                        .take(elements.size() as usize)
                        .collect::<Vec<_>>())
                })
                .collect::<Result<Vec<_>, _>>()
                .map(|values| {
                    let values: Vec<T> = values.into_iter().flatten().collect();
                    assert_eq!(values.len(), degree as usize);
                    values
                })
        }
        FunctionValueDefinition::TypeDeclaration(_)
        | FunctionValueDefinition::TypeConstructor(_, _)
        | FunctionValueDefinition::TraitDeclaration(_)
        | FunctionValueDefinition::TraitFunction(_, _) => panic!(),
    };
    match result {
        Err(err) => {
            eprintln!("Error evaluating fixed polynomial {name}{body}:\n{err}");
            panic!("{err}");
        }
        Ok(v) => v,
    }
}

type SymbolCache<'a, T> = HashMap<String, BTreeMap<Option<Vec<Type>>, Arc<Value<'a, T>>>>;

#[derive(Clone)]
pub struct CachedSymbols<'a, T> {
    symbols: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    cache: Arc<RwLock<SymbolCache<'a, T>>>,
    degree: DegreeType,
}

impl<'a, T: FieldElement> SymbolLookup<'a, T> for CachedSymbols<'a, T> {
    fn lookup(
        &mut self,
        name: &'a str,
        type_args: &Option<Vec<Type>>,
    ) -> Result<Arc<Value<'a, T>>, evaluator::EvalError> {
        if let Some(v) = self
            .cache
            .read()
            .unwrap()
            .get(name)
            .and_then(|map| map.get(type_args))
        {
            return Ok(v.clone());
        }
        let result = Definitions::lookup_with_symbols(self.symbols, name, type_args, self)?;
        self.cache
            .write()
            .unwrap()
            .entry(name.to_string())
            .or_default()
            .entry(type_args.clone())
            .or_insert_with(|| result.clone());
        Ok(result)
    }

    fn degree(&self) -> Result<Arc<Value<'a, T>>, evaluator::EvalError> {
        Ok(Value::Integer(self.degree.into()).into())
    }
}
