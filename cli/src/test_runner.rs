use std::{
    fs,
    path::{Path, PathBuf},
    str::FromStr,
};

use itertools::Itertools;

use powdr::ast::{
    analyzed::FunctionValueDefinition,
    parsed::{
        asm::SymbolPath,
        types::{FunctionType, Type},
    },
};
use powdr::pil_analyzer::evaluator::{self, SymbolLookup};
use powdr::{FieldElement, Pipeline};

/// Executes all functions in the given file that start with `test_` and are
/// inside a module called `test`.
///
/// @param include_std_tests: Whether to run the tests inside the standard library.
pub fn run<F: FieldElement>(input: &str, include_std_tests: bool) -> Result<(), Vec<String>> {
    let mut pipeline = Pipeline::<F>::default().from_file(PathBuf::from(&input));

    let analyzed = pipeline.compute_analyzed_pil()?;

    let mut symbols = evaluator::Definitions {
        definitions: &analyzed.definitions,
        solved_impls: &analyzed.solved_impls,
    };

    for (name, (_, val)) in analyzed
        .definitions
        .iter()
        .filter(|(n, _)| n.starts_with("test::test_") || n.contains("::test::test_"))
        .filter(|(n, _)| include_std_tests || !n.starts_with("std::"))
        .filter(|(n, _)| SymbolPath::from_str(n).unwrap().name().starts_with("test_"))
        .sorted_by_key(|(n, _)| *n)
    {
        let Some(FunctionValueDefinition::Expression(f)) = val else {
            continue;
        };
        // Require a plain `->()` type.
        let type_scheme = f.type_scheme.as_ref().unwrap();
        if !type_scheme.vars.is_empty()
            || type_scheme.ty
                != (FunctionType {
                    params: vec![],
                    value: Box::new(Type::empty_tuple()),
                })
                .into()
        {
            continue;
        }
        print!("Running test: {name}...");
        let function = symbols.lookup(name, &None).unwrap();
        evaluator::evaluate_function_call::<F>(function, vec![], &mut symbols).unwrap();
        println!("  ok");
    }

    Ok(())
}
