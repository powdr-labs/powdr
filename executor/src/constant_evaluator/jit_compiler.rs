use std::collections::HashMap;

use powdr_ast::analyzed::{Analyzed, PolyID};
use powdr_number::FieldElement;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use super::VariablySizedColumn;

/// Tries to just-in-time compile all fixed columns in `analyzed`
/// and then evaluates those functions to return `VariablySizedColumn`s.
/// Ignores all columns where the compilation fails.
pub fn generate_values<T: FieldElement>(
    analyzed: &Analyzed<T>,
) -> HashMap<(String, PolyID), VariablySizedColumn<T>> {
    let compiled_pil = match powdr_jit_compiler::compile(analyzed, &symbols_to_compile(analyzed)) {
        Err(err) => {
            // TODO this should be changed back to Error after the introduction of the ToCol trait.
            log::debug!("Failed to compile some constant columns: {err}");
            return HashMap::new();
        }
        Ok(fun_map) => fun_map,
    };

    analyzed
        .constant_polys_in_source_order()
        .filter_map(|(symbol, _)| {
            let fun = compiled_pil.get_fixed_column(symbol.absolute_name.as_str())?;
            Some((symbol, fun))
        })
        .map(|(symbol, fun)| {
            let column_values: Vec<Vec<T>> = symbol
                .degree
                .unwrap()
                .iter()
                .map(|degree| {
                    compiled_pil.set_degree(degree);
                    (0..degree)
                        .into_par_iter()
                        .map(|i| {
                            let result = fun.call(i);
                            T::from(result)
                        })
                        .collect()
                })
                .collect();

            (
                (symbol.absolute_name.clone(), symbol.into()),
                column_values.into(),
            )
        })
        .collect()
}

fn symbols_to_compile<T>(analyzed: &Analyzed<T>) -> Vec<&str> {
    analyzed
        .constant_polys_in_source_order()
        .filter_map(|(symbol, value)| {
            (!symbol.is_array() && value.is_some()).then_some(symbol.absolute_name.as_str())
        })
        .collect()
}
