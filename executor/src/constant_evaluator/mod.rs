use std::{collections::HashMap, fmt::Display, rc::Rc};

use itertools::Itertools;
use powdr_ast::analyzed::{Analyzed, FunctionValueDefinition};
use powdr_number::{DegreeType, FieldElement};
use powdr_pil_analyzer::evaluator::{self, Custom, EvalError, SymbolLookup, Value};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

/// Generates the constant polynomial values for all constant polynomials
/// that are defined (and not just declared).
/// @returns the values (in source order) and the degree of the polynomials.
pub fn generate<T: FieldElement>(analyzed: &Analyzed<T>) -> Vec<(&str, Vec<T>)> {
    let mut other_constants = HashMap::new();
    for (poly, value) in analyzed.constant_polys_in_source_order() {
        if let Some(value) = value {
            let values = generate_values(
                analyzed,
                analyzed.degree(),
                &poly.absolute_name,
                value,
                &other_constants,
            );
            other_constants.insert(&poly.absolute_name, values);
        }
    }

    other_constants
        .into_iter()
        .sorted_by_key(|(name, _)| analyzed.definitions[&name.to_string()].0.id)
        .collect::<Vec<_>>()
}

fn generate_values<T: FieldElement>(
    analyzed: &Analyzed<T>,
    degree: DegreeType,
    name: &str,
    body: &FunctionValueDefinition<T>,
    computed_columns: &HashMap<&str, Vec<T>>,
) -> Vec<T> {
    let symbols = Symbols {
        analyzed,
        computed_columns,
    };
    // TODO we should maybe pre-compute some symbols here.
    let result = match body {
        FunctionValueDefinition::Expression(e) => (0..degree)
            .into_par_iter()
            .map(|i| {
                // We could try to avoid the first evaluation to be run for each iteration,
                // but the data is not thread-safe.
                let fun = evaluator::evaluate(e, &symbols).unwrap();
                evaluator::evaluate_function_call(
                    fun,
                    vec![Rc::new(Value::Integer(num_bigint::BigInt::from(i)))],
                    &symbols,
                )
                .and_then(|v| v.try_to_field_element())
            })
            .collect::<Result<Vec<_>, _>>(),
        FunctionValueDefinition::Array(values) => values
            .iter()
            .map(|elements| {
                let items = elements
                    .pattern()
                    .iter()
                    .map(|v| {
                        evaluator::evaluate(v, &symbols).and_then(|v| v.try_to_field_element())
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
            }),
        FunctionValueDefinition::Query(_) => panic!("Query used for fixed column."),
    };
    match result {
        Err(err) => {
            eprintln!("Error evaluating fixed polynomial {name}{body}:\n{err}");
            panic!("{err}");
        }
        Ok(v) => v,
    }
}

struct Symbols<'a, T> {
    pub analyzed: &'a Analyzed<T>,
    pub computed_columns: &'a HashMap<&'a str, Vec<T>>,
}

impl<'a, T: FieldElement> SymbolLookup<'a, T, FixedColumnRef<'a>> for Symbols<'a, T> {
    fn lookup(&self, name: &str) -> Result<Value<'a, T, FixedColumnRef<'a>>, EvalError> {
        Ok(
            if let Some((name, _)) = self.computed_columns.get_key_value(name) {
                Value::Custom(FixedColumnRef { name })
            } else if let Some((_, value)) = self.analyzed.definitions.get(&name.to_string()) {
                match value {
                    Some(FunctionValueDefinition::Expression(value)) => {
                        evaluator::evaluate(value, self)?
                    }
                    Some(_) => Err(EvalError::Unsupported(
                        "Cannot evaluate arrays and queries.".to_string(),
                    ))?,
                    None => Err(EvalError::Unsupported(
                        "Cannot evaluate witness columns.".to_string(),
                    ))?,
                }
            } else {
                Err(EvalError::SymbolNotFound(format!(
                    "Symbol {name} not found."
                )))?
            },
        )
    }

    fn eval_function_application(
        &self,
        function: FixedColumnRef<'a>,
        arguments: &[Rc<Value<'a, T, FixedColumnRef<'a>>>],
    ) -> Result<Value<'a, T, FixedColumnRef<'a>>, EvalError> {
        if arguments.len() != 1 {
            Err(EvalError::TypeError(format!(
                "Expected one argument, but got {}",
                arguments.len()
            )))?
        };
        let Value::Integer(row) = arguments[0].as_ref() else {
            return Err(EvalError::TypeError(format!(
                "Expected integer but got {}",
                arguments[0]
            )));
        };
        let data = &self.computed_columns[function.name];
        Ok(Value::FieldElement(
            data[usize::try_from(row).unwrap() % data.len()],
        ))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct FixedColumnRef<'a> {
    pub name: &'a str,
}

impl<'a> Custom for FixedColumnRef<'a> {
    fn type_name(&self) -> String {
        "col".to_string()
    }
}

impl<'a> Display for FixedColumnRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[cfg(test)]
mod test {
    use powdr_number::GoldilocksField;
    use powdr_pil_analyzer::analyze_string;
    use pretty_assertions::assert_eq;
    use test_log::test;

    use super::*;

    fn convert(input: Vec<i32>) -> Vec<GoldilocksField> {
        input.into_iter().map(|x| x.into()).collect()
    }

    #[test]
    pub fn test_last() {
        let src = r#"
            constant %N = 8;
            namespace F(%N);
            pol constant LAST(i) { match i {
                %N - 1 => 1,
                _ => 0,
            } };
        "#;
        let analyzed = analyze_string(src);
        assert_eq!(analyzed.degree(), 8);
        let constants = generate(&analyzed);
        assert_eq!(
            constants,
            vec![("F.LAST", convert(vec![0, 0, 0, 0, 0, 0, 0, 1]))]
        );
    }

    #[test]
    pub fn test_counter() {
        let src = r#"
            constant %N = 8;
            namespace F(%N);
            pol constant EVEN(i) { 2 * (i - 1) + 4 };
        "#;
        let analyzed = analyze_string(src);
        assert_eq!(analyzed.degree(), 8);
        let constants = generate(&analyzed);
        assert_eq!(
            constants,
            vec![("F.EVEN", convert(vec![2, 4, 6, 8, 10, 12, 14, 16]))]
        );
    }

    #[test]
    pub fn test_xor() {
        let src = r#"
            constant %N = 8;
            namespace F(%N);
            pol constant X(i) { i ^ (i + 17) | 3 };
        "#;
        let analyzed = analyze_string(src);
        assert_eq!(analyzed.degree(), 8);
        let constants = generate(&analyzed);
        assert_eq!(
            constants,
            vec![("F.X", convert((0..8).map(|i| i ^ (i + 17) | 3).collect()))]
        );
    }

    #[test]
    pub fn test_match() {
        let src = r#"
            constant %N = 8;
            namespace F(%N);
            pol constant X(i) { match i {
                0 => 7,
                3 => 9,
                5 => 2,
                _ => 4,
            } + 1 };
        "#;
        let analyzed = analyze_string(src);
        assert_eq!(analyzed.degree(), 8);
        let constants = generate(&analyzed);
        assert_eq!(
            constants,
            vec![("F.X", convert(vec![8, 5, 5, 10, 5, 3, 5, 5]))]
        );
    }

    #[test]
    pub fn test_if() {
        let src = r#"
            constant %N = 8;
            namespace F(%N);
            let X = |i| if i < 3 { 7 } else { 9 };
        "#;
        let analyzed = analyze_string(src);
        assert_eq!(analyzed.degree(), 8);
        let constants = generate(&analyzed);
        assert_eq!(
            constants,
            vec![("F.X", convert(vec![7, 7, 7, 9, 9, 9, 9, 9]))]
        );
    }

    #[test]
    pub fn test_macro() {
        let src = r#"
            constant %N = 8;
            namespace F(%N);
            let minus_one = [|x| x - 1][0];
            pol constant EVEN(i) { 2 * minus_one(i) + 2 };
        "#;
        let analyzed = analyze_string(src);
        assert_eq!(analyzed.degree(), 8);
        let constants = generate(&analyzed);
        assert_eq!(
            constants,
            vec![("F.EVEN", convert(vec![0, 2, 4, 6, 8, 10, 12, 14]))]
        );
    }

    #[test]
    pub fn test_poly_call() {
        let src = r#"
            constant %N = 10;
            namespace std::convert(%N);
            let int = [];
            namespace F(%N);
            col fixed seq(i) { i };
            col fixed doub(i) { std::convert::int(seq((2 * i) % %N)) + 1 };
            col fixed half_nibble(i) { i & 0x7 };
            col fixed doubled_half_nibble(i) { half_nibble(i / 2) };
        "#;
        let analyzed = analyze_string(src);
        assert_eq!(analyzed.degree(), 10);
        let constants = generate(&analyzed);
        assert_eq!(constants.len(), 4);
        assert_eq!(
            constants[0],
            ("F.seq", convert((0..=9i32).collect::<Vec<_>>()))
        );
        assert_eq!(
            constants[1],
            (
                "F.doub",
                convert([1i32, 3, 5, 7, 9, 1, 3, 5, 7, 9].to_vec())
            )
        );
        assert_eq!(
            constants[2],
            (
                "F.half_nibble",
                convert([0i32, 1, 2, 3, 4, 5, 6, 7, 0, 1].to_vec())
            )
        );
        assert_eq!(
            constants[3],
            (
                "F.doubled_half_nibble",
                convert([0i32, 0, 1, 1, 2, 2, 3, 3, 4, 4].to_vec())
            )
        );
    }

    #[test]
    pub fn test_arrays() {
        let src = r#"
            constant %N = 10;
            namespace F(%N);
            col fixed alt = [0, 1, 0, 1, 0, 1] + [0]*;
            col fixed empty = [] + [0]*;
            col fixed ref_other = [%N-1, alt(1), 8] + [0]*;
        "#;
        let analyzed = analyze_string(src);
        assert_eq!(analyzed.degree(), 10);
        let constants = generate(&analyzed);
        assert_eq!(constants.len(), 3);
        assert_eq!(
            constants[0],
            ("F.alt", convert([0i32, 1, 0, 1, 0, 1, 0, 0, 0, 0].to_vec()))
        );
        assert_eq!(constants[1], ("F.empty", convert([0i32; 10].to_vec())));
        assert_eq!(
            constants[2],
            (
                "F.ref_other",
                convert([9i32, 1, 8, 0, 0, 0, 0, 0, 0, 0].to_vec())
            )
        );
    }

    #[test]
    pub fn repetition_front() {
        let src = r#"
            constant %N = 10;
            namespace F(%N);
            col fixed arr = [0, 1, 2]* + [7];
        "#;
        let analyzed = analyze_string(src);
        assert_eq!(analyzed.degree(), 10);
        let constants = generate(&analyzed);
        assert_eq!(constants.len(), 1);
        assert_eq!(
            constants[0],
            ("F.arr", convert([0i32, 1, 2, 0, 1, 2, 0, 1, 2, 7].to_vec()))
        );
    }

    #[test]
    pub fn comparisons() {
        let src = r#"
            constant %N = 6;
            namespace std::convert(%N);
            let int = 9;
            let fe = 8;
            namespace F(%N);
            col fixed id(i) { i };
            col fixed inv(i) { %N - i };
            col fixed a = [0, 1, 0, 1, 2, 1];
            col fixed b = [0, 0, 1, 1, 0, 5];
            col fixed or(i) { if (a(i) != std::convert::fe(0)) || (b(i) != std::convert::fe(0)) { 1 } else { 0 } };
            col fixed and(i) { if (a(i) != std::convert::fe(0)) && (b(i) != std::convert::fe(0)) { 1 } else { 0 } };
            col fixed not(i) { if !(a(i) != std::convert::fe(0)) { 1 } else { 0 } };
            col fixed less(i) { if std::convert::int(id(i)) < std::convert::int(inv(i)) { 1 } else { 0 } };
            col fixed less_eq(i) { if std::convert::int(id(i)) <= std::convert::int(inv(i)) { 1 } else { 0 } };
            col fixed eq(i) { if id(i) == inv(i) { 1 } else { 0 } };
            col fixed not_eq(i) { if id(i) != inv(i) { 1 } else { 0 } };
            col fixed greater(i) { if std::convert::int(id(i)) > std::convert::int(inv(i)) { 1 } else { 0 } };
            col fixed greater_eq(i) { if std::convert::int(id(i)) >= std::convert::int(inv(i)) { 1 } else { 0 } };
        "#;
        let analyzed = analyze_string(src);
        assert_eq!(analyzed.degree(), 6);
        let constants = generate(&analyzed);
        assert_eq!(constants[0], ("F.id", convert([0, 1, 2, 3, 4, 5].to_vec())));
        assert_eq!(
            constants[1],
            ("F.inv", convert([6, 5, 4, 3, 2, 1].to_vec()))
        );
        assert_eq!(constants[4], ("F.or", convert([0, 1, 1, 1, 1, 1].to_vec())));
        assert_eq!(
            constants[5],
            ("F.and", convert([0, 0, 0, 1, 0, 1].to_vec()))
        );
        assert_eq!(
            constants[6],
            ("F.not", convert([1, 0, 1, 0, 0, 0].to_vec()))
        );
        assert_eq!(
            constants[7],
            ("F.less", convert([1, 1, 1, 0, 0, 0].to_vec()))
        );
        assert_eq!(
            constants[8],
            ("F.less_eq", convert([1, 1, 1, 1, 0, 0].to_vec()))
        );
        assert_eq!(constants[9], ("F.eq", convert([0, 0, 0, 1, 0, 0].to_vec())));
        assert_eq!(
            constants[10],
            ("F.not_eq", convert([1, 1, 1, 0, 1, 1].to_vec()))
        );
        assert_eq!(
            constants[11],
            ("F.greater", convert([0, 0, 0, 0, 1, 1].to_vec()))
        );
        assert_eq!(
            constants[12],
            ("F.greater_eq", convert([0, 0, 0, 1, 1, 1].to_vec()))
        );
    }

    #[test]
    #[should_panic = "Cannot evaluate witness columns"]
    pub fn calling_witness() {
        let src = r#"
            constant %N = 10;
            namespace F(%N);
            let w;
            let x = |i| w(i) + 1;
        "#;
        let analyzed = analyze_string::<GoldilocksField>(src);
        assert_eq!(analyzed.degree(), 10);
        generate(&analyzed);
    }

    #[test]
    #[should_panic = "Symbol not found: w"]
    pub fn symbol_not_found() {
        let src = r#"
            constant %N = 10;
            namespace F(%N);
            let x = |i| w(i) + 1;
        "#;
        let analyzed = analyze_string::<GoldilocksField>(src);
        assert_eq!(analyzed.degree(), 10);
        generate(&analyzed);
    }

    #[test]
    #[should_panic = "Cannot evaluate arrays"]
    pub fn forward_reference_to_array() {
        let src = r#"
            constant %N = 10;
            namespace F(%N);
            let x = |i| y(i) + 1;
            col fixed y = [1, 2, 3]*;
        "#;
        let analyzed = analyze_string::<GoldilocksField>(src);
        assert_eq!(analyzed.degree(), 10);
        generate(&analyzed);
    }

    #[test]
    pub fn forward_reference_to_function() {
        let src = r#"
            constant %N = 4;
            namespace F(%N);
            let x = |i| y(i) + 1;
            let y = |i| i + 20;
        "#;
        let analyzed = analyze_string::<GoldilocksField>(src);
        assert_eq!(analyzed.degree(), 4);
        let constants = generate(&analyzed);
        assert_eq!(constants[0], ("F.x", convert([21, 22, 23, 24].to_vec())));
        assert_eq!(constants[1], ("F.y", convert([20, 21, 22, 23].to_vec())));
    }

    #[test]
    pub fn bigint_arith() {
        let src = r#"
            constant %N = 4;
            namespace std::convert(%N);
            let int = [];
            let fe = [];
            namespace F(%N);
            let x = |i| std::convert::fe((std::convert::int(1) << (2000 + i)) >> 2000);
        "#;
        let analyzed = analyze_string::<GoldilocksField>(src);
        assert_eq!(analyzed.degree(), 4);
        let constants = generate(&analyzed);
        assert_eq!(constants[0], ("F.x", convert([1, 2, 4, 8].to_vec())));
    }

    #[test]
    pub fn modulo_negative() {
        let src = r#"
            constant %N = 4;
            namespace std::convert(%N);
            let int = [];
            let fe = [];
            namespace F(%N);
            let x_arr = [ 3 % 4, (-3) % 4, 3 % (-4), (-3) % (-4)];
            let x = |i| 100 + x_arr[i];
        "#;
        let analyzed = analyze_string::<GoldilocksField>(src);
        assert_eq!(analyzed.degree(), 4);
        let constants = generate(&analyzed);
        // Semantics of p % q involving negative numbers:
        // sgn(p) * |p| % |q|
        assert_eq!(constants[0], ("F.x", convert([103, 97, 103, 97].to_vec())));
    }
}
