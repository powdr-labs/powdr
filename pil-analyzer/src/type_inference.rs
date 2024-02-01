use std::collections::HashMap;

use powdr_ast::analyzed::{types::Type, FunctionValueDefinition, Symbol};
use powdr_number::FieldElement;

pub fn infer_types<T: FieldElement>(
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
) -> Result<HashMap<String, Type>, String> {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    use powdr_number::GoldilocksField;

    use crate::analyze_string;

    fn parse_and_type_check(input: &str) -> Result<HashMap<String, Type>, String> {
        let analyzed = analyze_string::<GoldilocksField>(input);
        infer_types(&analyzed.definitions)
    }

    fn check(types: &Result<HashMap<String, Type>, String>, expected: &[(&str, &str)]) {
        let types = types.as_ref().unwrap();
        for (name, ty) in expected {
            assert_eq!(types[&name.to_string()].to_string(), *ty);
        }
    }

    // test a simple assignment.
    #[test]
    fn assignment() {
        let input = "let x: int -> int = |i| i; let y = x(2);";
        let result = parse_and_type_check(input);
        check(&result, &[("x", "int -> int"), ("y", "int")]);
    }
}
