use std::collections::BTreeMap;

use crate::parsed::visitor::AllChildren;

use super::{AlgebraicExpression, AlgebraicReferenceThin, PolynomialType};

/// Auxiliary function to check if an AST node contains a next reference
/// References to intermediate values are resolved recursively
fn contains_next_ref_with_intermediates<T, E: AllChildren<AlgebraicExpression<T>>>(
    e: &E,
    intermediate_definitions: &BTreeMap<AlgebraicReferenceThin, AlgebraicExpression<T>>,
    intermediates_cache: &mut BTreeMap<AlgebraicReferenceThin, bool>,
) -> bool {
    e.all_children()
        .filter_map(|e| {
            if let AlgebraicExpression::Reference(reference) = e {
                Some(reference)
            } else {
                None
            }
        })
        .any(|reference| {
            if reference.next {
                true
            } else if reference.poly_id.ptype == PolynomialType::Intermediate {
                let reference = reference.to_thin();
                intermediates_cache
                    .get(&reference)
                    .cloned()
                    .unwrap_or_else(|| {
                        let result = contains_next_ref_with_intermediates(
                            &intermediate_definitions[&reference],
                            intermediate_definitions,
                            intermediates_cache,
                        );
                        intermediates_cache.insert(reference, result);
                        result
                    })
            } else {
                false
            }
        })
}

pub trait ContainsNextRef<T> {
    fn contains_next_ref(
        &self,
        intermediate_definitions: &BTreeMap<AlgebraicReferenceThin, AlgebraicExpression<T>>,
    ) -> bool;
}

impl<T, E: AllChildren<AlgebraicExpression<T>>> ContainsNextRef<T> for E {
    fn contains_next_ref(
        &self,
        intermediate_definitions: &BTreeMap<AlgebraicReferenceThin, AlgebraicExpression<T>>,
    ) -> bool {
        contains_next_ref_with_intermediates(self, intermediate_definitions, &mut BTreeMap::new())
    }
}

#[cfg(test)]
mod tests {
    use std::iter::once;

    use crate::analyzed::{
        contains_next_ref::ContainsNextRef, AlgebraicExpression, AlgebraicReference, PolyID,
        PolynomialType,
    };

    #[test]
    fn contains_next_ref() {
        let column = AlgebraicExpression::<i32>::Reference(AlgebraicReference {
            name: "column".to_string(),
            poly_id: PolyID {
                id: 0,
                ptype: PolynomialType::Committed,
            },
            next: false,
        });

        let one = AlgebraicExpression::Number(1);

        let expr = column.clone() + one.clone() * column.clone();
        assert!(!expr.contains_next_ref(&Default::default()));

        let expr = column.clone() + one.clone() * column.clone().next().unwrap();
        assert!(expr.contains_next_ref(&Default::default()));

        let inter = AlgebraicReference {
            name: "inter".to_string(),
            poly_id: PolyID {
                id: 1,
                ptype: PolynomialType::Intermediate,
            },
            next: false,
        };
        let intermediates = once((inter.to_thin(), column.clone().next().unwrap())).collect();
        let expr = column.clone() + one.clone() * AlgebraicExpression::Reference(inter.clone());
        assert!(expr.contains_next_ref(&intermediates));
    }
}
