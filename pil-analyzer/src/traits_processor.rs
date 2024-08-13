use core::panic;
use std::collections::{BTreeMap, HashMap};

use powdr_ast::{
    analyzed::{Expression, FunctionValueDefinition, PolynomialReference, Reference},
    parsed::{types::Type, TraitImplementation},
};

pub struct TraitsProcessor<'a> {
    implementations: &'a HashMap<String, Vec<TraitImplementation<Expression>>>,
}

impl<'a> TraitsProcessor<'a> {
    pub fn new(implementations: &'a HashMap<String, Vec<TraitImplementation<Expression>>>) -> Self {
        Self { implementations }
    }

    pub fn traits_resolution(
        &self,
        references: &mut [&mut Reference],
        traits_functions_defs: HashMap<String, FunctionValueDefinition>,
    ) {
        for r in references.iter_mut() {
            let Reference::Poly(PolynomialReference {
                name,
                type_args: Some(types),
                ..
            }) = r
            else {
                continue;
            };

            let Some((trait_name, resolved_impl_pos)) =
                self.resolve_trait_function((name.to_string(), types), &traits_functions_defs)
            else {
                continue;
            };

            update_reference(&trait_name, r, &resolved_impl_pos);
        }
    }

    fn resolve_trait_function(
        &self,
        collected_ref: (String, &Vec<Type>),
        traits_functions_definition: &HashMap<String, FunctionValueDefinition>,
    ) -> Option<(String, BTreeMap<Vec<Type>, usize>)> {
        let mut resolved_impl_pos = BTreeMap::new();
        let Some(FunctionValueDefinition::TraitFunction(ref trait_decl, ref trait_fn)) =
            traits_functions_definition.get(&collected_ref.0)
        else {
            return None;
        };

        if let Some(impls) = self.implementations.get(&trait_decl.name) {
            for (index, impl_) in impls.iter().enumerate() {
                let Some(impl_fn) = impl_.function_by_name(&trait_fn.name) else {
                    panic!(
                        "Could not find function {} for {}",
                        trait_fn.name, trait_decl.name
                    );
                };

                resolved_impl_pos.insert(collected_ref.1.clone(), index);
            }
        }

        Some((
            format!("{}::{}", trait_decl.name.replace('.', "::"), trait_fn.name),
            resolved_impl_pos,
        ))
    }
}

fn update_reference(
    ref_name: &str,
    c: &mut Reference,
    resolved_impl_pos: &BTreeMap<Vec<Type>, usize>,
) {
    if let Reference::Poly(PolynomialReference {
        name,
        poly_id,
        type_args: current_type_args,
        ..
    }) = c
    {
        if name == ref_name {
            *c = Reference::Poly(PolynomialReference {
                name: name.clone(),
                type_args: current_type_args.clone(),
                poly_id: *poly_id,
                resolved_impls: resolved_impl_pos.clone(),
            });
        }
    }
}
