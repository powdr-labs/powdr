use std::mem;

use powdr_number::{FieldElement, KnownField};

/// Returns the rust code containing utility functions for PIL types.
pub fn util_code<T: FieldElement>() -> Result<String, String> {
    if !(T::has_direct_repr() && (mem::size_of::<T>() == 8 || mem::size_of::<T>() == 4)) {
        return Err(format!(
            "Field {}not supported",
            T::known_field()
                .map(|f| format!("{f} "))
                .unwrap_or_default()
        ));
    }

    let field_impl = match T::known_field() {
        Some(KnownField::GoldilocksField) => {
            include_str!("includes/field_goldilocks.rs").to_string()
        }
        _ => {
            let int_type = if mem::size_of::<T>() == 8 {
                "u64"
            } else {
                "u32"
            };
            let double_int_type = if mem::size_of::<T>() == 8 {
                "u128"
            } else {
                "u64"
            };
            let modulus = T::modulus();

            format!(
                "\
                #[derive(Clone, Copy, Default)]\n\
                #[repr(transparent)]\n\
                struct FieldElement({int_type});\n\
                \n\
                type IntType = {int_type};\n\
                type DoubleIntType = {double_int_type};\n\
                const MODULUS: IntType = {modulus}_{int_type};\n\
                {}\
                ",
                include_str!("includes/field_generic_up_to_64.rs")
            )
        }
    };

    let types = include_str!("includes/types.rs");

    Ok(format!(
        "#![allow(non_snake_case, unused_parens, unused_variables)]\n{types}\n{field_impl}\n"
    ))
}
