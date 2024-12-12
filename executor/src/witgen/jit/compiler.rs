use std::{mem, sync::Arc};

use powdr_jit_compiler::compiler::call_cargo;
use powdr_number::FieldElement;

use super::{affine_symbolic_expression::Effect, cell::Cell};

pub fn compile_effects<T: FieldElement>(
    first_column_id: u64,
    column_count: usize,
    effects: &[Effect<T, Cell>],
) -> Result<(), String> {
    let code = util_code::<T>(first_column_id, column_count)?;
    let lib_path = powdr_jit_compiler::compiler::call_cargo(&code)
        .map_err(|e| format!("Failed to compile generated code: {e}"))?;

    let library = Arc::new(unsafe { libloading::Library::new(&lib_path.path).unwrap() });
    // // TODO what happen if there is a conflict in function names? Should we
    // // encode the ID and the known inputs?
    // let witgen_fun = unsafe {
    //     library.get::<extern "C" fn(WitgenFunctionParams, *mut c_void, *const c_void)>(b"witgen")
    // }
    // .unwrap();

    // self.witgen_functions.write().unwrap().insert(
    //     key,
    //     (
    //         WitgenFunction {
    //             library: library.clone(),
    //             witgen_function: *witgen_fun,
    //         },
    //         known_after,
    //     ),
    // );
    // true
    Ok(())
}

/// Returns the rust code containing utility functions given a first column id and a column count
/// that is used to store the column table.
fn util_code<T: FieldElement>(first_column_id: u64, column_count: usize) -> Result<String, String> {
    if !(T::has_direct_repr() && (mem::size_of::<T>() == 8 || mem::size_of::<T>() == 4)) {
        return Err(format!(
            "Field {}not supported",
            T::known_field()
                .map(|f| format!("{f} "))
                .unwrap_or_default()
        ));
    }

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

    Ok(format!(
        r#"#![allow(non_snake_case)]
use std::ffi::c_void;

#[derive(Clone, Copy, Default)]
#[repr(transparent)]
struct FieldElement({int_type});

type IntType = {int_type};
type DoubleIntType = {double_int_type};
const MODULUS: IntType = {modulus}_{int_type};

impl std::fmt::Display for FieldElement {{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
        write!(f, "{{}}", self.0)
    }}
}}
impl From<IntType> for FieldElement {{
    #[inline]
    fn from(i: IntType) -> Self {{
        Self(i)
    }}
}}
impl std::ops::Add for FieldElement {{
    type Output = Self;
    #[inline]
    fn add(self, b: Self) -> Self {{
        // TODO this is inefficient.
        Self(IntType::try_from((DoubleIntType::from(self.0) + DoubleIntType::from(b.0)) % DoubleIntType::from(MODULUS)).unwrap())
    }}
}}
impl std::ops::Sub for FieldElement {{
    type Output = Self;
    #[inline]
    fn sub(self, b: Self) -> Self {{
        // TODO this is inefficient.
        Self(IntType::try_from((DoubleIntType::from(self.0) + DoubleIntType::from(MODULUS) - DoubleIntType::from(b.0)) % DoubleIntType::from(MODULUS)).unwrap())
    }}
}}
impl std::ops::Mul<FieldElement> for FieldElement {{
    type Output = Self;
    #[inline]
    fn mul(self, b: FieldElement) -> FieldElement {{
        Self(IntType::try_from((DoubleIntType::from(self.0) * b.0 as DoubleIntType) % DoubleIntType::from(MODULUS)).unwrap())
    }}
}}
impl std::ops::Div<FieldElement> for FieldElement {{
    type Output = Self;
    #[inline]
    fn div(self, _b: FieldElement) -> FieldElement {{
        // TODO we could generate the algorithm we use for goldilocks
        // for a generic prime field.
        todo!()
    }}
}}
#[inline]
fn integer_div(a: FieldElement, b: FieldElement) -> FieldElement {{
    FieldElement(a.0 / b.0)
}}
impl std::ops::BitAnd<FieldElement> for FieldElement {{
    type Output = Self;
    #[inline]
    fn bitand(self, b: FieldElement) -> FieldElement {{
        Self(self.0 & b.0)
    }}
}}
impl std::ops::BitOr<FieldElement> for FieldElement {{
    type Output = Self;
    #[inline]
    fn bitor(self, b: FieldElement) -> FieldElement {{
        Self(self.0 | b.0)
    }}
}}

#[inline]
fn index(global_offset: u64, local_offset: i32, column: u64) -> usize {{
    let column = column - {first_column_id};
    let row = (global_offset as i64 + local_offset as i64) as u64;
    (row * {column_count} + column) as usize
}}

#[inline]
fn index_known(global_offset: u64, local_offset: i32, column: u64) -> (u64, u64) {{
    let column = column - {first_column_id};
    let row = (global_offset as i64 + local_offset as i64) as u64;
    let words_per_row = ({column_count} + 31) / 32;
    (row * words_per_row + column / 32, column % 32)
}}

#[inline]
fn get(data: &[FieldElement], global_offset: u64, local_offset: i32, column: u64) -> FieldElement {{
    data[index(global_offset, local_offset, column)]
}}

#[inline]
fn set(data: &mut [FieldElement], global_offset: u64, local_offset: i32, column: u64, value: FieldElement) {{
    let i = index(global_offset, local_offset, column);
    data[i] = value;
}}

#[inline]
fn set_known(known: &mut [u32], global_offset: u64, local_offset: i32, column: u64) {{
    let (known_idx, known_bit) = index_known(global_offset, local_offset, column);
    known[known_idx as usize] |= 1 << (known_bit);
}}

#[repr(C)]
enum LookupCell<'a, T> {{
    /// Value is known (i.e. an input)
    Input(&'a T),
    /// Value is not known (i.e. an output)
    Output(&'a mut T),
}}

#[repr(C)]
struct WitgenFunctionParams {{
    data: *mut FieldElement,
    known: *mut u32,
    len: u64,
    row_offset: u64,
}}
    "#
    ))
}

#[cfg(test)]
mod tests {
    use powdr_number::GoldilocksField;

    use super::*;

    #[test]
    fn compile_util_code() {
        if let Err(e) = compile_effects::<GoldilocksField>(0, 2, &[]) {
            println!("{e}");
            panic!()
        }
    }
}
