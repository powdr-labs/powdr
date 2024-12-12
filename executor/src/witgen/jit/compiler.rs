use std::{mem, sync::Arc};

use itertools::Itertools;
use powdr_jit_compiler::compiler::call_cargo;
use powdr_number::FieldElement;

use super::{
    affine_symbolic_expression::{Assertion, Effect},
    cell::Cell,
    symbolic_expression::{BinaryOperator, SymbolicExpression, UnaryOperator},
};

pub fn compile_effects<T: FieldElement>(
    first_column_id: u64,
    column_count: usize,
    known_inputs: &[Cell],
    effects: &[Effect<T, Cell>],
) -> Result<(), String> {
    let utils = util_code::<T>(first_column_id, column_count)?;
    let witgen_code = witgen_code(known_inputs, effects);
    let code = format!("{utils}\n//-------------------------------\n{witgen_code}");

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

fn witgen_code<T: FieldElement>(known_inputs: &[Cell], effects: &[Effect<T, Cell>]) -> String {
    // TODO indent
    let fun_name = "witgen";
    let assign_inputs = known_inputs
        .iter()
        .map(|c| {
            format!(
                "let {} = get(data, row_offset, {}, {});",
                cell_to_var_name(c),
                c.row_offset,
                c.id
            )
        })
        .format("\n");
    let main_code = effects.iter().map(format_effect).format("\n");
    let store_values = ""; // TODO
    format!(
        r#"
#[no_mangle]
extern "C" fn {fun_name}(
    WitgenFunctionParams{{
        data,
        known,
        len,
        row_offset,
    }}: WitgenFunctionParams,
    _mutable_state: *const c_void,
    _call_machine: fn(*const c_void, u64, &mut [LookupCell<'_, FieldElement>]) -> bool
) {{
    let data: &mut [FieldElement] = unsafe {{ std::slice::from_raw_parts_mut(data as *mut FieldElement, len as usize) }};
    {assign_inputs}
    {main_code}
    {store_values}
    // TODO store_known
    }}
"#
    )
}

fn format_effect<T: FieldElement>(effect: &Effect<T, Cell>) -> String {
    match effect {
        Effect::Assignment(var, e) => {
            format!("let {} = {};", cell_to_var_name(var), format_expression(e))
        }
        Effect::RangeConstraint(..) => {
            unreachable!("Final code should not contain pure range constraints.")
        }
        Effect::Assertion(Assertion {
            lhs,
            rhs,
            expected_equal,
        }) => format!(
            "assert!({} {} {});",
            format_expression(lhs),
            if *expected_equal { "==" } else { "!=" },
            format_expression(rhs)
        ),
        Effect::MachineCall(id, arguments) => todo!(),
    }
}

fn format_expression<T: FieldElement>(e: &SymbolicExpression<T, Cell>) -> String {
    match e {
        SymbolicExpression::Concrete(v) => format!("FieldElement::from({v})"),
        SymbolicExpression::Symbol(cell, _) => cell_to_var_name(cell),
        SymbolicExpression::BinaryOperation(left, op, right, _) => {
            let left = format_expression(left);
            let right = format_expression(right);
            match op {
                BinaryOperator::Add => format!("({left} + {right})"),
                BinaryOperator::Sub => format!("({left} - {right})"),
                BinaryOperator::Mul => format!("({left} * {right})"),
                BinaryOperator::Div => format!("({left} / {right})"),
                BinaryOperator::IntegerDiv => format!("integer_div({left}, {right})"),
                BinaryOperator::BitAnd => format!("({left} & {right})"),
                BinaryOperator::BitOr => format!("({left} | {right})"),
            }
        }
        SymbolicExpression::UnaryOperation(op, inner, _) => {
            let inner = format_expression(inner);
            match op {
                UnaryOperator::Neg => format!("-{}", inner),
            }
        }
    }
}

fn cell_to_var_name(
    Cell {
        column_name,
        row_offset,
        ..
    }: &Cell,
) -> String {
    // TODO this might still not be unique.
    let column_name = &column_name[column_name.rfind("::").unwrap() + 2..]
        .replace("[", "_")
        .replace("]", "_");
    if *row_offset < 0 {
        format!("{column_name}_u{}", -row_offset)
    } else {
        format!("{column_name}_d{row_offset}")
    }
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
        if let Err(e) = compile_effects::<GoldilocksField>(0, 2, &[], &[]) {
            println!("{e}");
            panic!()
        }
    }
}
