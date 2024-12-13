use std::{iter, mem, sync::Arc};

use auto_enums::auto_enum;
use itertools::Itertools;
use libloading::Library;
use powdr_number::FieldElement;

use crate::witgen::{jit::affine_symbolic_expression::MachineCallArgument, machines::LookupCell};

use super::{
    affine_symbolic_expression::{Assertion, Effect},
    cell::Cell,
    symbolic_expression::{BinaryOperator, SymbolicExpression, UnaryOperator},
};

#[repr(C)]
pub struct WitgenFunctionParams<T> {
    data: *mut T,
    known: *mut u32,
    len: u64,
    row_offset: u64,
    call_machine: extern "C" fn(u64, *mut LookupCell<'_, T>, usize) -> bool,
}

pub type WitgenFunction<T> = extern "C" fn(WitgenFunctionParams<T>);

pub fn compile_effects<T: FieldElement>(
    first_column_id: u64,
    column_count: usize,
    known_inputs: &[Cell],
    effects: &[Effect<T, Cell>],
) -> Result<(Arc<Library>, WitgenFunction<T>), String> {
    let utils = util_code::<T>(first_column_id, column_count)?;
    let witgen_code = witgen_code(known_inputs, effects);
    let code = format!("{utils}\n//-------------------------------\n{witgen_code}");

    let lib_path = powdr_jit_compiler::call_cargo(&code)
        .map_err(|e| format!("Failed to compile generated code: {e}"))?;

    let library = Arc::new(unsafe { libloading::Library::new(&lib_path.path).unwrap() });
    let witgen_fun = unsafe { library.get::<WitgenFunction<T>>(b"witgen\0") }.unwrap();

    Ok((library.clone(), *witgen_fun))
}

fn witgen_code<T: FieldElement>(known_inputs: &[Cell], effects: &[Effect<T, Cell>]) -> String {
    let load_known_cells = known_inputs
        .iter()
        .map(|c| {
            format!(
                "    let {} = get(data, row_offset, {}, {});",
                cell_to_var_name(c),
                c.row_offset,
                c.id
            )
        })
        .format("\n");
    let main_code = effects.iter().map(format_effect).format("\n");
    let cells_known = effects
        .iter()
        .flat_map(written_cells_in_effect)
        .collect_vec();
    let store_values = cells_known
        .iter()
        .map(|cell| {
            format!(
                "    set(data, row_offset, {}, {}, {});",
                cell.row_offset,
                cell.id,
                cell_to_var_name(cell)
            )
        })
        .format("\n");
    let store_known = cells_known
        .iter()
        .map(|cell| {
            format!(
                "    set_known(known, row_offset, {}, {});",
                cell.row_offset, cell.id
            )
        })
        .format("\n");
    format!(
        r#"
#[no_mangle]
extern "C" fn witgen(
    WitgenFunctionParams{{
        data,
        known,
        len,
        row_offset,
        call_machine
    }}: WitgenFunctionParams,
) {{
    let data = data_to_slice(data, len);
    let known = known_to_slice(known, len);

{load_known_cells}

{main_code}

{store_values}

{store_known}
}}
"#
    )
}

/// Returns an iterator over all cells written to in the effect.
#[auto_enum(Iterator)]
fn written_cells_in_effect<T: FieldElement>(
    effect: &Effect<T, Cell>,
) -> impl Iterator<Item = &Cell> + '_ {
    match effect {
        Effect::Assignment(cell, _) => iter::once(cell),
        Effect::RangeConstraint(..) => unreachable!(),
        Effect::Assertion(..) => iter::empty(),
        Effect::MachineCall(_, arguments) => arguments.iter().flat_map(|e| match e {
            MachineCallArgument::Unknown(e) => Some(e.single_unknown_variable().unwrap()),
            MachineCallArgument::Known(_) => None,
        }),
    }
}

fn format_effect<T: FieldElement>(effect: &Effect<T, Cell>) -> String {
    match effect {
        Effect::Assignment(var, e) => {
            format!(
                "    let {} = {};",
                cell_to_var_name(var),
                format_expression(e)
            )
        }
        Effect::RangeConstraint(..) => {
            unreachable!("Final code should not contain pure range constraints.")
        }
        Effect::Assertion(Assertion {
            lhs,
            rhs,
            expected_equal,
        }) => format!(
            "    assert!({} {} {});",
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
                UnaryOperator::Neg => format!("-{inner}"),
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
    let column_name = column_name
        .rfind("::")
        .map_or(column_name.as_str(), |i| &column_name[i + 2..])
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
    fn div(self, b: FieldElement) -> FieldElement {{
        if b.0 == 0 {{
            panic!("Division by zero");
        }}
        let uint_result = self.0 / b.0;
        if uint_result * b.0 == self.0 {{
            return Self(uint_result);
        }}
        // TODO we could also try signed integer division.
        full_field_div(self, b)
    }}
}}
fn full_field_div(a: FieldElement, b: FieldElement) -> FieldElement {{
    todo!()
    // TODO generate the algorithm we use for goldilocks
    // for a generic prime field.
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
fn data_to_slice<'a>(data: *mut FieldElement, len: u64) -> &'a mut [FieldElement] {{
    unsafe {{ std::slice::from_raw_parts_mut(data, len as usize) }}
}}
#[inline]
fn known_to_slice<'a>(known: *mut u32, len: u64) -> &'a mut [u32] {{
    let words_per_row = ({column_count} + 31) / 32;
    let rows = len / {column_count};
    let known_len = rows * words_per_row;
    unsafe {{ std::slice::from_raw_parts_mut(known, known_len as usize) }}
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
    call_machine: fn(u64, *mut [LookupCell<'_, FieldElement>], usize) -> bool,
}}
    "#
    ))
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use powdr_number::GoldilocksField;

    use super::*;

    #[test]
    fn compile_util_code() {
        if let Err(e) = compile_effects::<GoldilocksField>(0, 2, &[], &[]) {
            println!("{e}");
            panic!()
        }
    }

    fn cell(column_name: &str, id: u64, row_offset: i32) -> Cell {
        Cell {
            column_name: column_name.to_string(),
            row_offset,
            id,
        }
    }

    fn symbol(cell: &Cell) -> SymbolicExpression<GoldilocksField, Cell> {
        SymbolicExpression::from_symbol(cell.clone(), None)
    }

    fn number(n: u64) -> SymbolicExpression<GoldilocksField, Cell> {
        SymbolicExpression::from(GoldilocksField::from(n))
    }

    fn assignment(
        cell: &Cell,
        e: SymbolicExpression<GoldilocksField, Cell>,
    ) -> Effect<GoldilocksField, Cell> {
        Effect::Assignment(cell.clone(), e)
    }

    #[test]
    fn simple_effects() {
        let a0 = cell("a", 2, 0);
        let x0 = cell("x", 0, 0);
        let ym1 = cell("y", 1, -1);
        let yp1 = cell("y", 1, 1);
        let effects = vec![
            assignment(&x0, number(7) * symbol(&a0)),
            assignment(&ym1, symbol(&x0)),
            assignment(&yp1, symbol(&ym1) + symbol(&x0)),
            Effect::Assertion(Assertion {
                lhs: symbol(&ym1),
                rhs: symbol(&x0),
                expected_equal: true,
            }),
        ];
        let known_inputs = vec![a0.clone()];
        let code = witgen_code(&known_inputs, &effects);
        assert_eq!(
            code,
            "
#[no_mangle]
extern \"C\" fn witgen(
    WitgenFunctionParams{
        data,
        known,
        len,
        row_offset,
        call_machine
    }: WitgenFunctionParams,
) {
    let data = data_to_slice(data, len);
    let known = known_to_slice(known, len);

    let a_d0 = get(data, row_offset, 0, 2);

    let x_d0 = (FieldElement::from(7) * a_d0);
    let y_u1 = x_d0;
    let y_d1 = (y_u1 + x_d0);
    assert!(y_u1 == x_d0);

    set(data, row_offset, 0, 0, x_d0);
    set(data, row_offset, -1, 1, y_u1);
    set(data, row_offset, 1, 1, y_d1);

    set_known(known, row_offset, 0, 0);
    set_known(known, row_offset, -1, 1);
    set_known(known, row_offset, 1, 1);
}
"
        );
    }

    extern "C" fn no_call_machine(
        _: u64,
        _: *mut LookupCell<'_, GoldilocksField>,
        _: usize,
    ) -> bool {
        false
    }

    #[test]
    fn load_code() {
        let x = cell("x", 0, 0);
        let y = cell("y", 1, 0);
        let effects = vec![
            assignment(&x, number(7)),
            assignment(&y, symbol(&x) + number(2)),
        ];
        let (_lib, f) = compile_effects(0, 1, &[], &effects).unwrap();
        let mut data = vec![GoldilocksField::from(0); 2];
        let mut known = vec![0; 1];
        let params = WitgenFunctionParams {
            data: data.as_mut_ptr(),
            known: known.as_mut_ptr(),
            len: data.len() as u64,
            row_offset: 0,
            call_machine: no_call_machine,
        };
        f(params);
        assert_eq!(data[0], GoldilocksField::from(7));
        assert_eq!(data[1], GoldilocksField::from(9));
        assert_eq!(known[0], 3);
    }

    #[test]
    fn load_twice() {
        let x = cell("x", 0, 0);
        let effects1 = vec![Effect::Assignment(
            x.clone(),
            GoldilocksField::from(7).into(),
        )];
        let effects2 = vec![Effect::Assignment(
            x.clone(),
            GoldilocksField::from(9).into(),
        )];
        let row_count = 2;
        let column_count = 2;
        let data_len = column_count * row_count;
        let (_lib1, f1) = compile_effects(0, column_count, &[], &effects1).unwrap();
        let (_lib2, f2) = compile_effects(0, column_count, &[], &effects2).unwrap();
        let mut data = vec![GoldilocksField::from(0); data_len];
        let mut known = vec![0; row_count];
        let params1 = WitgenFunctionParams {
            data: data.as_mut_ptr(),
            known: known.as_mut_ptr(),
            len: data_len as u64,
            row_offset: 0,
            call_machine: no_call_machine,
        };
        f1(params1);
        assert_eq!(data[0], GoldilocksField::from(7));
        assert_eq!(data[1], GoldilocksField::from(0));
        assert_eq!(data[2], GoldilocksField::from(0));
        assert_eq!(data[3], GoldilocksField::from(0));
        assert_eq!(known, vec![1, 0]);
        let params2 = WitgenFunctionParams {
            data: data.as_mut_ptr(),
            known: known.as_mut_ptr(),
            len: data_len as u64,
            row_offset: 1,
            call_machine: no_call_machine,
        };
        f2(params2);
        assert_eq!(data[0], GoldilocksField::from(7));
        assert_eq!(data[1], GoldilocksField::from(0));
        assert_eq!(data[2], GoldilocksField::from(9));
        assert_eq!(data[3], GoldilocksField::from(0));
        assert_eq!(known, vec![1, 1]);
    }
}
