use std::{iter, mem, sync::Arc};

use auto_enums::auto_enum;
use itertools::Itertools;
use libloading::Library;
use powdr_number::FieldElement;

use crate::witgen::{jit::affine_symbolic_expression::MachineCallArgument, machines::LookupCell};

use super::{
    affine_symbolic_expression::{Assertion, Effect},
    symbolic_expression::{BinaryOperator, SymbolicExpression, UnaryOperator},
    variable::{Cell, Variable},
};

// TODO We might want to pass arguments as direct function parameters
// (instead of a struct), so that
// they are stored in registers instead of the stack. Should be checked.

#[repr(C)]
pub struct MutSlice<T> {
    data: *mut T,
    len: u64,
}

impl<T> Default for MutSlice<T> {
    fn default() -> Self {
        MutSlice {
            data: std::ptr::null_mut(),
            len: 0,
        }
    }
}

impl<T> From<&mut [T]> for MutSlice<T> {
    fn from(slice: &mut [T]) -> Self {
        MutSlice {
            data: slice.as_mut_ptr(),
            len: slice.len() as u64,
        }
    }
}

impl<T> Default for ConstSlice<T> {
    fn default() -> Self {
        ConstSlice {
            data: std::ptr::null(),
            len: 0,
        }
    }
}

#[repr(C)]
pub struct ConstSlice<T> {
    data: *const T,
    len: u64,
}

impl<T> From<&[T]> for ConstSlice<T> {
    fn from(slice: &[T]) -> Self {
        ConstSlice {
            data: slice.as_ptr(),
            len: slice.len() as u64,
        }
    }
}

#[repr(C)]
pub struct WitgenFunctionParams<T> {
    data: MutSlice<T>,
    known: *mut u32,
    row_offset: u64,
    // TODO do we really want to reshuffle?
    input_params: MutSlice<*mut T>,
    output_params: ConstSlice<*const T>,
    call_machine: extern "C" fn(u64, MutSlice<LookupCell<'_, T>>) -> bool,
}

pub type WitgenFunction<T> = extern "C" fn(WitgenFunctionParams<T>);

pub fn compile_effects<T: FieldElement>(
    first_column_id: u64,
    column_count: usize,
    known_inputs: &[Variable],
    //    params: &[LookupCell<'_, T>],
    effects: &[Effect<T, Variable>],
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

fn witgen_code<T: FieldElement>(
    known_inputs: &[Variable],
    effects: &[Effect<T, Variable>],
) -> String {
    let load_known_inputs = known_inputs
        .iter()
        .map(|v| {
            format!(
                "    let {} = {};",
                variable_to_string(v),
                match v {
                    Variable::Cell(c) => {
                        format!("get(data, row_offset, {}, {})", c.row_offset, c.id)
                    }
                    Variable::Param(i) => format!("params[{i}]",),
                }
            )
        })
        .format("\n");
    let main_code = effects.iter().map(format_effect).format("\n");
    let vars_known = effects
        .iter()
        .flat_map(written_vars_in_effect)
        .collect_vec();
    let store_values = vars_known
        .iter()
        .map(|var| {
            let value = variable_to_string(var);
            match var {
                Variable::Cell(cell) => {
                    format!(
                        "    set(data, row_offset, {}, {}, {value});",
                        cell.row_offset, cell.id,
                    )
                }
                Variable::Param(i) => {
                    format!("    params[{i}] = {value};")
                }
            }
        })
        .format("\n");
    // We do not store "known" together with the values, because we hope
    // that this way, the optimizer can group them better.
    let store_known = vars_known
        .iter()
        .filter_map(|var| match var {
            Variable::Cell(cell) => Some(cell),
            Variable::Param(_) => None,
        })
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
        row_offset,
        input_params,
        output_params,
        call_machine
    }}: WitgenFunctionParams<FieldElement>,
) {{
    let known = known_to_slice(known, data.len);
    let data = data_to_slice(data.data, data.len);

{load_known_inputs}

{main_code}

{store_values}

{store_known}
}}
"#
    )
}

/// Returns an iterator over all variables written to in the effect.
#[auto_enum(Iterator)]
fn written_vars_in_effect<T: FieldElement>(
    effect: &Effect<T, Variable>,
) -> impl Iterator<Item = &Variable> + '_ {
    match effect {
        Effect::Assignment(var, _) => iter::once(var),
        Effect::RangeConstraint(..) => unreachable!(),
        Effect::Assertion(..) => iter::empty(),
        Effect::MachineCall(_, arguments) => arguments.iter().flat_map(|e| match e {
            MachineCallArgument::Unknown(e) => Some(e.single_unknown_variable().unwrap()),
            MachineCallArgument::Known(_) => None,
        }),
    }
}

fn format_effect<T: FieldElement>(effect: &Effect<T, Variable>) -> String {
    match effect {
        Effect::Assignment(var, e) => {
            format!(
                "    let {} = {};",
                variable_to_string(var),
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
        Effect::MachineCall(..) => todo!(),
    }
}

fn format_expression<T: FieldElement>(e: &SymbolicExpression<T, Variable>) -> String {
    match e {
        SymbolicExpression::Concrete(v) => format!("FieldElement::from({v})"),
        SymbolicExpression::Symbol(cell, _) => variable_to_string(cell),
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

/// Returns the name of a local (stack) variable for the given expression variable.
fn variable_to_string(v: &Variable) -> String {
    match v {
        Variable::Cell(cell) => format!(
            "c_{}_{}_{}",
            escape_column_name(&cell.column_name),
            cell.id,
            format_row_offset(cell.row_offset)
        ),
        Variable::Param(i) => format!("p_{i}"),
    }
}

fn escape_column_name(column_name: &str) -> String {
    column_name
        .rfind("::")
        .map_or(column_name, |i| &column_name[i + 2..])
        .replace("[", "_")
        .replace("]", "_")
}

fn format_row_offset(row_offset: i32) -> String {
    if row_offset < 0 {
        format!("m{}", -row_offset)
    } else {
        format!("{}", row_offset)
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
        // TODO this is inefficient.
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

        if let Some(result) = try_integer_div_without_remainder(self.0, b.0) {{
            Self(result)
        }} else if let Some(result) = try_integer_div_without_remainder(self.0, MODULUS - b.0) {{
            Self(MODULUS - result)
        }} else if let Some(result) = try_integer_div_without_remainder(MODULUS - self.0, b.0) {{
            Self(MODULUS - result)
        }} else if let Some(result) = try_integer_div_without_remainder(MODULUS - self.0, MODULUS - b.0) {{
            Self(result)
        }} else {{
            full_field_div(self, b)
        }}
    }}
}}
#[inline]
fn try_integer_div_without_remainder(a: IntType, b: IntType) -> Option<IntType> {{
    (a % b == 0).then(|| a / b)
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
pub struct MutSlice<T> {{
    data: *mut T,
    len: u64,
}}

impl<T> From<&mut [T]> for MutSlice<T> {{
    fn from(slice: &mut [T]) -> Self {{
        MutSlice {{
            data: slice.as_mut_ptr(),
            len: slice.len() as u64,
        }}
    }}
}}

#[repr(C)]
pub struct ConstSlice<T> {{
    data: *const T,
    len: u64,
}}

impl<T> From<&[T]> for ConstSlice<T> {{
    fn from(slice: &[T]) -> Self {{
        ConstSlice {{
            data: slice.as_ptr(),
            len: slice.len() as u64,
        }}
    }}
}}

#[repr(C)]
pub struct WitgenFunctionParams<T> {{
    data: MutSlice<T>,
    known: *mut u32,
    row_offset: u64,
    input_params: MutSlice<*mut T>,
    output_params: ConstSlice<*const T>,
    call_machine: extern "C" fn(u64, MutSlice<LookupCell<'_, T>>) -> bool,
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
        compile_effects::<GoldilocksField>(0, 2, &[], &[]).unwrap();
    }

    fn cell(column_name: &str, id: u64, row_offset: i32) -> Variable {
        Variable::Cell(Cell {
            column_name: column_name.to_string(),
            row_offset,
            id,
        })
    }

    fn symbol(var: &Variable) -> SymbolicExpression<GoldilocksField, Variable> {
        SymbolicExpression::from_symbol(var.clone(), None)
    }

    fn number(n: u64) -> SymbolicExpression<GoldilocksField, Variable> {
        SymbolicExpression::from(GoldilocksField::from(n))
    }

    fn assignment(
        var: &Variable,
        e: SymbolicExpression<GoldilocksField, Variable>,
    ) -> Effect<GoldilocksField, Variable> {
        Effect::Assignment(var.clone(), e)
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
        row_offset,
        input_params,
        output_params,
        call_machine
    }: WitgenFunctionParams<FieldElement>,
) {
    let known = known_to_slice(known, data.len);
    let data = data_to_slice(data.data, data.len);

    let c_a_2_0 = get(data, row_offset, 0, 2);

    let c_x_0_0 = (FieldElement::from(7) * c_a_2_0);
    let c_y_1_m1 = c_x_0_0;
    let c_y_1_1 = (c_y_1_m1 + c_x_0_0);
    assert!(c_y_1_m1 == c_x_0_0);

    set(data, row_offset, 0, 0, c_x_0_0);
    set(data, row_offset, -1, 1, c_y_1_m1);
    set(data, row_offset, 1, 1, c_y_1_1);

    set_known(known, row_offset, 0, 0);
    set_known(known, row_offset, -1, 1);
    set_known(known, row_offset, 1, 1);
}
"
        );
    }

    extern "C" fn no_call_machine(_: u64, _: MutSlice<LookupCell<'_, GoldilocksField>>) -> bool {
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
            data: MutSlice::from(data.as_mut_slice()),
            known: known.as_mut_ptr(),
            row_offset: 0,
            input_params: Default::default(),
            output_params: Default::default(),
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
            data: MutSlice::from(data.as_mut_slice()),
            known: known.as_mut_ptr(),
            row_offset: 0,
            input_params: Default::default(),
            output_params: Default::default(),
            call_machine: no_call_machine,
        };
        f1(params1);
        assert_eq!(data[0], GoldilocksField::from(7));
        assert_eq!(data[1], GoldilocksField::from(0));
        assert_eq!(data[2], GoldilocksField::from(0));
        assert_eq!(data[3], GoldilocksField::from(0));
        assert_eq!(known, vec![1, 0]);
        let params2 = WitgenFunctionParams {
            data: MutSlice::from(data.as_mut_slice()),
            known: known.as_mut_ptr(),
            row_offset: 1,
            input_params: Default::default(),
            output_params: Default::default(),
            call_machine: no_call_machine,
        };
        f2(params2);
        assert_eq!(data[0], GoldilocksField::from(7));
        assert_eq!(data[1], GoldilocksField::from(0));
        assert_eq!(data[2], GoldilocksField::from(9));
        assert_eq!(data[3], GoldilocksField::from(0));
        assert_eq!(known, vec![1, 1]);
    }

    #[test]
    fn field_div_via_integer() {
        let effects = vec![
            assignment(&cell("x", 0, 0), number(8).field_div(&number(2))),
            assignment(&cell("x", 0, 1), number(1 << 60).field_div(&number(8))),
            assignment(&cell("x", 0, 2), (-number(8)).field_div(&number(2))),
            assignment(&cell("x", 0, 3), number(8).field_div(&-number(2))),
            assignment(&cell("x", 0, 4), (-number(8)).field_div(&-number(2))),
        ];
        let (_lib, f) = compile_effects(0, 1, &[], &effects).unwrap();
        let mut data = vec![GoldilocksField::from(0); 5];
        let mut known = vec![0; 1];
        let params = WitgenFunctionParams {
            data: data.as_mut_slice().into(),
            known: known.as_mut_ptr(),
            row_offset: 0,
            input_params: Default::default(),
            output_params: Default::default(),
            call_machine: no_call_machine,
        };
        f(params);
        assert_eq!(data[0], GoldilocksField::from(4));
        assert_eq!(
            data[1],
            GoldilocksField::from(1u64 << 60) / GoldilocksField::from(8)
        );
        assert_eq!(data[2], -GoldilocksField::from(4));
        assert_eq!(data[3], -GoldilocksField::from(4));
        assert_eq!(data[4], GoldilocksField::from(4));
    }

    #[test]
    fn field_multiplication() {
        let x = cell("x", 0, 0);
        let y = cell("y", 1, 0);
        let z = cell("z", 2, 0);
        let effects = vec![assignment(&x, symbol(&y) * symbol(&z))];
        let known_inputs = vec![y.clone(), z.clone()];
        let (_lib, f) = compile_effects(0, 3, &known_inputs, &effects).unwrap();
        let mut data = vec![
            GoldilocksField::from(0),
            GoldilocksField::from(3),
            -GoldilocksField::from(4),
        ];
        let mut known = vec![0; 1];
        let params = WitgenFunctionParams {
            data: data.as_mut_slice().into(),
            known: known.as_mut_ptr(),
            row_offset: 0,
            input_params: Default::default(),
            output_params: Default::default(),
            call_machine: no_call_machine,
        };
        f(params);
        assert_eq!(data[0], -GoldilocksField::from(12));
    }
}