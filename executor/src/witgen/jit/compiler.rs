use std::{ffi::c_void, iter, mem, sync::Arc};

use auto_enums::auto_enum;
use itertools::Itertools;
use libloading::Library;
use powdr_number::{FieldElement, KnownField};

use crate::witgen::{
    data_structures::{finalizable_data::CompactDataRef, mutable_state::MutableState},
    jit::effect::MachineCallArgument,
    machines::{
        profiling::{record_end, record_start},
        LookupCell,
    },
    QueryCallback,
};

use super::{
    effect::{Assertion, Effect},
    symbolic_expression::{BinaryOperator, BitOperator, SymbolicExpression, UnaryOperator},
    variable::Variable,
};

pub struct WitgenFunction<T> {
    // TODO We might want to pass arguments as direct function parameters
    // (instead of a struct), so that
    // they are stored in registers instead of the stack. Should be checked.
    function: extern "C" fn(WitgenFunctionParams<T>),
    _library: Arc<Library>,
}

impl<T: FieldElement> WitgenFunction<T> {
    /// Call the witgen function to fill the data and "known" tables
    /// given a slice of parameters.
    /// The `row_offset` is the index inside `data` of the row considered to be "row zero".
    /// This function always succeeds (unless it panics).
    pub fn call<Q: QueryCallback<T>>(
        &self,
        _mutable_state: &MutableState<'_, T, Q>,
        params: &mut [LookupCell<T>],
        mut data: CompactDataRef<'_, T>,
    ) {
        let row_offset = data.row_offset().try_into().unwrap();
        let (data, known) = data.as_mut_slices();
        (self.function)(WitgenFunctionParams {
            data: data.into(),
            known: known.as_mut_ptr(),
            row_offset,
            params: params.into(),
            call_machine: call_machine::<T, Q>,
        });
    }
}

extern "C" fn call_machine<T: FieldElement, Q: QueryCallback<T>>(
    mutable_state: *const c_void,
    identity_id: u64,
    params: MutSlice<LookupCell<T>>,
) -> bool {
    let mutable_state = unsafe { &*(mutable_state as *const MutableState<T, Q>) };
    mutable_state
        .call_direct(identity_id, params.into())
        .unwrap()
}

/// Compile the given inferred effects into machine code and load it.
pub fn compile_effects<T: FieldElement>(
    first_column_id: u64,
    column_count: usize,
    known_inputs: &[Variable],
    effects: &[Effect<T, Variable>],
) -> Result<WitgenFunction<T>, String> {
    let utils = util_code::<T>(first_column_id, column_count)?;
    let witgen_code = witgen_code(known_inputs, effects);
    let code = format!("{utils}\n//-------------------------------\n{witgen_code}");

    record_start("JIT-compilation");
    let start = std::time::Instant::now();
    log::trace!("Calling cargo...");
    let r = powdr_jit_compiler::call_cargo(&code);
    log::trace!("Done compiling, took {:.2}s", start.elapsed().as_secs_f32());
    record_end("JIT-compilation");
    let lib_path = r.map_err(|e| format!("Failed to compile generated code: {e}"))?;

    let library = Arc::new(unsafe { libloading::Library::new(&lib_path.path).unwrap() });
    let witgen_fun = unsafe { library.get(b"witgen\0") }.unwrap();
    Ok(WitgenFunction {
        function: *witgen_fun,
        _library: library,
    })
}

#[repr(C)]
struct WitgenFunctionParams<'a, T: 'a> {
    /// A mutable slice to the full trace table. It has to have enough pre-allocated space.
    data: MutSlice<T>,
    /// A pointer to the data area of the "known" PaddedBitVec.
    known: *mut u32,
    /// The offset of the row considered to be "row zero".
    row_offset: u64,
    /// Input and output parameters if this is a machine call.
    params: MutSlice<LookupCell<'a, T>>,
    /// A callback to call submachines.
    call_machine: extern "C" fn(*const c_void, u64, MutSlice<LookupCell<'_, T>>) -> bool,
}

#[repr(C)]
struct MutSlice<T> {
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

impl<T> From<MutSlice<T>> for &mut [T] {
    fn from(slice: MutSlice<T>) -> Self {
        unsafe { std::slice::from_raw_parts_mut(slice.data, slice.len as usize) }
    }
}

fn witgen_code<T: FieldElement>(
    known_inputs: &[Variable],
    effects: &[Effect<T, Variable>],
) -> String {
    let load_known_inputs = known_inputs
        .iter()
        .map(|v| {
            let var_name = variable_to_string(v);
            let value = match v {
                Variable::Cell(c) => {
                    format!("get(data, row_offset, {}, {})", c.row_offset, c.id)
                }
                Variable::Param(i) => format!("get_param(params, {i})"),
            };
            format!("    let {var_name} = {value};")
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
                    format!("    set_param(params, {i}, {value});")
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
        params,
        call_machine
    }}: WitgenFunctionParams<FieldElement>,
) {{
    let known = known_to_slice(known, data.len);
    let data = data.to_mut_slice();
    let params = params.to_mut_slice();

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
        SymbolicExpression::Symbol(symbol, _) => variable_to_string(symbol),
        SymbolicExpression::BinaryOperation(left, op, right, _) => {
            let left = format_expression(left);
            let right = format_expression(right);
            match op {
                BinaryOperator::Add => format!("({left} + {right})"),
                BinaryOperator::Sub => format!("({left} - {right})"),
                BinaryOperator::Mul => format!("({left} * {right})"),
                BinaryOperator::Div => format!("({left} / {right})"),
                BinaryOperator::IntegerDiv => format!("integer_div({left}, {right})"),
            }
        }
        SymbolicExpression::UnaryOperation(op, inner, _) => {
            let inner = format_expression(inner);
            match op {
                UnaryOperator::Neg => format!("-{inner}"),
            }
        }
        SymbolicExpression::BitOperation(left, op, right, _) => {
            let left = format_expression(left);
            match op {
                BitOperator::And => format!("({left} & {right})"),
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
        format!("{row_offset}")
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

    let interface = format!(
        "\
        const column_count: u64 = {column_count};\n\
        const first_column_id: u64 = {first_column_id};\n\
        {}",
        include_str!("includes/interface.rs")
    );

    Ok(format!(
        "#![allow(non_snake_case, unused_parens, unused_variables)]\n{field_impl}\n{interface}"
    ))
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use powdr_number::GoldilocksField;

    use crate::witgen::jit::variable::Cell;

    use super::*;

    #[test]
    fn compile_util_code_goldilocks() {
        compile_effects::<GoldilocksField>(0, 2, &[], &[]).unwrap();
    }

    // We would like to test the generic field implementation, but
    // we need direct representation and this is not clear.
    // #[test]
    // fn compile_util_code_koalabear() {
    //     compile_effects::<KoalaBearField>(0, 2, &[], &[]).unwrap();
    // }

    fn cell(column_name: &str, id: u64, row_offset: i32) -> Variable {
        Variable::Cell(Cell {
            column_name: column_name.to_string(),
            row_offset,
            id,
        })
    }

    fn param(i: usize) -> Variable {
        Variable::Param(i)
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
        params,
        call_machine
    }: WitgenFunctionParams<FieldElement>,
) {
    let known = known_to_slice(known, data.len);
    let data = data.to_mut_slice();
    let params = params.to_mut_slice();

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

    extern "C" fn no_call_machine(
        _: *const c_void,
        _: u64,
        _: MutSlice<LookupCell<'_, GoldilocksField>>,
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
        let f = compile_effects(0, 1, &[], &effects).unwrap();
        let mut data = vec![GoldilocksField::from(0); 2];
        let mut known = vec![0; 1];
        let params = WitgenFunctionParams {
            data: MutSlice::from(data.as_mut_slice()),
            known: known.as_mut_ptr(),
            row_offset: 0,
            params: Default::default(),
            call_machine: no_call_machine,
        };
        (f.function)(params);
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
        let f1 = compile_effects(0, column_count, &[], &effects1).unwrap();
        let f2 = compile_effects(0, column_count, &[], &effects2).unwrap();
        let mut data = vec![GoldilocksField::from(0); data_len];
        let mut known = vec![0; row_count];
        let params1 = WitgenFunctionParams {
            data: MutSlice::from(data.as_mut_slice()),
            known: known.as_mut_ptr(),
            row_offset: 0,
            params: Default::default(),
            call_machine: no_call_machine,
        };
        (f1.function)(params1);
        assert_eq!(data[0], GoldilocksField::from(7));
        assert_eq!(data[1], GoldilocksField::from(0));
        assert_eq!(data[2], GoldilocksField::from(0));
        assert_eq!(data[3], GoldilocksField::from(0));
        assert_eq!(known, vec![1, 0]);
        let params2 = WitgenFunctionParams {
            data: MutSlice::from(data.as_mut_slice()),
            known: known.as_mut_ptr(),
            row_offset: 1,
            params: Default::default(),
            call_machine: no_call_machine,
        };
        (f2.function)(params2);
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
        let f = compile_effects(0, 1, &[], &effects).unwrap();
        let mut data = vec![GoldilocksField::from(0); 5];
        let mut known = vec![0; 5];
        let params = WitgenFunctionParams {
            data: data.as_mut_slice().into(),
            known: known.as_mut_ptr(),
            row_offset: 0,
            params: Default::default(),
            call_machine: no_call_machine,
        };
        (f.function)(params);
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
        let f = compile_effects(0, 3, &known_inputs, &effects).unwrap();
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
            params: Default::default(),
            call_machine: no_call_machine,
        };
        (f.function)(params);
        assert_eq!(data[0], -GoldilocksField::from(12));
    }

    #[test]
    fn integer_division() {
        let x = cell("x", 0, 0);
        let y = cell("y", 1, 0);
        let z = cell("z", 2, 0);
        let effects = vec![
            assignment(&y, symbol(&x).integer_div(&number(10))),
            assignment(&z, symbol(&x).integer_div(&-number(10))),
        ];
        let known_inputs = vec![x.clone()];
        let f = compile_effects(0, 3, &known_inputs, &effects).unwrap();
        let mut data = vec![
            GoldilocksField::from(23),
            GoldilocksField::from(0),
            GoldilocksField::from(0),
        ];
        let mut known = vec![0; 1];
        let params = WitgenFunctionParams {
            data: data.as_mut_slice().into(),
            known: known.as_mut_ptr(),
            row_offset: 0,
            params: Default::default(),
            call_machine: no_call_machine,
        };
        (f.function)(params);
        assert_eq!(data[0], GoldilocksField::from(23));
        assert_eq!(data[1], GoldilocksField::from(2));
        assert_eq!(data[2], GoldilocksField::from(0));
    }

    #[test]
    fn input_output() {
        let x = param(0);
        let y = param(1);
        let x_val: GoldilocksField = 7.into();
        let mut y_val: GoldilocksField = 9.into();
        let effects = vec![assignment(&y, symbol(&x) + number(7))];
        let f = compile_effects(0, 1, &[x], &effects).unwrap();
        let mut data = vec![];
        let mut known = vec![];
        let mut params = vec![LookupCell::Input(&x_val), LookupCell::Output(&mut y_val)];
        let params = WitgenFunctionParams {
            data: data.as_mut_slice().into(),
            known: known.as_mut_ptr(),
            row_offset: 0,
            params: params.as_mut_slice().into(),
            call_machine: no_call_machine,
        };
        (f.function)(params);
        assert_eq!(y_val, GoldilocksField::from(7 * 2));
    }

    #[test]
    fn bit_ops() {
        let a = cell("a", 0, 0);
        let x = cell("x", 1, 0);
        // Test that the operators & and | work with numbers larger than the modulus.
        let large_num =
            <powdr_number::GoldilocksField as powdr_number::FieldElement>::Integer::from(
                0xffffffffffffffff_u64,
            );
        assert!(large_num.to_string().parse::<u64>().unwrap() == 0xffffffffffffffff_u64);
        assert!(large_num > GoldilocksField::modulus());
        let effects = vec![assignment(&x, symbol(&a) & large_num)];
        let known_inputs = vec![a.clone()];
        let code = witgen_code(&known_inputs, &effects);
        assert!(code.contains(&format!("let c_x_1_0 = (c_a_0_0 & {large_num});")));
    }
}
