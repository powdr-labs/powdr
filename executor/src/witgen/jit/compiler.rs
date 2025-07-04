use std::{cmp::Ordering, ffi::c_void, sync::Arc};

use itertools::Itertools;
use libloading::Library;
use powdr_ast::{
    analyzed::{PolyID, PolynomialType},
    indent,
};
use powdr_constraint_solver::{
    effect::{Assertion, BitDecomposition, BitDecompositionComponent, Condition},
    symbolic_expression::{BinaryOperator, SymbolicExpression, UnaryOperator},
};
use powdr_jit_compiler::{util_code::util_code, CodeGenerator, DefinitionFetcher};
use powdr_number::FieldElement;

use crate::witgen::{
    data_structures::{
        finalizable_data::{ColumnLayout, CompactDataRef},
        mutable_state::MutableState,
    },
    jit::prover_function_heuristics::{ProverFunctionComputation, QueryType},
    machines::{
        profiling::{record_end, record_start},
        LookupCell,
    },
    FixedData, QueryCallback,
};

use super::{
    effect::{Effect, ProverFunctionCall},
    prover_function_heuristics::ProverFunction,
    variable::Variable,
};

pub struct CompiledFunction<T> {
    // TODO We might want to pass arguments as direct function parameters
    // (instead of a struct), so that
    // they are stored in registers instead of the stack. Should be checked.
    function: extern "C" fn(WitgenFunctionParams<T>),
    _library: Arc<Library>,
}

impl<T: FieldElement> CompiledFunction<T> {
    pub fn call<Q: QueryCallback<T>>(
        &self,
        fixed_data: &FixedData<'_, T>,
        mutable_state: &MutableState<'_, T, Q>,
        params: &mut [LookupCell<T>],
        mut data: CompactDataRef<'_, T>,
    ) {
        let row_offset = data.row_offset.try_into().unwrap();
        let (data, known) = data.as_mut_slices();
        let params = WitgenFunctionParams {
            data: data.into(),
            known: known.as_mut_ptr(),
            row_offset,
            params: params.into(),
            mutable_state: mutable_state as *const _ as *const c_void,
            call_machine: call_machine::<T, Q>,
            fixed_data: fixed_data as *const _ as *const c_void,
            get_fixed_value: get_fixed_value::<T>,
            input_from_channel: input_from_channel::<T, Q>,
            output_to_channel: output_to_channel::<T, Q>,
        };
        (self.function)(params);
    }
}

extern "C" fn get_fixed_value<T: FieldElement>(
    fixed_data: *const c_void,
    column: u64,
    row: u64,
) -> T {
    let fixed_data = unsafe { &*(fixed_data as *const FixedData<'_, T>) };
    let poly_id = PolyID {
        id: column,
        ptype: PolynomialType::Constant,
    };
    fixed_data.fixed_cols[&poly_id].values_max_size()[row as usize]
}

extern "C" fn call_machine<T: FieldElement, Q: QueryCallback<T>>(
    mutable_state: *const c_void,
    bus_id: T,
    params: MutSlice<LookupCell<T>>,
) -> bool {
    let mutable_state = unsafe { &*(mutable_state as *const MutableState<T, Q>) };
    mutable_state.call_direct(bus_id, params.into()).unwrap()
}

extern "C" fn input_from_channel<T: FieldElement, Q: QueryCallback<T>>(
    mutable_state: *const c_void,
    channel: u32,
    index: u64,
) -> T {
    let mutable_state = unsafe { &*(mutable_state as *const MutableState<T, Q>) };
    // TODO what is the proper error handling?
    // TODO What to do for Ok(None)?
    (mutable_state.query_callback())(&format!("Input({channel},{index})"))
        .unwrap()
        .unwrap()
}

extern "C" fn output_to_channel<T: FieldElement, Q: QueryCallback<T>>(
    mutable_state: *const c_void,
    fd: u32,
    elem: T,
) {
    let mutable_state = unsafe { &*(mutable_state as *const MutableState<T, Q>) };
    (mutable_state.query_callback())(&format!("Output({fd},{elem})"))
        .unwrap()
        .unwrap();
}

/// Compile the given inferred effects into machine code and load it.
pub fn compile_effects<T: FieldElement, D: DefinitionFetcher>(
    definitions: &D,
    column_layout: ColumnLayout,
    known_inputs: &[Variable],
    effects: &[Effect<T, Variable>],
    prover_functions: Vec<ProverFunction<'_, T>>,
) -> Result<CompiledFunction<T>, String> {
    let utils = util_code::<T>()?;
    let interface = interface_code(column_layout);
    let mut codegen = CodeGenerator::<T, _>::new(definitions);
    let prover_functions = prover_functions
        .iter()
        .map(|f| prover_function_code(f, &mut codegen))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .format("\n");
    let prover_functions_dependents = codegen.generated_code();
    let witgen_code = witgen_code(known_inputs, effects);
    let code = format!(
        "{utils}\n\
        //-------------------------------\n\
        {prover_functions_dependents}\n\
        {prover_functions}\n\
        //-------------------------------\n\
        {interface}\n\
        //-------------------------------\n\
        {witgen_code}"
    );

    record_start("JIT-compilation");
    let start = std::time::Instant::now();
    let opt_level = 0;
    log::trace!("Compiling the following code using optimization level {opt_level}:\n{code}");
    let r = powdr_jit_compiler::call_cargo(&code, Some(opt_level));
    log::trace!("Done compiling, took {:.2}s", start.elapsed().as_secs_f32());
    record_end("JIT-compilation");
    let lib_path = r.map_err(|e| format!("Failed to compile generated code: {e}"))?;

    let library = Arc::new(unsafe { libloading::Library::new(&lib_path.path).unwrap() });
    let witgen_fun = unsafe { library.get(b"witgen\0") }.unwrap();
    Ok(CompiledFunction {
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
    /// The pointer to the mutable state.
    mutable_state: *const c_void,
    /// A callback to call submachines.
    call_machine: extern "C" fn(*const c_void, T, MutSlice<LookupCell<'_, T>>) -> bool,
    /// A pointer to the "fixed data".
    fixed_data: *const c_void,
    /// A callback to retrieve values from fixed columns.
    /// The parameters are: fixed data pointer, fixed column id, row number.
    get_fixed_value: extern "C" fn(*const c_void, u64, u64) -> T,
    /// A callback to retrieve a prover-provided value from a channel
    /// The parameters are: mutable state pointer, channel number, index.
    input_from_channel: extern "C" fn(*const c_void, u32, u64) -> T,
    /// A callback to output a value to a channel.
    /// The parameters are: mutable state pointer, channel number, value.
    output_to_channel: extern "C" fn(*const c_void, u32, T),
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
                Variable::WitnessCell(c) => {
                    format!("get(data, row_offset, {}, {})", c.row_offset, c.id)
                }
                Variable::Param(i) => format!("get_param(params, {i})"),
                Variable::FixedCell(_) => panic!("Fixed columns should not be known inputs."),
                Variable::IntermediateCell(_) => {
                    unreachable!("Intermediate cells should not be known inputs.")
                }
                Variable::MachineCallParam(_) => {
                    unreachable!("Machine call variables should not be pre-known.")
                }
            };
            format!("    let {var_name} = {value};")
        })
        .format("\n");

    // Pre-load all the fixed columns so that we can treat them as
    // plain variables later.
    let load_fixed = effects
        .iter()
        .flat_map(|e| e.referenced_variables())
        .filter_map(|v| match v {
            Variable::FixedCell(c) => Some((v, c)),
            _ => None,
        })
        .unique()
        .map(|(var, cell)| {
            format!(
                "    let {} = get_fixed_value(fixed_data, {}, (row_offset + {}));",
                variable_to_string(var),
                cell.id,
                cell.row_offset,
            )
        })
        .format("\n");

    let main_code = format_effects(effects);
    let vars_known = effects.iter().flat_map(Effect::written_vars).collect_vec();
    let store_values = vars_known
        .iter()
        .filter_map(|var| {
            let value = variable_to_string(var);
            match var {
                Variable::WitnessCell(cell) => Some(format!(
                    "    set(data, row_offset, {}, {}, {value});",
                    cell.row_offset, cell.id,
                )),
                Variable::Param(i) => Some(format!("    set_param(params, {i}, {value});")),
                Variable::FixedCell(_) => panic!("Fixed columns should not be written to."),
                Variable::IntermediateCell(_) => {
                    // We do not permanently store intermediate cells
                    None
                }
                Variable::MachineCallParam(_) => {
                    // This is just an internal variable.
                    None
                }
            }
        })
        .format("\n");
    // We do not store "known" together with the values, because we hope
    // that this way, the optimizer can group them better.
    let store_known = vars_known
        .iter()
        .filter_map(|var| match var {
            Variable::WitnessCell(cell) => Some(cell),
            Variable::Param(_)
            | Variable::FixedCell(_)
            | Variable::IntermediateCell(_)
            | Variable::MachineCallParam(_) => None,
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
        mutable_state,
        call_machine,
        fixed_data,
        get_fixed_value,
        input_from_channel,
        output_to_channel,
    }}: WitgenFunctionParams<FieldElement>,
) {{
    let known = known_to_slice(known, data.len);
    let data = data.to_mut_slice();
    let params = params.to_mut_slice();

    // Pre-load fixed column values into local variables
{load_fixed}

    // Load all known inputs into local variables
{load_known_inputs}

    // Perform the main computations
{main_code}

    // Store the newly derived witness cell values
{store_values}

    // Store the "known" flags
{store_known}
}}
"#
    )
}

pub fn format_effects<T: FieldElement>(effects: &[Effect<T, Variable>]) -> String {
    indent(format_effects_inner(effects, true), 1)
}

fn format_effects_inner<T: FieldElement>(
    effects: &[Effect<T, Variable>],
    is_top_level: bool,
) -> String {
    effects
        .iter()
        .map(|effect| format_effect(effect, is_top_level))
        .join("\n")
}

fn format_effect<T: FieldElement>(effect: &Effect<T, Variable>, is_top_level: bool) -> String {
    match effect {
        Effect::Assignment(var, e) => {
            format!(
                "{}{} = {};",
                if is_top_level { "let " } else { "" },
                variable_to_string(var),
                format_expression(e)
            )
        }
        Effect::BitDecomposition(BitDecomposition { value, components }) => {
            format_bit_decomposition(is_top_level, value, components)
        }
        Effect::RangeConstraint(..) => {
            unreachable!("Final code should not contain pure range constraints.")
        }
        Effect::Assertion(Assertion {
            lhs,
            rhs,
            expected_equal,
            ..
        }) => format!(
            "assert!({} {} {});",
            format_expression(lhs),
            if *expected_equal { "==" } else { "!=" },
            format_expression(rhs)
        ),
        Effect::MachineCall(id, known, vars) => {
            let mut result_vars = vec![];
            let args = vars
                .iter()
                .zip_eq(known)
                .map(|(v, known)| {
                    let var_name = variable_to_string(v);
                    if known {
                        format!("LookupCell::Input(&{var_name})")
                    } else {
                        if is_top_level {
                            result_vars.push(var_name.clone());
                        }
                        format!("LookupCell::Output(&mut {var_name})")
                    }
                })
                .format(", ")
                .to_string();
            let var_decls = result_vars
                .iter()
                .map(|var_name| format!("let mut {var_name} = FieldElement::default();\n"))
                .format("");
            format!(
                "{var_decls}assert!(call_machine(mutable_state, {id}.into(), MutSlice::from((&mut [{args}]).as_mut_slice())));"
            )
        }
        Effect::ProverFunctionCall(ProverFunctionCall {
            targets,
            function_index,
            row_offset,
            inputs,
        }) => {
            format!(
                "{}[{}] = prover_function_{function_index}(mutable_state, input_from_channel, output_to_channel, row_offset + {row_offset}, &[{}]);",
                if is_top_level { "let " } else { "" },
                targets.iter().map(|v|
                    if let Some(v) = v {
                        variable_to_string(v)
                    } else {
                        "_".to_string()
                    }
                    ).format(", "),
                inputs.iter().map(variable_to_string).format(", ")
            )
        }
        Effect::Branch(condition, first, second) => {
            let var_decls = if is_top_level {
                // We need to declare all assigned variables at top level,
                // so that they are available after the branches.
                first
                    .iter()
                    .chain(second)
                    .flat_map(|e| e.written_vars())
                    .sorted()
                    .dedup()
                    .map(|v| {
                        let v = variable_to_string(v);
                        format!("let mut {v} = FieldElement::default();\n")
                    })
                    .format("")
                    .to_string()
            } else {
                "".to_string()
            };

            let else_part = if matches!(second[..], [Effect::Branch(..)]) {
                format_effects_inner(second, false)
            } else {
                format!("{{\n{}\n}}", indent(format_effects_inner(second, false), 1))
            };

            format!(
                "{var_decls}if {} {{\n{}\n}} else {else_part}",
                format_condition(condition),
                indent(format_effects_inner(first, false), 1)
            )
        }
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

fn format_bit_decomposition<T: FieldElement>(
    is_top_level: bool,
    value: &SymbolicExpression<T, Variable>,
    components: &[BitDecompositionComponent<T, Variable>],
) -> String {
    let mut result = format!("let bit_decomp_value = {};\n", format_expression(value));

    // The components need to be sorted so that carry propagates correctly.
    let components = components
        .iter()
        .sorted_by_key(|c| c.exponent)
        .collect_vec();
    // If no component is negative, we can assume that the value is unsigned
    // which makes some functions much easier.
    let signed = if components.iter().any(|c| c.is_negative) {
        "signed"
    } else {
        "unsigned"
    };
    for BitDecompositionComponent {
        variable,
        is_negative,
        exponent,
        bit_mask,
    } in components
    {
        assert!(*bit_mask != 0.into());
        result.push_str(&format!(
            "let bit_decomp_component = bitand_{signed}({}bit_decomp_value, 0x{bit_mask:0x});\n",
            if *is_negative { "-" } else { "" },
        ));

        result.push_str(&format!(
            "{}{} = unsigned_shift(bit_decomp_component, {exponent});\n",
            if is_top_level { "let " } else { "" },
            variable_to_string(variable)
        ));
        result.push_str(&format!(
            "let bit_decomp_value = bit_decomp_value {} bit_decomp_component;\n",
            // Inverted because we remove the extracted value.
            if *is_negative { "+" } else { "-" },
        ));
    }
    result.push_str("assert!(bit_decomp_value == FieldElement::from(0));\n");
    result
}

fn format_condition<T: FieldElement>(
    Condition { value, condition }: &Condition<SymbolicExpression<T, Variable>>,
) -> String {
    let value = format!("IntType::from({})", format_expression(value));
    let (min, max) = condition.range();
    match min.cmp(&max) {
        Ordering::Equal => format!("{value} == {min}",),
        Ordering::Less => format!("{{ let v = {value}; {min} <= v && v <= {max} }}"),
        Ordering::Greater => format!("{{ let v = {value}; v <= {min} || v >= {max} }}"),
    }
}

/// Returns the name of a local (stack) variable for the given expression variable.
fn variable_to_string(v: &Variable) -> String {
    match v {
        Variable::WitnessCell(cell) => format!(
            "c_{}_{}_{}",
            escape_column_name(&cell.column_name),
            cell.id,
            format_row_offset(cell.row_offset)
        ),
        Variable::Param(i) => format!("p_{i}"),
        Variable::FixedCell(cell) => format!(
            "f_{}_{}_{}",
            escape_column_name(&cell.column_name),
            cell.id,
            format_row_offset(cell.row_offset)
        ),
        Variable::IntermediateCell(cell) => format!(
            "i_{}_{}_{}",
            escape_column_name(&cell.column_name),
            cell.id,
            format_row_offset(cell.row_offset)
        ),
        Variable::MachineCallParam(call_var) => {
            format!(
                "call_var_{}_{}_{}",
                call_var.identity_id,
                format_row_offset(call_var.row_offset),
                call_var.index
            )
        }
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

/// Returns the rust code containing functions and data structures used to
/// interface with witgen functions given the layout of the trace table.
fn interface_code(column_layout: ColumnLayout) -> String {
    let ColumnLayout {
        column_count,
        first_column_id,
    } = column_layout;
    format!(
        "\
        const column_count: u64 = {column_count};\n\
        const first_column_id: u64 = {first_column_id};\n\
        {}",
        include_str!("includes/interface.rs")
    )
}

fn prover_function_code<T: FieldElement, D: DefinitionFetcher>(
    f: &ProverFunction<'_, T>,
    codegen: &mut CodeGenerator<'_, T, D>,
) -> Result<String, String> {
    let code = match &f.computation {
        ProverFunctionComputation::ComputeFrom(code) => format!(
            "({}).call(args.to_vec().into())",
            codegen.generate_code_for_expression(code)?
        ),
        ProverFunctionComputation::ProvideIfUnknown(code) => {
            assert!(!f.compute_multi);
            format!("({}).call(())", codegen.generate_code_for_expression(code)?)
        }
        ProverFunctionComputation::HandleQueryInputOutput(branches) => {
            let indent = "        ";
            // We assign zero in the "no match" case. The correct behaviour would be to
            // not assign anything, but it should work for all our use-cases.
            format!(
                "match IntType::from(args[0]) {{\n{}\n{indent}_ => 0.into(),\n    }}",
                branches
                    .iter()
                    .map(|(value, query_type)| {
                        let result = match query_type {
                            QueryType::Input => {
                                "input_from_channel(mutable_state, IntType::from(args[0]) as u32, IntType::from(args[1]) as u64),"
                            }
                            QueryType::Output => {
                                "{ output_to_channel(mutable_state, IntType::from(args[0]) as u32, args[1]); 0.into() },"
                            }
                        };
                        format!("{indent}{value} => {result}")
                    })
                    .format("\n")
            )
        }
    };
    let code = if f.compute_multi {
        format!("({code}).as_slice().try_into().unwrap()")
    } else {
        assert_eq!(f.target.len(), 1);
        format!("[{code}]")
    };
    let length = f.target.len();
    let index = f.index;
    Ok(format!(
        r#"fn prover_function_{index}(
    mutable_state: *const std::ffi::c_void,
    input_from_channel: extern "C" fn(*const std::ffi::c_void, u32, u64) -> FieldElement,
    output_to_channel: extern "C" fn(*const std::ffi::c_void, u32, FieldElement),
    i: u64,
    args: &[FieldElement]
) -> [FieldElement; {length}] {{
    let i: ibig::IBig = i.into();
    {code}
}}"#
    ))
}

#[cfg(test)]
mod tests {

    use std::ptr::null;

    use powdr_ast::analyzed::AlgebraicReference;
    use powdr_ast::analyzed::FunctionValueDefinition;
    use powdr_constraint_solver::range_constraint::RangeConstraint;
    use powdr_constraint_solver::runtime_constant::RuntimeConstant;
    use pretty_assertions::assert_eq;
    use test_log::test;

    use powdr_number::GoldilocksField;

    use crate::witgen::jit::prover_function_heuristics::QueryType;
    use crate::witgen::jit::variable::Cell;
    use crate::witgen::jit::variable::MachineCallVariable;

    use super::*;

    struct NoDefinitions;
    impl DefinitionFetcher for NoDefinitions {
        fn get_definition(&self, _: &str) -> Option<&FunctionValueDefinition> {
            None
        }
    }

    fn compile_effects(
        column_count: usize,
        known_inputs: &[Variable],
        effects: &[Effect<GoldilocksField, Variable>],
    ) -> Result<CompiledFunction<GoldilocksField>, String> {
        super::compile_effects(
            &NoDefinitions,
            ColumnLayout {
                column_count,
                first_column_id: 0,
            },
            known_inputs,
            effects,
            vec![],
        )
    }

    #[test]
    fn compile_util_code_goldilocks() {
        compile_effects(2, &[], &[]).unwrap();
    }

    // We would like to test the generic field implementation, but
    // we need direct representation and this is not clear.
    // #[test]
    // fn compile_util_code_koalabear() {
    //     compile_effects::<KoalaBearField>(0, 2, &[], &[]).unwrap();
    // }

    fn cell(column_name: &str, id: u64, row_offset: i32) -> Variable {
        Variable::WitnessCell(Cell {
            column_name: column_name.to_string(),
            row_offset,
            id,
        })
    }

    fn param(i: usize) -> Variable {
        Variable::Param(i)
    }

    fn call_var(identity_id: u64, row_offset: i32, index: usize) -> Variable {
        Variable::MachineCallParam(MachineCallVariable {
            identity_id,
            row_offset,
            index,
        })
    }

    fn symbol(var: &Variable) -> SymbolicExpression<GoldilocksField, Variable> {
        SymbolicExpression::from_symbol(var.clone(), Default::default())
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
    fn code_for_effects() {
        let a0 = cell("a", 2, 0);
        let x0 = cell("x", 0, 0);
        let ym1 = cell("y", 1, -1);
        let yp1 = cell("y", 1, 1);
        let cv1 = call_var(7, 1, 0);
        let r1 = call_var(7, 1, 1);
        let effects = vec![
            assignment(&x0, number(7) * symbol(&a0)),
            assignment(&cv1, symbol(&x0)),
            Effect::MachineCall(
                GoldilocksField::from(7),
                [false, true].into_iter().collect(),
                vec![r1.clone(), cv1.clone()],
            ),
            assignment(&ym1, symbol(&r1)),
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
        mutable_state,
        call_machine,
        fixed_data,
        get_fixed_value,
        input_from_channel,
        output_to_channel,
    }: WitgenFunctionParams<FieldElement>,
) {
    let known = known_to_slice(known, data.len);
    let data = data.to_mut_slice();
    let params = params.to_mut_slice();

    // Pre-load fixed column values into local variables


    // Load all known inputs into local variables
    let c_a_2_0 = get(data, row_offset, 0, 2);

    // Perform the main computations
    let c_x_0_0 = (FieldElement::from(7) * c_a_2_0);
    let call_var_7_1_0 = c_x_0_0;
    let mut call_var_7_1_1 = FieldElement::default();
    assert!(call_machine(mutable_state, 7.into(), MutSlice::from((&mut [LookupCell::Output(&mut call_var_7_1_1), LookupCell::Input(&call_var_7_1_0)]).as_mut_slice())));
    let c_y_1_m1 = call_var_7_1_1;
    let c_y_1_1 = (c_y_1_m1 + c_x_0_0);
    assert!(c_y_1_m1 == c_x_0_0);

    // Store the newly derived witness cell values
    set(data, row_offset, 0, 0, c_x_0_0);
    set(data, row_offset, -1, 1, c_y_1_m1);
    set(data, row_offset, 1, 1, c_y_1_1);

    // Store the \"known\" flags
    set_known(known, row_offset, 0, 0);
    set_known(known, row_offset, -1, 1);
    set_known(known, row_offset, 1, 1);
}
"
        );
    }

    extern "C" fn no_call_machine(
        _: *const c_void,
        _: GoldilocksField,
        _: MutSlice<LookupCell<'_, GoldilocksField>>,
    ) -> bool {
        false
    }

    extern "C" fn get_fixed_data_test(_: *const c_void, col_id: u64, row: u64) -> GoldilocksField {
        GoldilocksField::from(col_id * 2000 + row)
    }

    extern "C" fn input_from_channel_test(_: *const c_void, _: u32, _: u64) -> GoldilocksField {
        GoldilocksField::from(117)
    }

    extern "C" fn output_to_channel_test(_: *const c_void, _: u32, _: GoldilocksField) {}

    fn witgen_fun_params<'a>(
        data: &mut [GoldilocksField],
        known: &mut [u32],
    ) -> WitgenFunctionParams<'a, GoldilocksField> {
        WitgenFunctionParams {
            data: data.into(),
            known: known.as_mut_ptr(),
            row_offset: 0,
            params: Default::default(),
            mutable_state: std::ptr::null(),
            call_machine: no_call_machine,
            fixed_data: null(),
            get_fixed_value: get_fixed_data_test,
            input_from_channel: input_from_channel_test,
            output_to_channel: output_to_channel_test,
        }
    }

    fn witgen_fun_params_with_params<'a>(
        data: &mut [GoldilocksField],
        known: &mut [u32],
        params: &'a mut [LookupCell<'a, GoldilocksField>],
    ) -> WitgenFunctionParams<'a, GoldilocksField> {
        WitgenFunctionParams {
            data: data.into(),
            known: known.as_mut_ptr(),
            row_offset: 0,
            params: params.into(),
            mutable_state: std::ptr::null(),
            call_machine: no_call_machine,
            fixed_data: null(),
            get_fixed_value: get_fixed_data_test,
            input_from_channel: input_from_channel_test,
            output_to_channel: output_to_channel_test,
        }
    }

    #[test]
    fn load_code() {
        let x = cell("x", 0, 0);
        let y = cell("y", 1, 0);
        let effects = vec![
            assignment(&x, number(7)),
            assignment(&y, symbol(&x) + number(2)),
        ];
        let f = compile_effects(1, &[], &effects).unwrap();
        let mut data = vec![GoldilocksField::from(0); 2];
        let mut known = vec![0; 1];
        (f.function)(witgen_fun_params(&mut data, &mut known));
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
        let f1 = compile_effects(column_count, &[], &effects1).unwrap();
        let f2 = compile_effects(column_count, &[], &effects2).unwrap();
        let mut data = vec![GoldilocksField::from(0); data_len];
        let mut known = vec![0; row_count];
        (f1.function)(witgen_fun_params(&mut data, &mut known));
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
            mutable_state: std::ptr::null(),
            call_machine: no_call_machine,
            fixed_data: null(),
            get_fixed_value: get_fixed_data_test,
            input_from_channel: input_from_channel_test,
            output_to_channel: output_to_channel_test,
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
        let f = compile_effects(1, &[], &effects).unwrap();
        let mut data = vec![GoldilocksField::from(0); 5];
        let mut known = vec![0; 5];
        (f.function)(witgen_fun_params(&mut data, &mut known));
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
        let f = compile_effects(3, &known_inputs, &effects).unwrap();
        let mut data = vec![
            GoldilocksField::from(0),
            GoldilocksField::from(3),
            -GoldilocksField::from(4),
        ];
        let mut known = vec![0; 1];
        (f.function)(witgen_fun_params(&mut data, &mut known));
        assert_eq!(data[0], -GoldilocksField::from(12));
    }

    #[test]
    fn input_output() {
        let x = param(0);
        let y = param(1);
        let x_val: GoldilocksField = 7.into();
        let mut y_val: GoldilocksField = 9.into();
        let effects = vec![assignment(&y, symbol(&x) + number(7))];
        let f = compile_effects(1, &[x], &effects).unwrap();
        let mut data = vec![];
        let mut known = vec![];
        let mut params = vec![LookupCell::Input(&x_val), LookupCell::Output(&mut y_val)];
        let params = witgen_fun_params_with_params(&mut data, &mut known, &mut params);
        (f.function)(params);
        assert_eq!(y_val, GoldilocksField::from(7 * 2));
    }

    #[test]
    fn bit_decomposition() {
        let x = param(0);
        let a = param(1);
        let b = param(2);
        let c = param(3);
        let x_val: GoldilocksField = 0x1f202.into();
        let mut a_val: GoldilocksField = 9.into();
        let mut b_val: GoldilocksField = 9.into();
        let mut c_val: GoldilocksField = 9.into();
        let effects = vec![Effect::BitDecomposition(BitDecomposition {
            value: symbol(&x),
            components: vec![
                BitDecompositionComponent {
                    variable: a.clone(),
                    is_negative: false,
                    exponent: 16,
                    bit_mask: 0xff0000u64.into(),
                },
                BitDecompositionComponent {
                    variable: b.clone(),
                    is_negative: true,
                    exponent: 8,
                    bit_mask: 0xff00u64.into(),
                },
                BitDecompositionComponent {
                    variable: c.clone(),
                    is_negative: false,
                    exponent: 0,
                    bit_mask: 0xffu64.into(),
                },
            ],
        })];
        let f = compile_effects(1, &[x], &effects).unwrap();
        let mut data = vec![];
        let mut known = vec![];
        let mut params = vec![
            LookupCell::Input(&x_val),
            LookupCell::Output(&mut a_val),
            LookupCell::Output(&mut b_val),
            LookupCell::Output(&mut c_val),
        ];
        let params = witgen_fun_params_with_params(&mut data, &mut known, &mut params);
        (f.function)(params);
        assert_eq!(a_val, GoldilocksField::from(2));
        assert_eq!(b_val, GoldilocksField::from(0x100 - 0xf2));
        assert_eq!(c_val, GoldilocksField::from(2));
        assert_eq!(
            GoldilocksField::from(0x1f202),
            a_val * GoldilocksField::from(0x10000) - b_val * GoldilocksField::from(0x100) + c_val
        );

        let x_val = -GoldilocksField::from(0x808);
        let mut params = vec![
            LookupCell::Input(&x_val),
            LookupCell::Output(&mut a_val),
            LookupCell::Output(&mut b_val),
            LookupCell::Output(&mut c_val),
        ];
        let params = witgen_fun_params_with_params(&mut data, &mut known, &mut params);
        (f.function)(params);
        assert_eq!(a_val, GoldilocksField::from(0));
        assert_eq!(b_val, GoldilocksField::from(9));
        assert_eq!(c_val, GoldilocksField::from(0xf8));
        assert_eq!(
            -GoldilocksField::from(0x808),
            a_val * GoldilocksField::from(0x10000) - b_val * GoldilocksField::from(0x100) + c_val
        );
    }

    #[test]
    fn fixed_column_access() {
        let a = cell("a", 0, 0);
        let x = Variable::FixedCell(Cell {
            column_name: "X".to_string(),
            id: 15,
            row_offset: 6,
        });
        let effects = vec![assignment(&a, symbol(&x))];
        let f = compile_effects(1, &[], &effects).unwrap();
        let mut data = vec![7.into()];
        let mut known = vec![0];
        let params = witgen_fun_params(&mut data, &mut known);
        (f.function)(params);
        assert_eq!(data[0], GoldilocksField::from(30006));
    }

    extern "C" fn mock_call_machine(
        _: *const c_void,
        id: GoldilocksField,
        params: MutSlice<LookupCell<'_, GoldilocksField>>,
    ) -> bool {
        assert_eq!(id, 7.into());
        assert_eq!(params.len, 3);

        let params: &mut [LookupCell<GoldilocksField>] = params.into();
        match &params[0] {
            LookupCell::Input(x) => assert_eq!(**x, 7.into()),
            _ => panic!(),
        }
        match &mut params[1] {
            LookupCell::Output(y) => **y = 9.into(),
            _ => panic!(),
        }
        match &mut params[2] {
            LookupCell::Output(z) => **z = 18.into(),
            _ => panic!(),
        }
        true
    }

    #[test]
    fn submachine_calls() {
        let x = cell("x", 0, 0);
        let y = cell("y", 1, 0);
        let v1 = call_var(7, 0, 0);
        let r1 = call_var(7, 0, 1);
        let r2 = call_var(7, 0, 2);
        let effects = vec![
            Effect::Assignment(v1.clone(), number(7)),
            Effect::MachineCall(
                7.into(),
                [true, false, false].into_iter().collect(),
                vec![v1, r1.clone(), r2.clone()],
            ),
            Effect::Assignment(x.clone(), symbol(&r1)),
            Effect::Assignment(y.clone(), symbol(&r2)),
        ];
        let known_inputs = vec![];
        let f = compile_effects(3, &known_inputs, &effects).unwrap();
        let mut data = vec![GoldilocksField::from(0); 3];
        let mut known = vec![0; 1];
        let params = WitgenFunctionParams {
            data: data.as_mut_slice().into(),
            known: known.as_mut_ptr(),
            row_offset: 0,
            params: Default::default(),
            mutable_state: std::ptr::null(),
            call_machine: mock_call_machine,
            fixed_data: null(),
            get_fixed_value: get_fixed_data_test,
            input_from_channel: input_from_channel_test,
            output_to_channel: output_to_channel_test,
        };
        (f.function)(params);
        assert_eq!(data[0], GoldilocksField::from(9));
        assert_eq!(data[1], GoldilocksField::from(18));
        assert_eq!(data[2], GoldilocksField::from(0));
    }

    #[test]
    fn branches() {
        let x = param(0);
        let y = param(1);
        let mut x_val: GoldilocksField = 7.into();
        let mut y_val: GoldilocksField = 9.into();
        let effects = vec![Effect::Branch(
            Condition {
                value: SymbolicExpression::from_symbol(x.clone(), Default::default()),
                condition: RangeConstraint::from_range(7.into(), 20.into()),
            },
            vec![assignment(&y, symbol(&x) + number(1))],
            vec![assignment(&y, symbol(&x) + number(2))],
        )];
        let f = compile_effects(1, &[x], &effects).unwrap();
        let mut data = vec![];
        let mut known = vec![];

        let mut params = vec![LookupCell::Input(&x_val), LookupCell::Output(&mut y_val)];
        let params = witgen_fun_params_with_params(&mut data, &mut known, &mut params);
        (f.function)(params);
        assert_eq!(y_val, GoldilocksField::from(8));

        x_val = 2.into();
        let mut params = vec![LookupCell::Input(&x_val), LookupCell::Output(&mut y_val)];
        let params = witgen_fun_params_with_params(&mut data, &mut known, &mut params);
        (f.function)(params);
        assert_eq!(y_val, GoldilocksField::from(4));
    }

    #[test]
    fn branches_codegen() {
        let x = param(0);
        let y = param(1);
        let z = param(2);
        let branch_effect = Effect::Branch(
            Condition {
                value: SymbolicExpression::from_symbol(x.clone(), Default::default()),
                condition: RangeConstraint::from_range(7.into(), 20.into()),
            },
            vec![assignment(&y, symbol(&x) + number(1))],
            vec![Effect::Branch(
                Condition {
                    value: SymbolicExpression::from_symbol(z.clone(), Default::default()),
                    condition: RangeConstraint::from_range(7.into(), 20.into()),
                },
                vec![assignment(&y, symbol(&x) + number(2))],
                vec![assignment(&y, symbol(&x) + number(3))],
            )],
        );
        let expectation = "    let mut p_1 = FieldElement::default();
    if { let v = IntType::from(p_0); 7 <= v && v <= 20 } {
        p_1 = (p_0 + FieldElement::from(1));
    } else if { let v = IntType::from(p_2); 7 <= v && v <= 20 } {
        p_1 = (p_0 + FieldElement::from(2));
    } else {
        p_1 = (p_0 + FieldElement::from(3));
    }";
        assert_eq!(format_effects(&[branch_effect]), expectation);
    }

    #[test]
    fn handle_query_prover_function() {
        fn to_algebraic_ref(name: &str, id: u64) -> AlgebraicReference {
            AlgebraicReference {
                name: name.to_string(),
                poly_id: PolyID {
                    id,
                    ptype: PolynomialType::Committed,
                },
                next: false,
            }
        }

        let x = cell("x", 0, 0);
        let y = cell("y", 1, 0);
        let z = cell("z", 2, 0);
        let effects = vec![Effect::ProverFunctionCall(ProverFunctionCall {
            targets: vec![Some(x.clone())],
            function_index: 0,
            row_offset: 0,
            inputs: vec![y.clone(), z.clone()],
        })];
        let known_inputs = vec![y.clone(), z.clone()];
        let prover_function = ProverFunction {
            index: 0,
            target: vec![to_algebraic_ref("x", 0)],
            compute_multi: false,
            computation: ProverFunctionComputation::HandleQueryInputOutput(
                [(7, QueryType::Input), (8, QueryType::Output)]
                    .into_iter()
                    .collect(),
            ),
            condition: None,
            input_columns: vec![to_algebraic_ref("y", 1), to_algebraic_ref("z", 2)],
        };
        let f = super::compile_effects(
            &NoDefinitions,
            ColumnLayout {
                column_count: 3,
                first_column_id: 0,
            },
            &known_inputs,
            &effects,
            vec![prover_function],
        )
        .unwrap();

        let mut data = vec![
            GoldilocksField::from(0),
            GoldilocksField::from(7),
            GoldilocksField::from(2),
        ];
        let mut known = vec![0; 1];
        (f.function)(witgen_fun_params(&mut data, &mut known));
        assert_eq!(data[0], GoldilocksField::from(117));
        assert_eq!(data[1], GoldilocksField::from(7));
        assert_eq!(data[2], GoldilocksField::from(2));

        let mut data = vec![
            GoldilocksField::from(0),
            GoldilocksField::from(8),
            GoldilocksField::from(2),
        ];
        let mut known = vec![0; 1];
        (f.function)(witgen_fun_params(&mut data, &mut known));
        assert_eq!(data[0], GoldilocksField::from(0));
        assert_eq!(data[1], GoldilocksField::from(8));
        assert_eq!(data[2], GoldilocksField::from(2));

        let mut data = vec![
            GoldilocksField::from(0),
            GoldilocksField::from(9),
            GoldilocksField::from(2),
        ];
        let mut known = vec![0; 1];
        (f.function)(witgen_fun_params(&mut data, &mut known));
        assert_eq!(data[0], GoldilocksField::from(0));
        assert_eq!(data[1], GoldilocksField::from(9));
        assert_eq!(data[2], GoldilocksField::from(2));
    }
}
