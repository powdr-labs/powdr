use std::iter::once;

use analysis::utils::parse_pil_statement;
use ast::{
    object::{Location, PILGraph},
    parsed::{
        build::{direct_reference, namespaced_reference},
        Expression, PILFile, PilStatement, SelectedExpressions,
    },
};
use number::FieldElement;

const DEFAULT_DEGREE: u64 = 1024;
const MAIN_OPERATION_NAME: &str = "main";

/// a monolithic linker which outputs a single AIR
/// It sets the degree of submachines to the degree of the main machine, and errors out if a submachine has an explicit degree which doesn't match the main one
pub fn link<T: FieldElement>(graph: PILGraph<T>) -> Result<PILFile<T>, Vec<String>> {
    let main_machine = graph.main;
    let main_degree = graph
        .objects
        .get(&main_machine.location)
        .unwrap()
        .degree
        .unwrap_or(DEFAULT_DEGREE);

    let mut errors = vec![];

    let pil = graph
        .objects
        .into_iter()
        .flat_map(|(location, object)| {
            let mut pil = vec![];

            if let Some(degree) = object.degree {
                if degree != main_degree {
                    errors.push(format!(
                        "Machine {location} should have degree {main_degree}, found {}",
                        degree
                    ))
                }
            }

            // create a namespace for this object
            pil.push(PilStatement::Namespace(
                0,
                location.to_string(),
                Expression::Number(T::from(main_degree)),
            ));
            pil.extend(object.pil);
            for link in object.links {
                // add the link to this namespace as a lookup

                let from = link.from;
                let to = link.to;

                // the lhs is `instr_flag { inputs, outputs }`
                let lhs = SelectedExpressions {
                    selector: Some(from.flag),
                    expressions: once(Expression::Number(to.operation.id))
                        .chain(
                            from.params
                                .inputs
                                .params
                                .into_iter()
                                .chain(
                                    from.params
                                        .outputs
                                        .into_iter()
                                        .flat_map(|o| o.params.into_iter()),
                                )
                                .map(|i| {
                                    assert!(i.ty.is_none());
                                    i.name
                                })
                                .map(|i| direct_reference(i)),
                        )
                        .collect(),
                };
                // the rhs is `(instr_flag * latch) { inputs, outputs }`
                // get the instruction in the submachine

                let params = to.operation.params;

                let to_namespace = to.machine.location.clone().to_string();

                let rhs = SelectedExpressions {
                    selector: Some(namespaced_reference(to_namespace.clone(), to.machine.latch)),
                    expressions: once(namespaced_reference(
                        to_namespace.clone(),
                        to.machine.operation_id,
                    ))
                    .chain(
                        params
                            .inputs
                            .params
                            .iter()
                            .chain(params.outputs.iter().flat_map(|o| o.params.iter()))
                            .map(|i| namespaced_reference(to_namespace.clone(), i.name.clone())),
                    )
                    .collect(),
                };

                let lookup = PilStatement::PlookupIdentity(0, lhs, rhs);
                pil.push(lookup);
            }

            if location == Location::main() {
                if let Some(main_operation) = graph
                    .entry_points
                    .iter()
                    .find(|f| f.name == MAIN_OPERATION_NAME)
                {
                    let main_operation_id = main_operation.id;
                    let operation_id = main_machine.operation_id.clone();
                    // call the main operation by initialising `operation_id` to that of the main operation
                    let linker_first_step = "_linker_first_step";
                    pil.extend([
                        parse_pil_statement(&format!("col fixed {linker_first_step} = [1] + [0]*")),
                        parse_pil_statement(&format!(
                            "{linker_first_step} * ({operation_id} - {main_operation_id}) = 0"
                        )),
                    ]);
                }
            }

            pil
        })
        .collect();

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(PILFile(pil))
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use ast::{
        object::{Location, Object, PILGraph},
        parsed::{Expression, PILFile},
    };
    use number::{Bn254Field, FieldElement, GoldilocksField};

    use analysis::analyze;
    use parser::parse_asm;

    use pretty_assertions::assert_eq;

    use crate::{link, DEFAULT_DEGREE};

    fn parse_analyse_and_compile<T: FieldElement>(input: &str) -> PILGraph<T> {
        airgen::compile(analyze(parse_asm(None, input).unwrap()).unwrap())
    }

    #[test]
    fn degree() {
        // a graph with two objects of degree `main_degree` and `foo_degree`
        let test_graph = |main_degree, foo_degree| PILGraph {
            main: ast::object::Machine {
                location: Location::main(),
                operation_id: "operation_id".into(),
                latch: "latch".into(),
            },
            entry_points: vec![],
            objects: [
                (Location::main(), Object::default().with_degree(main_degree)),
                (
                    Location::main().join("foo"),
                    Object::default().with_degree(foo_degree),
                ),
            ]
            .into_iter()
            .collect(),
        };
        // a test over a pil file `f` checking if all namespaces have degree `n`
        let all_namespaces_have_degree = |f: PILFile<Bn254Field>, n| {
            f.0.iter().all(|s| match s {
                ast::parsed::PilStatement::Namespace(_, _, e) => {
                    *e == Expression::Number(Bn254Field::from(n))
                }
                _ => true,
            })
        };

        let inferred: PILGraph<Bn254Field> = test_graph(Some(8), None);
        assert!(all_namespaces_have_degree(link(inferred).unwrap(), 8));
        let matches: PILGraph<Bn254Field> = test_graph(Some(8), Some(8));
        assert!(all_namespaces_have_degree(link(matches).unwrap(), 8));
        let default_infer: PILGraph<Bn254Field> = test_graph(None, Some(DEFAULT_DEGREE));
        assert!(all_namespaces_have_degree(
            link(default_infer).unwrap(),
            1024
        ));
        let default_no_match: PILGraph<Bn254Field> = test_graph(None, Some(8));
        assert_eq!(
            link(default_no_match),
            Err(vec![
                "Machine main_foo should have degree 1024, found 8".to_string()
            ])
        );
    }

    #[test]
    fn compile_empty_vm() {
        let expectation = r#"
        namespace main(8);
pol commit _operation_id;
pol commit _sigma;
pol constant _romgen_first_step = [1] + [0]*;
_sigma' = ((1 - _romgen_first_step') * (_sigma + instr_return));
(_sigma * (_operation_id - 2)) = 0;
pol commit pc;
pol commit instr__jump_to_operation;
pol commit instr__reset;
pol commit instr__loop;
pol commit instr_return;
pol constant first_step = [1] + [0]*;
pc' = ((1 - first_step') * ((((instr__jump_to_operation * _operation_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1))));
pol constant p_line = [0, 1, 2] + [2]*;
pol constant p_instr__jump_to_operation = [0, 1, 0] + [0]*;
pol constant p_instr__loop = [0, 0, 1] + [1]*;
pol constant p_instr__reset = [1, 0, 0] + [0]*;
pol constant p_instr_return = [0, 0, 0] + [0]*;
{ pc, instr__jump_to_operation, instr__reset, instr__loop, instr_return } in { p_line, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return };
pol constant _block_enforcer_last_step = [0]* + [1];
pol commit _operation_id_no_change;
_operation_id_no_change = ((1 - _block_enforcer_last_step) * (1 - instr_return));
(_operation_id_no_change * (_operation_id' - _operation_id)) = 0;
        "#;

        let file_name = "../test_data/asm/empty_vm.asm";
        let contents = fs::read_to_string(file_name).unwrap();
        let graph = parse_analyse_and_compile::<GoldilocksField>(&contents);
        let pil = link(graph).unwrap();
        assert_eq!(format!("{pil}").trim(), expectation.trim());
    }

    #[test]
    fn compile_different_signatures() {
        let expectation = r#"
        namespace main(16);
pol commit _operation_id;
pol commit _sigma;
pol constant _romgen_first_step = [1] + [0]*;
_sigma' = ((1 - _romgen_first_step') * (_sigma + instr_return));
(_sigma * (_operation_id - 4)) = 0;
pol commit pc;
pol commit X;
pol commit Y;
pol commit reg_write_X_A;
pol commit reg_write_Y_A;
pol commit A;
pol commit instr_identity;
pol commit instr_one;
pol commit instr_nothing;
pol commit instr__jump_to_operation;
pol commit instr__reset;
pol commit instr__loop;
pol commit instr_return;
pol commit X_const;
pol commit X_read_free;
pol commit read_X_A;
pol commit read_X_pc;
X = ((((read_X_A * A) + (read_X_pc * pc)) + X_const) + (X_read_free * X_free_value));
pol commit Y_const;
pol commit Y_read_free;
pol commit read_Y_A;
pol commit read_Y_pc;
Y = ((((read_Y_A * A) + (read_Y_pc * pc)) + Y_const) + (Y_read_free * Y_free_value));
pol constant first_step = [1] + [0]*;
A' = ((((reg_write_X_A * X) + (reg_write_Y_A * Y)) + (instr__reset * 0)) + ((1 - ((reg_write_X_A + reg_write_Y_A) + instr__reset)) * A));
pc' = ((1 - first_step') * ((((instr__jump_to_operation * _operation_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1))));
pol constant p_line = [0, 1, 2, 3, 4] + [4]*;
pol commit X_free_value(i) query match pc {  };
pol commit Y_free_value(i) query match pc {  };
pol constant p_X_const = [0, 0, 0, 0, 0] + [0]*;
pol constant p_X_read_free = [0, 0, 0, 0, 0] + [0]*;
pol constant p_Y_const = [0, 0, 0, 0, 0] + [0]*;
pol constant p_Y_read_free = [0, 0, 1, 0, 0] + [0]*;
pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0] + [0]*;
pol constant p_instr__loop = [0, 0, 0, 0, 1] + [1]*;
pol constant p_instr__reset = [1, 0, 0, 0, 0] + [0]*;
pol constant p_instr_identity = [0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_nothing = [0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_one = [0, 0, 1, 0, 0] + [0]*;
pol constant p_instr_return = [0, 0, 0, 1, 0] + [0]*;
pol constant p_read_X_A = [0, 0, 0, 0, 0] + [0]*;
pol constant p_read_X_pc = [0, 0, 0, 0, 0] + [0]*;
pol constant p_read_Y_A = [0, 0, 0, 0, 0] + [0]*;
pol constant p_read_Y_pc = [0, 0, 0, 0, 0] + [0]*;
pol constant p_reg_write_X_A = [0, 0, 0, 0, 0] + [0]*;
pol constant p_reg_write_Y_A = [0, 0, 1, 0, 0] + [0]*;
{ pc, reg_write_X_A, reg_write_Y_A, instr_identity, instr_one, instr_nothing, instr__jump_to_operation, instr__reset, instr__loop, instr_return, X_const, X_read_free, read_X_A, read_X_pc, Y_const, Y_read_free, read_Y_A, read_Y_pc } in { p_line, p_reg_write_X_A, p_reg_write_Y_A, p_instr_identity, p_instr_one, p_instr_nothing, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p_X_const, p_X_read_free, p_read_X_A, p_read_X_pc, p_Y_const, p_Y_read_free, p_read_Y_A, p_read_Y_pc };
pol constant _block_enforcer_last_step = [0]* + [1];
pol commit _operation_id_no_change;
_operation_id_no_change = ((1 - _block_enforcer_last_step) * (1 - instr_return));
(_operation_id_no_change * (_operation_id' - _operation_id)) = 0;
instr_identity { 2, X, Y } in main_sub.instr_return { main_sub._operation_id, main_sub._input_0, main_sub._output_0 };
instr_one { 3, Y } in main_sub.instr_return { main_sub._operation_id, main_sub._output_0 };
instr_nothing { 4 } in main_sub.instr_return { main_sub._operation_id };
pol constant _linker_first_step = [1] + [0]*;
(_linker_first_step * (_operation_id - 2)) = 0;
namespace main_sub(16);
pol commit _operation_id;
pol commit _sigma;
pol constant _romgen_first_step = [1] + [0]*;
_sigma' = ((1 - _romgen_first_step') * (_sigma + instr_return));
(_sigma * (_operation_id - 5)) = 0;
pol commit pc;
pol commit _input_0;
pol commit _output_0;
pol commit instr__jump_to_operation;
pol commit instr__reset;
pol commit instr__loop;
pol commit instr_return;
pol commit _output_0_const;
pol commit _output_0_read_free;
pol commit read__output_0_pc;
pol commit read__output_0__input_0;
_output_0 = ((((read__output_0_pc * pc) + (read__output_0__input_0 * _input_0)) + _output_0_const) + (_output_0_read_free * _output_0_free_value));
pol constant first_step = [1] + [0]*;
((1 - instr__reset) * _input_0') = ((1 - instr__reset) * _input_0);
pc' = ((1 - first_step') * ((((instr__jump_to_operation * _operation_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1))));
pol constant p_line = [0, 1, 2, 3, 4, 5] + [5]*;
pol commit _output_0_free_value(i) query match pc {  };
pol constant p__output_0_const = [0, 0, 0, 1, 0, 0] + [0]*;
pol constant p__output_0_read_free = [0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0, 0] + [0]*;
pol constant p_instr__loop = [0, 0, 0, 0, 0, 1] + [1]*;
pol constant p_instr__reset = [1, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_return = [0, 0, 1, 1, 1, 0] + [0]*;
pol constant p_read__output_0__input_0 = [0, 0, 1, 0, 0, 0] + [0]*;
pol constant p_read__output_0_pc = [0, 0, 0, 0, 0, 0] + [0]*;
{ pc, instr__jump_to_operation, instr__reset, instr__loop, instr_return, _output_0_const, _output_0_read_free, read__output_0_pc, read__output_0__input_0 } in { p_line, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p__output_0_const, p__output_0_read_free, p_read__output_0_pc, p_read__output_0__input_0 };
pol constant _block_enforcer_last_step = [0]* + [1];
pol commit _operation_id_no_change;
_operation_id_no_change = ((1 - _block_enforcer_last_step) * (1 - instr_return));
(_operation_id_no_change * (_operation_id' - _operation_id)) = 0;
        "#;

        let file_name = "../test_data/asm/different_signatures.asm";
        let contents = fs::read_to_string(file_name).unwrap();
        let graph = parse_analyse_and_compile::<GoldilocksField>(&contents);
        let pil = link(graph).unwrap();
        assert_eq!(format!("{pil}").trim(), expectation.trim());
    }

    #[test]
    fn compile_simple_sum() {
        let expectation = r#"
namespace main(1024);
pol commit XInv;
pol commit XIsZero;
XIsZero = (1 - (X * XInv));
(XIsZero * X) = 0;
(XIsZero * (1 - XIsZero)) = 0;
pol commit _operation_id;
pol commit _sigma;
pol constant _romgen_first_step = [1] + [0]*;
_sigma' = ((1 - _romgen_first_step') * (_sigma + instr_return));
(_sigma * (_operation_id - 10)) = 0;
pol commit pc;
pol commit X;
pol commit reg_write_X_A;
pol commit A;
pol commit reg_write_X_CNT;
pol commit CNT;
pol commit instr_jmpz;
pol commit instr_jmpz_param_l;
pol commit instr_jmp;
pol commit instr_jmp_param_l;
pol commit instr_dec_CNT;
pol commit instr_assert_zero;
(instr_assert_zero * (XIsZero - 1)) = 0;
pol commit instr__jump_to_operation;
pol commit instr__reset;
pol commit instr__loop;
pol commit instr_return;
pol commit X_const;
pol commit X_read_free;
pol commit read_X_A;
pol commit read_X_CNT;
pol commit read_X_pc;
X = (((((read_X_A * A) + (read_X_CNT * CNT)) + (read_X_pc * pc)) + X_const) + (X_read_free * X_free_value));
pol constant first_step = [1] + [0]*;
A' = (((reg_write_X_A * X) + (instr__reset * 0)) + ((1 - (reg_write_X_A + instr__reset)) * A));
CNT' = ((((reg_write_X_CNT * X) + (instr_dec_CNT * (CNT - 1))) + (instr__reset * 0)) + ((1 - ((reg_write_X_CNT + instr_dec_CNT) + instr__reset)) * CNT));
pc' = ((1 - first_step') * ((((((instr_jmpz * ((XIsZero * instr_jmpz_param_l) + ((1 - XIsZero) * (pc + 1)))) + (instr_jmp * instr_jmp_param_l)) + (instr__jump_to_operation * _operation_id)) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((((instr_jmpz + instr_jmp) + instr__jump_to_operation) + instr__loop) + instr_return)) * (pc + 1))));
pol constant p_line = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] + [10]*;
pol commit X_free_value(i) query match pc { 2 => ("input", 1), 4 => ("input", (CNT + 1)), 7 => ("input", 0), };
pol constant p_X_const = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_X_read_free = [0, 0, 1, 0, 1, 0, 0, -1, 0, 0, 0] + [0]*;
pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr__loop = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1] + [1]*;
pol constant p_instr__reset = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_assert_zero = [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0] + [0]*;
pol constant p_instr_dec_CNT = [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_jmp = [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0] + [0]*;
pol constant p_instr_jmp_param_l = [0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0] + [0]*;
pol constant p_instr_jmpz = [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_jmpz_param_l = [0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_return = [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0] + [0]*;
pol constant p_read_X_A = [0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0] + [0]*;
pol constant p_read_X_CNT = [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_read_X_pc = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_reg_write_X_A = [0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0] + [0]*;
pol constant p_reg_write_X_CNT = [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
{ pc, reg_write_X_A, reg_write_X_CNT, instr_jmpz, instr_jmpz_param_l, instr_jmp, instr_jmp_param_l, instr_dec_CNT, instr_assert_zero, instr__jump_to_operation, instr__reset, instr__loop, instr_return, X_const, X_read_free, read_X_A, read_X_CNT, read_X_pc } in { p_line, p_reg_write_X_A, p_reg_write_X_CNT, p_instr_jmpz, p_instr_jmpz_param_l, p_instr_jmp, p_instr_jmp_param_l, p_instr_dec_CNT, p_instr_assert_zero, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p_X_const, p_X_read_free, p_read_X_A, p_read_X_CNT, p_read_X_pc };
pol constant _block_enforcer_last_step = [0]* + [1];
pol commit _operation_id_no_change;
_operation_id_no_change = ((1 - _block_enforcer_last_step) * (1 - instr_return));
(_operation_id_no_change * (_operation_id' - _operation_id)) = 0;
pol constant _linker_first_step = [1] + [0]*;
(_linker_first_step * (_operation_id - 2)) = 0;

"#;
        let file_name = "../test_data/asm/simple_sum.asm";
        let contents = fs::read_to_string(file_name).unwrap();
        let graph = parse_analyse_and_compile::<GoldilocksField>(&contents);
        let pil = link(graph).unwrap();
        assert_eq!(format!("{pil}").trim(), expectation.trim());
    }

    #[test]
    fn compile_literal_number_args() {
        let source = r#"
machine Machine {
    reg pc[@pc];
    reg fp;

    instr inc_fp amount: unsigned { fp' = fp + amount }
    instr adjust_fp amount: signed, t: label { fp' = fp + amount, pc' = label }

    function main {
        inc_fp 7;
        loop::
        adjust_fp -2, loop;
    }
}
"#;
        let expectation = r#"
namespace main(1024);
pol commit _operation_id;
pol commit _sigma;
pol constant _romgen_first_step = [1] + [0]*;
_sigma' = ((1 - _romgen_first_step') * (_sigma + instr_return));
(_sigma * (_operation_id - 4)) = 0;
pol commit pc;
pol commit fp;
pol commit instr_inc_fp;
pol commit instr_inc_fp_param_amount;
pol commit instr_adjust_fp;
pol commit instr_adjust_fp_param_amount;
pol commit instr_adjust_fp_param_t;
pol commit instr__jump_to_operation;
pol commit instr__reset;
pol commit instr__loop;
pol commit instr_return;
pol constant first_step = [1] + [0]*;
fp' = ((((instr_inc_fp * (fp + instr_inc_fp_param_amount)) + (instr_adjust_fp * (fp + instr_adjust_fp_param_amount))) + (instr__reset * 0)) + ((1 - ((instr_inc_fp + instr_adjust_fp) + instr__reset)) * fp));
pc' = ((1 - first_step') * (((((instr_adjust_fp * label) + (instr__jump_to_operation * _operation_id)) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - (((instr_adjust_fp + instr__jump_to_operation) + instr__loop) + instr_return)) * (pc + 1))));
pol constant p_line = [0, 1, 2, 3, 4] + [4]*;
pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0] + [0]*;
pol constant p_instr__loop = [0, 0, 0, 0, 1] + [1]*;
pol constant p_instr__reset = [1, 0, 0, 0, 0] + [0]*;
pol constant p_instr_adjust_fp = [0, 0, 0, 1, 0] + [0]*;
pol constant p_instr_adjust_fp_param_amount = [0, 0, 0, -2, 0] + [0]*;
pol constant p_instr_adjust_fp_param_t = [0, 0, 0, 3, 0] + [0]*;
pol constant p_instr_inc_fp = [0, 0, 1, 0, 0] + [0]*;
pol constant p_instr_inc_fp_param_amount = [0, 0, 7, 0, 0] + [0]*;
pol constant p_instr_return = [0, 0, 0, 0, 0] + [0]*;
{ pc, instr_inc_fp, instr_inc_fp_param_amount, instr_adjust_fp, instr_adjust_fp_param_amount, instr_adjust_fp_param_t, instr__jump_to_operation, instr__reset, instr__loop, instr_return } in { p_line, p_instr_inc_fp, p_instr_inc_fp_param_amount, p_instr_adjust_fp, p_instr_adjust_fp_param_amount, p_instr_adjust_fp_param_t, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return };
pol constant _block_enforcer_last_step = [0]* + [1];
pol commit _operation_id_no_change;
_operation_id_no_change = ((1 - _block_enforcer_last_step) * (1 - instr_return));
(_operation_id_no_change * (_operation_id' - _operation_id)) = 0;
pol constant _linker_first_step = [1] + [0]*;
(_linker_first_step * (_operation_id - 2)) = 0;
"#;
        let graph = parse_analyse_and_compile::<GoldilocksField>(source);
        let pil = link(graph).unwrap();
        assert_eq!(format!("{pil}").trim(), expectation.trim());
    }

    #[test]
    #[should_panic(expected = "Number passed to unsigned parameter is negative or too large")]
    pub fn negative_for_unsigned() {
        let source = r#"
machine NegativeForUnsigned {
    reg pc[@pc];
    reg fp;
    
    instr instro x: unsigned { pc' = pc + x }
    
    function main {
        instro 9223372034707292161;
    }
}
"#;
        let graph = parse_analyse_and_compile::<GoldilocksField>(source);
        let _ = link(graph);
    }
}
