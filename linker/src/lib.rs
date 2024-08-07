#![deny(clippy::print_stdout)]

use std::collections::BTreeMap;

use powdr_analysis::utils::parse_pil_statement;
use powdr_ast::{
    object::{Link, Location, PILGraph, TypeOrExpression},
    parsed::{
        asm::{AbsoluteSymbolPath, SymbolPath},
        build::{index_access, namespaced_reference},
        PILFile, PilStatement, SelectedExpressions, TypedExpression,
    },
};
use powdr_parser_util::SourceRef;

use itertools::Itertools;

const DEFAULT_DEGREE: u32 = 1024;
const MAIN_OPERATION_NAME: &str = "main";

/// a monolithic linker which outputs a single AIR
/// It sets the degree of submachines to the degree of the main machine, and errors out if a submachine has an explicit degree which doesn't match the main one
pub fn link(graph: PILGraph) -> Result<PILFile, Vec<String>> {
    let main_machine = graph.main;
    let main_degree = graph
        .objects
        .get(&main_machine.location)
        .unwrap()
        .degree
        .clone()
        .unwrap_or_else(|| DEFAULT_DEGREE.into());

    let mut errors = vec![];

    let mut pil = process_definitions(graph.definitions);

    for (location, object) in graph.objects.into_iter() {
        if let Some(degree) = object.degree {
            if degree != main_degree {
                errors.push(format!(
                    "Machine {location} should have degree {main_degree}, found {degree}"
                ))
            }
        }

        // create a namespace for this object
        pil.push(PilStatement::Namespace(
            SourceRef::unknown(),
            SymbolPath::from_identifier(location.to_string()),
            Some(main_degree.clone()),
        ));

        pil.extend(object.pil);
        pil.extend(object.links.into_iter().map(process_link));

        if location == Location::main() {
            if let Some(main_operation) = graph
                .entry_points
                .iter()
                .find(|f| f.name == MAIN_OPERATION_NAME)
            {
                let main_operation_id = main_operation.id.clone();
                let operation_id = main_machine.operation_id.clone();
                match (operation_id, main_operation_id) {
                    (Some(operation_id), Some(main_operation_id)) => {
                        // call the main operation by initializing `operation_id` to that of the main operation
                        let linker_first_step = "_linker_first_step";
                        pil.extend([
                            parse_pil_statement(&format!(
                                "col fixed {linker_first_step} = [1] + [0]*;"
                            )),
                            parse_pil_statement(&format!(
                                "{linker_first_step} * ({operation_id} - {main_operation_id}) = 0;"
                            )),
                        ]);
                    }
                    (None, None) => {}
                    _ => unreachable!(),
                }
            }
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(PILFile(pil))
    }
}

// Extract the utilities and sort them into namespaces where possible.
fn process_definitions(
    definitions: BTreeMap<AbsoluteSymbolPath, TypeOrExpression>,
) -> Vec<PilStatement> {
    let mut current_namespace = Default::default();
    definitions
        .into_iter()
        .sorted_by_cached_key(|(namespace, _)| {
            let mut namespace = namespace.clone();
            let name = namespace.pop();
            // Group by namespace and then sort by name.
            (namespace, name)
        })
        .flat_map(|(mut namespace, type_or_expr)| {
            let name = namespace.pop().unwrap();
            let statement = match type_or_expr {
                TypeOrExpression::Expression(TypedExpression { e, type_scheme }) => {
                    PilStatement::LetStatement(
                        SourceRef::unknown(),
                        name.to_string(),
                        type_scheme,
                        Some(e),
                    )
                }
                TypeOrExpression::Type(enum_decl) => {
                    PilStatement::EnumDeclaration(SourceRef::unknown(), enum_decl)
                }
            };

            // If there is a namespace change, insert a namespace statement.
            if current_namespace != namespace {
                current_namespace = namespace.clone();
                vec![
                    PilStatement::Namespace(
                        SourceRef::unknown(),
                        namespace.relative_to(&AbsoluteSymbolPath::default()),
                        None,
                    ),
                    statement,
                ]
            } else {
                vec![statement]
            }
        })
        .collect::<Vec<_>>()
}

fn process_link(link: Link) -> PilStatement {
    let from = link.from;
    let to = link.to;

    // the lhs is `instr_flag { operation_id, inputs, outputs }`
    let op_id = to.operation.id.iter().cloned().map(|n| n.into());

    if link.is_permutation {
        // permutation lhs is `flag { operation_id, inputs, outputs }`
        let lhs = SelectedExpressions {
            selector: Some(from.flag),
            expressions: op_id
                .chain(from.params.inputs)
                .chain(from.params.outputs)
                .collect(),
        };

        // permutation rhs is `(latch * selector[idx]) { operation_id, inputs, outputs }`
        let to_namespace = to.machine.location.clone().to_string();
        let op_id = to
            .machine
            .operation_id
            .map(|oid| namespaced_reference(to_namespace.clone(), oid))
            .into_iter();

        let latch = namespaced_reference(to_namespace.clone(), to.machine.latch.unwrap());
        let rhs_selector = if let Some(call_selectors) = to.machine.call_selectors {
            let call_selector_array = namespaced_reference(to_namespace.clone(), call_selectors);
            let call_selector =
                index_access(call_selector_array, Some(to.selector_idx.unwrap().into()));
            Some(latch * call_selector)
        } else {
            Some(latch)
        };

        let rhs = SelectedExpressions {
            selector: rhs_selector,
            expressions: op_id
                .chain(to.operation.params.inputs_and_outputs().map(|i| {
                    index_access(
                        namespaced_reference(to_namespace.clone(), &i.name),
                        i.index.clone(),
                    )
                }))
                .collect(),
        };
        PilStatement::PermutationIdentity(SourceRef::unknown(), lhs, rhs)
    } else {
        // plookup lhs is `flag { operation_id, inputs, outputs }`
        let lhs = SelectedExpressions {
            selector: Some(from.flag),
            expressions: op_id
                .chain(from.params.inputs)
                .chain(from.params.outputs)
                .collect(),
        };

        let to_namespace = to.machine.location.clone().to_string();
        let op_id = to
            .machine
            .operation_id
            .map(|oid| namespaced_reference(to_namespace.clone(), oid))
            .into_iter();

        // plookup rhs is `latch { operation_id, inputs, outputs }`
        let latch = Some(namespaced_reference(
            to_namespace.clone(),
            to.machine.latch.unwrap(),
        ));

        let rhs = SelectedExpressions {
            selector: latch,
            expressions: op_id
                .chain(to.operation.params.inputs_and_outputs().map(|i| {
                    index_access(
                        namespaced_reference(to_namespace.clone(), &i.name),
                        i.index.clone(),
                    )
                }))
                .collect(),
        };
        PilStatement::PlookupIdentity(SourceRef::unknown(), lhs, rhs)
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use powdr_ast::{
        object::{Location, Object, PILGraph},
        parsed::PILFile,
    };
    use powdr_number::{BigUint, FieldElement, GoldilocksField};

    use powdr_analysis::convert_asm_to_pil;
    use powdr_parser::parse_asm;

    use pretty_assertions::assert_eq;

    use crate::{link, DEFAULT_DEGREE};

    fn parse_analyze_and_compile<T: FieldElement>(input: &str) -> PILGraph {
        let parsed = parse_asm(None, input).unwrap();
        let resolved = powdr_importer::load_dependencies_and_resolve(None, parsed).unwrap();
        powdr_airgen::compile(convert_asm_to_pil::<T>(resolved).unwrap())
    }

    #[test]
    fn degree() {
        // a graph with two objects of degree `main_degree` and `foo_degree`
        let test_graph = |main_degree, foo_degree| PILGraph {
            main: powdr_ast::object::Machine {
                location: Location::main(),
                operation_id: Some("operation_id".into()),
                latch: Some("latch".into()),
                call_selectors: None,
            },
            entry_points: vec![],
            definitions: Default::default(),
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
        // a test over a pil file `f` checking if all namespaces have degree `n` (if they are set)
        let all_namespaces_have_degree = |f: PILFile, n: u64| {
            f.0.iter().all(|s| match s {
                powdr_ast::parsed::PilStatement::Namespace(_, _, Some(e)) => {
                    *e == BigUint::from(n).into()
                }
                _ => true,
            })
        };

        let inferred: PILGraph = test_graph(Some(8), None);
        assert!(all_namespaces_have_degree(link(inferred).unwrap(), 8));
        let matches: PILGraph = test_graph(Some(8), Some(8));
        assert!(all_namespaces_have_degree(link(matches).unwrap(), 8));
        let default_infer: PILGraph = test_graph(None, Some(DEFAULT_DEGREE));
        assert!(all_namespaces_have_degree(
            link(default_infer).unwrap(),
            1024
        ));
        let default_no_match: PILGraph = test_graph(None, Some(8));
        assert_eq!(
            link(default_no_match),
            Err(vec![
                "Machine main_foo should have degree 1024, found 8".to_string()
            ])
        );
    }

    fn extract_main(code: &str) -> &str {
        let start = code.find("namespace main").unwrap();
        &code[start..]
    }

    #[test]
    fn compile_empty_vm() {
        let expectation = r#"namespace main(4 + 4);
    pol commit _operation_id(i) query std::prover::Query::Hint(2);
    pol constant _block_enforcer_last_step = [0]* + [1];
    let _operation_id_no_change = (1 - _block_enforcer_last_step) * (1 - instr_return);
    _operation_id_no_change * (_operation_id' - _operation_id) = 0;
    pol commit pc;
    pol commit instr__jump_to_operation;
    pol commit instr__reset;
    pol commit instr__loop;
    pol commit instr_return;
    pol constant first_step = [1] + [0]*;
    pol pc_update = instr__jump_to_operation * _operation_id + instr__loop * pc + instr_return * 0 + (1 - (instr__jump_to_operation + instr__loop + instr_return)) * (pc + 1);
    pc' = (1 - first_step') * pc_update;
    pol constant p_line = [0, 1, 2] + [2]*;
    pol constant p_instr__jump_to_operation = [0, 1, 0] + [0]*;
    pol constant p_instr__loop = [0, 0, 1] + [1]*;
    pol constant p_instr__reset = [1, 0, 0] + [0]*;
    pol constant p_instr_return = [0]*;
    { pc, instr__jump_to_operation, instr__reset, instr__loop, instr_return } in { p_line, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return };
"#;

        let file_name = format!(
            "{}/../test_data/asm/empty_vm.asm",
            env!("CARGO_MANIFEST_DIR")
        );
        let contents = fs::read_to_string(file_name).unwrap();
        let graph = parse_analyze_and_compile::<GoldilocksField>(&contents);
        let pil = link(graph).unwrap();
        assert_eq!(extract_main(&format!("{pil}")), expectation);
    }

    #[test]
    fn compile_really_empty_vm() {
        let expectation = r#"namespace main(1024);
"#;

        let graph = parse_analyze_and_compile::<GoldilocksField>("");
        let pil = link(graph).unwrap();
        assert_eq!(extract_main(&format!("{pil}")), expectation);
    }

    #[test]
    fn compile_pil_without_machine() {
        let input = "    let even = std::array::new(5, (|i| 2 * i));";
        let graph = parse_analyze_and_compile::<GoldilocksField>(input);
        let pil = link(graph).unwrap().to_string();
        assert_eq!(&pil[0..input.len()], input);
    }

    #[test]
    fn compile_different_signatures() {
        let expectation = r#"namespace main(16);
    pol commit _operation_id(i) query std::prover::Query::Hint(4);
    pol constant _block_enforcer_last_step = [0]* + [1];
    let _operation_id_no_change = (1 - _block_enforcer_last_step) * (1 - instr_return);
    _operation_id_no_change * (_operation_id' - _operation_id) = 0;
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
    X = read_X_A * A + read_X_pc * pc + X_const + X_read_free * X_free_value;
    pol commit Y_const;
    pol commit Y_read_free;
    pol commit read_Y_A;
    pol commit read_Y_pc;
    Y = read_Y_A * A + read_Y_pc * pc + Y_const + Y_read_free * Y_free_value;
    pol constant first_step = [1] + [0]*;
    A' = reg_write_X_A * X + reg_write_Y_A * Y + instr__reset * 0 + (1 - (reg_write_X_A + reg_write_Y_A + instr__reset)) * A;
    pol pc_update = instr__jump_to_operation * _operation_id + instr__loop * pc + instr_return * 0 + (1 - (instr__jump_to_operation + instr__loop + instr_return)) * (pc + 1);
    pc' = (1 - first_step') * pc_update;
    pol constant p_line = [0, 1, 2, 3, 4] + [4]*;
    pol commit X_free_value;
    pol commit Y_free_value;
    pol constant p_X_const = [0]*;
    pol constant p_X_read_free = [0]*;
    pol constant p_Y_const = [0]*;
    pol constant p_Y_read_free = [0, 0, 1, 0, 0] + [0]*;
    pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0] + [0]*;
    pol constant p_instr__loop = [0, 0, 0, 0, 1] + [1]*;
    pol constant p_instr__reset = [1, 0, 0, 0, 0] + [0]*;
    pol constant p_instr_identity = [0]*;
    pol constant p_instr_nothing = [0]*;
    pol constant p_instr_one = [0, 0, 1, 0, 0] + [0]*;
    pol constant p_instr_return = [0, 0, 0, 1, 0] + [0]*;
    pol constant p_read_X_A = [0]*;
    pol constant p_read_X_pc = [0]*;
    pol constant p_read_Y_A = [0]*;
    pol constant p_read_Y_pc = [0]*;
    pol constant p_reg_write_X_A = [0]*;
    pol constant p_reg_write_Y_A = [0, 0, 1, 0, 0] + [0]*;
    { pc, reg_write_X_A, reg_write_Y_A, instr_identity, instr_one, instr_nothing, instr__jump_to_operation, instr__reset, instr__loop, instr_return, X_const, X_read_free, read_X_A, read_X_pc, Y_const, Y_read_free, read_Y_A, read_Y_pc } in { p_line, p_reg_write_X_A, p_reg_write_Y_A, p_instr_identity, p_instr_one, p_instr_nothing, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p_X_const, p_X_read_free, p_read_X_A, p_read_X_pc, p_Y_const, p_Y_read_free, p_read_Y_A, p_read_Y_pc };
    instr_identity { 2, X, Y } in main_sub.instr_return { main_sub._operation_id, main_sub._input_0, main_sub._output_0 };
    instr_one { 4, Y } in main_sub.instr_return { main_sub._operation_id, main_sub._output_0 };
    instr_nothing { 3 } in main_sub.instr_return { main_sub._operation_id };
    pol constant _linker_first_step = [1] + [0]*;
    _linker_first_step * (_operation_id - 2) = 0;
namespace main_sub(16);
    pol commit _operation_id(i) query std::prover::Query::Hint(5);
    pol constant _block_enforcer_last_step = [0]* + [1];
    let _operation_id_no_change = (1 - _block_enforcer_last_step) * (1 - instr_return);
    _operation_id_no_change * (_operation_id' - _operation_id) = 0;
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
    _output_0 = read__output_0_pc * pc + read__output_0__input_0 * _input_0 + _output_0_const + _output_0_read_free * _output_0_free_value;
    pol constant first_step = [1] + [0]*;
    (1 - instr__reset) * (_input_0' - _input_0) = 0;
    pol pc_update = instr__jump_to_operation * _operation_id + instr__loop * pc + instr_return * 0 + (1 - (instr__jump_to_operation + instr__loop + instr_return)) * (pc + 1);
    pc' = (1 - first_step') * pc_update;
    pol constant p_line = [0, 1, 2, 3, 4, 5] + [5]*;
    pol commit _output_0_free_value;
    pol constant p__output_0_const = [0, 0, 0, 0, 1, 0] + [0]*;
    pol constant p__output_0_read_free = [0]*;
    pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0, 0] + [0]*;
    pol constant p_instr__loop = [0, 0, 0, 0, 0, 1] + [1]*;
    pol constant p_instr__reset = [1, 0, 0, 0, 0, 0] + [0]*;
    pol constant p_instr_return = [0, 0, 1, 1, 1, 0] + [0]*;
    pol constant p_read__output_0__input_0 = [0, 0, 1, 0, 0, 0] + [0]*;
    pol constant p_read__output_0_pc = [0]*;
    { pc, instr__jump_to_operation, instr__reset, instr__loop, instr_return, _output_0_const, _output_0_read_free, read__output_0_pc, read__output_0__input_0 } in { p_line, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p__output_0_const, p__output_0_read_free, p_read__output_0_pc, p_read__output_0__input_0 };
"#;
        let file_name = format!(
            "{}/../test_data/asm/different_signatures.asm",
            env!("CARGO_MANIFEST_DIR")
        );
        let contents = fs::read_to_string(file_name).unwrap();
        let graph = parse_analyze_and_compile::<GoldilocksField>(&contents);
        let pil = link(graph).unwrap();
        assert_eq!(extract_main(&format!("{pil}")), expectation);
    }

    #[test]
    fn compile_simple_sum() {
        let expectation = r#"namespace main(1024);
    pol commit XInv;
    pol commit XIsZero;
    XIsZero = 1 - X * XInv;
    XIsZero * X = 0;
    XIsZero * (1 - XIsZero) = 0;
    pol commit _operation_id(i) query std::prover::Query::Hint(10);
    pol constant _block_enforcer_last_step = [0]* + [1];
    let _operation_id_no_change = (1 - _block_enforcer_last_step) * (1 - instr_return);
    _operation_id_no_change * (_operation_id' - _operation_id) = 0;
    pol commit pc;
    pol commit X;
    pol commit reg_write_X_A;
    pol commit A;
    pol commit reg_write_X_CNT;
    pol commit CNT;
    pol commit instr_jmpz;
    pol commit instr_jmpz_param_l;
    pol instr_jmpz_pc_update = XIsZero * instr_jmpz_param_l;
    pol instr_jmpz_pc_update_1 = (1 - XIsZero) * (pc + 1);
    pol commit instr_jmp;
    pol commit instr_jmp_param_l;
    pol commit instr_dec_CNT;
    pol commit instr_assert_zero;
    instr_assert_zero * (XIsZero - 1) = 0;
    pol commit instr__jump_to_operation;
    pol commit instr__reset;
    pol commit instr__loop;
    pol commit instr_return;
    pol commit X_const;
    pol commit X_read_free;
    pol commit read_X_A;
    pol commit read_X_CNT;
    pol commit read_X_pc;
    X = read_X_A * A + read_X_CNT * CNT + read_X_pc * pc + X_const + X_read_free * X_free_value;
    pol constant first_step = [1] + [0]*;
    A' = reg_write_X_A * X + instr__reset * 0 + (1 - (reg_write_X_A + instr__reset)) * A;
    CNT' = reg_write_X_CNT * X + instr_dec_CNT * (CNT - 1) + instr__reset * 0 + (1 - (reg_write_X_CNT + instr_dec_CNT + instr__reset)) * CNT;
    pol pc_update = instr_jmpz * (instr_jmpz_pc_update + instr_jmpz_pc_update_1) + instr_jmp * instr_jmp_param_l + instr__jump_to_operation * _operation_id + instr__loop * pc + instr_return * 0 + (1 - (instr_jmpz + instr_jmp + instr__jump_to_operation + instr__loop + instr_return)) * (pc + 1);
    pc' = (1 - first_step') * pc_update;
    pol constant p_line = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] + [10]*;
    pol commit X_free_value(__i) query match std::prover::eval(pc) {
        2 => std::prover::Query::Input(1),
        4 => std::prover::Query::Input(std::convert::int(std::prover::eval(CNT) + 1)),
        7 => std::prover::Query::Input(0),
        _ => std::prover::Query::None,
    };
    pol constant p_X_const = [0]*;
    pol constant p_X_read_free = [0, 0, 1, 0, 1, 0, 0, 18446744069414584320, 0, 0, 0] + [0]*;
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
    pol constant p_read_X_pc = [0]*;
    pol constant p_reg_write_X_A = [0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0] + [0]*;
    pol constant p_reg_write_X_CNT = [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    { pc, reg_write_X_A, reg_write_X_CNT, instr_jmpz, instr_jmpz_param_l, instr_jmp, instr_jmp_param_l, instr_dec_CNT, instr_assert_zero, instr__jump_to_operation, instr__reset, instr__loop, instr_return, X_const, X_read_free, read_X_A, read_X_CNT, read_X_pc } in { p_line, p_reg_write_X_A, p_reg_write_X_CNT, p_instr_jmpz, p_instr_jmpz_param_l, p_instr_jmp, p_instr_jmp_param_l, p_instr_dec_CNT, p_instr_assert_zero, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p_X_const, p_X_read_free, p_read_X_A, p_read_X_CNT, p_read_X_pc };
    pol constant _linker_first_step = [1] + [0]*;
    _linker_first_step * (_operation_id - 2) = 0;
"#;
        let file_name = format!(
            "{}/../test_data/asm/simple_sum.asm",
            env!("CARGO_MANIFEST_DIR")
        );
        let contents = fs::read_to_string(file_name).unwrap();
        let graph = parse_analyze_and_compile::<GoldilocksField>(&contents);
        let pil = link(graph).unwrap();
        assert_eq!(extract_main(&format!("{pil}")), expectation);
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
        loop:
        adjust_fp -2, loop;
    }
}
"#;
        let expectation = r#"namespace main(1024);
    pol commit _operation_id(i) query std::prover::Query::Hint(4);
    pol constant _block_enforcer_last_step = [0]* + [1];
    let _operation_id_no_change = (1 - _block_enforcer_last_step) * (1 - instr_return);
    _operation_id_no_change * (_operation_id' - _operation_id) = 0;
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
    fp' = instr_inc_fp * (fp + instr_inc_fp_param_amount) + instr_adjust_fp * (fp + instr_adjust_fp_param_amount) + instr__reset * 0 + (1 - (instr_inc_fp + instr_adjust_fp + instr__reset)) * fp;
    pol pc_update = instr_adjust_fp * label + instr__jump_to_operation * _operation_id + instr__loop * pc + instr_return * 0 + (1 - (instr_adjust_fp + instr__jump_to_operation + instr__loop + instr_return)) * (pc + 1);
    pc' = (1 - first_step') * pc_update;
    pol constant p_line = [0, 1, 2, 3, 4] + [4]*;
    pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0] + [0]*;
    pol constant p_instr__loop = [0, 0, 0, 0, 1] + [1]*;
    pol constant p_instr__reset = [1, 0, 0, 0, 0] + [0]*;
    pol constant p_instr_adjust_fp = [0, 0, 0, 1, 0] + [0]*;
    pol constant p_instr_adjust_fp_param_amount = [0, 0, 0, 18446744069414584319, 0] + [0]*;
    pol constant p_instr_adjust_fp_param_t = [0, 0, 0, 3, 0] + [0]*;
    pol constant p_instr_inc_fp = [0, 0, 1, 0, 0] + [0]*;
    pol constant p_instr_inc_fp_param_amount = [0, 0, 7, 0, 0] + [0]*;
    pol constant p_instr_return = [0]*;
    { pc, instr_inc_fp, instr_inc_fp_param_amount, instr_adjust_fp, instr_adjust_fp_param_amount, instr_adjust_fp_param_t, instr__jump_to_operation, instr__reset, instr__loop, instr_return } in { p_line, p_instr_inc_fp, p_instr_inc_fp_param_amount, p_instr_adjust_fp, p_instr_adjust_fp_param_amount, p_instr_adjust_fp_param_t, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return };
    pol constant _linker_first_step = [1] + [0]*;
    _linker_first_step * (_operation_id - 2) = 0;
"#;
        let graph = parse_analyze_and_compile::<GoldilocksField>(source);
        let pil = link(graph).unwrap();
        assert_eq!(extract_main(&format!("{pil}")), expectation);
    }

    #[test]
    #[should_panic(expected = "Number passed to unsigned parameter is negative or too large")]
    fn negative_for_unsigned() {
        let source = r#"
machine NegativeForUnsigned {
    reg pc[@pc];
    reg fp;
    
    instr my_instr x: unsigned { pc' = pc + x }
    
    function main {
        my_instr 9223372034707292161;
    }
}
"#;
        let graph = parse_analyze_and_compile::<GoldilocksField>(source);
        let _ = link(graph);
    }

    #[test]
    fn instr_external_generated_pil() {
        let asm = r"
machine SubVM with latch: latch, operation_id: operation_id {
    operation add5<0> x -> y;

    col witness operation_id;
    col fixed latch = [1]*;

    col witness x;
    col witness y;

    y = x + 5;
}

machine Main {
    reg pc[@pc];
    reg X[<=];
    reg A;

    SubVM vm;

    instr add5_into_A X = vm.add5 X -> A';

    function main {
        add5_into_A 10; // A <== 15
    }
}
";
        let expected = r#"namespace main(1024);
    pol commit _operation_id(i) query std::prover::Query::Hint(3);
    pol constant _block_enforcer_last_step = [0]* + [1];
    let _operation_id_no_change = (1 - _block_enforcer_last_step) * (1 - instr_return);
    _operation_id_no_change * (_operation_id' - _operation_id) = 0;
    pol commit pc;
    pol commit X;
    pol commit reg_write_X_A;
    pol commit A;
    pol commit instr_add5_into_A;
    pol commit instr__jump_to_operation;
    pol commit instr__reset;
    pol commit instr__loop;
    pol commit instr_return;
    pol commit X_const;
    pol commit X_read_free;
    pol commit read_X_A;
    pol commit read_X_pc;
    X = read_X_A * A + read_X_pc * pc + X_const + X_read_free * X_free_value;
    pol constant first_step = [1] + [0]*;
    A' = reg_write_X_A * X + instr_add5_into_A * A' + instr__reset * 0 + (1 - (reg_write_X_A + instr_add5_into_A + instr__reset)) * A;
    pol pc_update = instr__jump_to_operation * _operation_id + instr__loop * pc + instr_return * 0 + (1 - (instr__jump_to_operation + instr__loop + instr_return)) * (pc + 1);
    pc' = (1 - first_step') * pc_update;
    pol constant p_line = [0, 1, 2, 3] + [3]*;
    pol commit X_free_value;
    pol constant p_X_const = [0, 0, 10, 0] + [0]*;
    pol constant p_X_read_free = [0]*;
    pol constant p_instr__jump_to_operation = [0, 1, 0, 0] + [0]*;
    pol constant p_instr__loop = [0, 0, 0, 1] + [1]*;
    pol constant p_instr__reset = [1, 0, 0, 0] + [0]*;
    pol constant p_instr_add5_into_A = [0, 0, 1, 0] + [0]*;
    pol constant p_instr_return = [0]*;
    pol constant p_read_X_A = [0]*;
    pol constant p_read_X_pc = [0]*;
    pol constant p_reg_write_X_A = [0]*;
    { pc, reg_write_X_A, instr_add5_into_A, instr__jump_to_operation, instr__reset, instr__loop, instr_return, X_const, X_read_free, read_X_A, read_X_pc } in { p_line, p_reg_write_X_A, p_instr_add5_into_A, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p_X_const, p_X_read_free, p_read_X_A, p_read_X_pc };
    instr_add5_into_A { 0, X, A' } in main_vm.latch { main_vm.operation_id, main_vm.x, main_vm.y };
    pol constant _linker_first_step = [1] + [0]*;
    _linker_first_step * (_operation_id - 2) = 0;
namespace main_vm(1024);
    pol commit operation_id;
    pol constant latch = [1]*;
    pol commit x;
    pol commit y;
    y = x + 5;
"#;
        let graph = parse_analyze_and_compile::<GoldilocksField>(asm);
        let pil = link(graph).unwrap();
        assert_eq!(extract_main(&(pil.to_string())), expected);
    }

    #[test]
    fn permutation_instructions() {
        let expected = r#"namespace main(65536);
    pol commit _operation_id(i) query std::prover::Query::Hint(13);
    pol constant _block_enforcer_last_step = [0]* + [1];
    let _operation_id_no_change = (1 - _block_enforcer_last_step) * (1 - instr_return);
    _operation_id_no_change * (_operation_id' - _operation_id) = 0;
    pol commit pc;
    pol commit X;
    pol commit Y;
    pol commit Z;
    pol commit reg_write_X_A;
    pol commit reg_write_Y_A;
    pol commit reg_write_Z_A;
    pol commit A;
    pol commit reg_write_X_B;
    pol commit reg_write_Y_B;
    pol commit reg_write_Z_B;
    pol commit B;
    pol commit instr_or;
    pol commit instr_or_into_B;
    pol commit instr_assert_eq;
    instr_assert_eq * (X - Y) = 0;
    pol commit instr__jump_to_operation;
    pol commit instr__reset;
    pol commit instr__loop;
    pol commit instr_return;
    pol commit X_const;
    pol commit X_read_free;
    pol commit read_X_A;
    pol commit read_X_B;
    pol commit read_X_pc;
    X = read_X_A * A + read_X_B * B + read_X_pc * pc + X_const + X_read_free * X_free_value;
    pol commit Y_const;
    pol commit Y_read_free;
    pol commit read_Y_A;
    pol commit read_Y_B;
    pol commit read_Y_pc;
    Y = read_Y_A * A + read_Y_B * B + read_Y_pc * pc + Y_const + Y_read_free * Y_free_value;
    pol commit Z_const;
    pol commit Z_read_free;
    pol commit read_Z_A;
    pol commit read_Z_B;
    pol commit read_Z_pc;
    Z = read_Z_A * A + read_Z_B * B + read_Z_pc * pc + Z_const + Z_read_free * Z_free_value;
    pol constant first_step = [1] + [0]*;
    A' = reg_write_X_A * X + reg_write_Y_A * Y + reg_write_Z_A * Z + instr__reset * 0 + (1 - (reg_write_X_A + reg_write_Y_A + reg_write_Z_A + instr__reset)) * A;
    B' = reg_write_X_B * X + reg_write_Y_B * Y + reg_write_Z_B * Z + instr_or_into_B * B' + instr__reset * 0 + (1 - (reg_write_X_B + reg_write_Y_B + reg_write_Z_B + instr_or_into_B + instr__reset)) * B;
    pol pc_update = instr__jump_to_operation * _operation_id + instr__loop * pc + instr_return * 0 + (1 - (instr__jump_to_operation + instr__loop + instr_return)) * (pc + 1);
    pc' = (1 - first_step') * pc_update;
    pol constant p_line = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13] + [13]*;
    pol commit X_free_value;
    pol commit Y_free_value;
    pol commit Z_free_value;
    pol constant p_X_const = [0, 0, 2, 0, 1, 0, 3, 0, 2, 0, 1, 0, 0, 0] + [0]*;
    pol constant p_X_read_free = [0]*;
    pol constant p_Y_const = [0, 0, 3, 3, 2, 3, 4, 7, 3, 3, 2, 3, 0, 0] + [0]*;
    pol constant p_Y_read_free = [0]*;
    pol constant p_Z_const = [0]*;
    pol constant p_Z_read_free = [0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    pol constant p_instr__loop = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1] + [1]*;
    pol constant p_instr__reset = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    pol constant p_instr_assert_eq = [0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0] + [0]*;
    pol constant p_instr_or = [0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    pol constant p_instr_or_into_B = [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0] + [0]*;
    pol constant p_instr_return = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0] + [0]*;
    pol constant p_read_X_A = [0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0] + [0]*;
    pol constant p_read_X_B = [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0] + [0]*;
    pol constant p_read_X_pc = [0]*;
    pol constant p_read_Y_A = [0]*;
    pol constant p_read_Y_B = [0]*;
    pol constant p_read_Y_pc = [0]*;
    pol constant p_read_Z_A = [0]*;
    pol constant p_read_Z_B = [0]*;
    pol constant p_read_Z_pc = [0]*;
    pol constant p_reg_write_X_A = [0]*;
    pol constant p_reg_write_X_B = [0]*;
    pol constant p_reg_write_Y_A = [0]*;
    pol constant p_reg_write_Y_B = [0]*;
    pol constant p_reg_write_Z_A = [0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    pol constant p_reg_write_Z_B = [0]*;
    { pc, reg_write_X_A, reg_write_Y_A, reg_write_Z_A, reg_write_X_B, reg_write_Y_B, reg_write_Z_B, instr_or, instr_or_into_B, instr_assert_eq, instr__jump_to_operation, instr__reset, instr__loop, instr_return, X_const, X_read_free, read_X_A, read_X_B, read_X_pc, Y_const, Y_read_free, read_Y_A, read_Y_B, read_Y_pc, Z_const, Z_read_free, read_Z_A, read_Z_B, read_Z_pc } in { p_line, p_reg_write_X_A, p_reg_write_Y_A, p_reg_write_Z_A, p_reg_write_X_B, p_reg_write_Y_B, p_reg_write_Z_B, p_instr_or, p_instr_or_into_B, p_instr_assert_eq, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p_X_const, p_X_read_free, p_read_X_A, p_read_X_B, p_read_X_pc, p_Y_const, p_Y_read_free, p_read_Y_A, p_read_Y_B, p_read_Y_pc, p_Z_const, p_Z_read_free, p_read_Z_A, p_read_Z_B, p_read_Z_pc };
    instr_or { 0, X, Y, Z } is main_bin.latch * main_bin.sel[0] { main_bin.operation_id, main_bin.A, main_bin.B, main_bin.C };
    instr_or_into_B { 0, X, Y, B' } is main_bin.latch * main_bin.sel[1] { main_bin.operation_id, main_bin.A, main_bin.B, main_bin.C };
    pol constant _linker_first_step = [1] + [0]*;
    _linker_first_step * (_operation_id - 2) = 0;
namespace main_bin(65536);
    pol commit operation_id;
    pol constant latch(i) { if i % 4 == 3 { 1 } else { 0 } };
    pol constant FACTOR(i) { 1 << (i + 1) % 4 * 8 };
    let a = (|i| i % 256);
    pol constant P_A(i) { a(i) };
    let b = (|i| (i >> 8) % 256);
    pol constant P_B(i) { b(i) };
    pol constant P_C(i) { (a(i) | b(i)) & 255 };
    pol commit A_byte;
    pol commit B_byte;
    pol commit C_byte;
    pol commit A;
    pol commit B;
    pol commit C;
    A' = A * (1 - latch) + A_byte * FACTOR;
    B' = B * (1 - latch) + B_byte * FACTOR;
    C' = C * (1 - latch) + C_byte * FACTOR;
    { A_byte, B_byte, C_byte } in { P_A, P_B, P_C };
    pol commit sel[2];
    std::array::map(sel, std::utils::force_bool);
"#;
        let file_name = format!(
            "{}/../test_data/asm/permutations/vm_to_block.asm",
            env!("CARGO_MANIFEST_DIR")
        );
        let contents = fs::read_to_string(file_name).unwrap();
        let graph = parse_analyze_and_compile::<GoldilocksField>(&contents);
        let pil = link(graph).unwrap();
        assert_eq!(extract_main(&format!("{pil}")), expected);
    }
}
