use std::iter::once;

use ast::{
    object::PILGraph,
    parsed::{
        build::{direct_reference, namespaced_reference},
        Expression, PILFile, PilStatement, SelectedExpressions,
    },
};
use number::FieldElement;

const DEFAULT_DEGREE: u64 = 1024;

/// a monolithic linker which outputs a single AIR
/// It sets the degree of submachines to the degree of the main machine, and errors out if a submachine has an explicit degree which doesn't match the main one
pub fn link<T: FieldElement>(graph: PILGraph<T>) -> Result<PILFile<T>, Vec<String>> {
    let main_location = graph.main;
    let main_degree = graph
        .objects
        .get(&main_location.location)
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
                    selector: Some(direct_reference(from.instr.flag)),
                    expressions: once(Expression::Number(to.function.id))
                        .chain(
                            from.instr
                                .params
                                .inputs
                                .params
                                .into_iter()
                                .chain(
                                    from.instr
                                        .params
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

                let params = to.function.params;

                let to_namespace = to.machine.location.clone().to_string();

                let rhs = SelectedExpressions {
                    selector: Some(
                        to.machine
                            .latch
                            .map(|latch| namespaced_reference(to_namespace.clone(), latch))
                            .unwrap_or(Expression::Number(T::from(0))),
                    ),
                    expressions: once(
                        to.machine
                            .function_id
                            .map(|function_id| {
                                namespaced_reference(to_namespace.clone(), function_id)
                            })
                            .unwrap_or(Expression::Number(T::from(0))),
                    )
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
        object::{Location, Machine, Object, PILGraph},
        parsed::{Expression, PILFile},
    };
    use number::{Bn254Field, FieldElement, GoldilocksField};

    use analysis::analyze;
    use parser::parse_asm;

    use pretty_assertions::assert_eq;

    use crate::{link, DEFAULT_DEGREE};

    fn parse_analyse_and_compile<T: FieldElement>(input: &str) -> PILGraph<T> {
        airgen::compile(asm_to_pil::compile(
            analyze(parse_asm(None, input).unwrap()).unwrap(),
        ))
    }

    #[test]
    fn degree() {
        // a graph with two objects of degree `main_degree` and `foo_degree`
        let test_graph = |main_degree, foo_degree| PILGraph {
            main: Machine {
                location: Location::from("main".to_string()),
                latch: None,
                function_id: None,
            },
            objects: [
                (
                    Location::from("main".to_string()),
                    Object::default().with_degree(main_degree),
                ),
                (
                    Location::from("foo".to_string()),
                    Object::default().with_degree(foo_degree),
                ),
            ]
            .into_iter()
            .collect(),
            entry_points: vec![],
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
                "Machine foo should have degree 1024, found 8".to_string()
            ])
        );
    }

    #[test]
    pub fn compile_simple_sum() {
        let expectation = r#"
namespace main(1024);
pol commit XInv;
pol commit XIsZero;
XIsZero = (1 - (X * XInv));
(XIsZero * X) = 0;
(XIsZero * (1 - XIsZero)) = 0;
pol constant first_step = [1] + [0]*;
pol commit pc;
pol commit X;
(first_step * A) = 0;
pol commit reg_write_X_A;
pol commit A;
(first_step * CNT) = 0;
pol commit reg_write_X_CNT;
pol commit CNT;
pol commit instr_jmpz;
pol commit instr_jmpz_param_l;
pol commit instr_jmp;
pol commit instr_jmp_param_l;
pol commit instr_dec_CNT;
pol commit instr_assert_zero;
(instr_assert_zero * (XIsZero - 1)) = 0;
pol commit X_const;
pol commit X_read_free;
pol commit read_X_A;
pol commit read_X_CNT;
pol commit read_X_pc;
X = (((((read_X_A * A) + (read_X_CNT * CNT)) + (read_X_pc * pc)) + X_const) + (X_read_free * X_free_value));
A' = (((first_step' * 0) + (reg_write_X_A * X)) + ((1 - (first_step' + reg_write_X_A)) * A));
CNT' = ((((first_step' * 0) + (reg_write_X_CNT * X)) + (instr_dec_CNT * (CNT - 1))) + ((1 - ((first_step' + reg_write_X_CNT) + instr_dec_CNT)) * CNT));
pc' = ((1 - first_step') * (((instr_jmpz * ((XIsZero * instr_jmpz_param_l) + ((1 - XIsZero) * (pc + 1)))) + (instr_jmp * instr_jmp_param_l)) + ((1 - (instr_jmpz + instr_jmp)) * (pc + 1))));
pol constant p_line = [0, 1, 2, 3, 4, 5, 6, 7] + [7]*;
pol commit X_free_value(i) query match pc { 0 => ("input", 1), 2 => ("input", (CNT + 1)), 5 => ("input", 0), };
pol constant p_X_const = [0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_X_read_free = [1, 0, 1, 0, 0, -1, 0, 0] + [0]*;
pol constant p_instr_assert_zero = [0, 0, 0, 0, 0, 0, 1, 0] + [0]*;
pol constant p_instr_dec_CNT = [0, 0, 0, 1, 0, 0, 0, 0] + [0]*;
pol constant p_instr_jmp = [0, 0, 0, 0, 1, 0, 0, 1] + [1]*;
pol constant p_instr_jmp_param_l = [0, 0, 0, 0, 1, 0, 0, 7] + [7]*;
pol constant p_instr_jmpz = [0, 1, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_jmpz_param_l = [0, 5, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_read_X_A = [0, 0, 1, 0, 0, 1, 1, 0] + [0]*;
pol constant p_read_X_CNT = [0, 1, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_read_X_pc = [0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_reg_write_X_A = [0, 0, 1, 0, 0, 1, 0, 0] + [0]*;
pol constant p_reg_write_X_CNT = [1, 0, 0, 0, 0, 0, 0, 0] + [0]*;
{ pc, reg_write_X_A, reg_write_X_CNT, instr_jmpz, instr_jmpz_param_l, instr_jmp, instr_jmp_param_l, instr_dec_CNT, instr_assert_zero, X_const, X_read_free, read_X_A, read_X_CNT, read_X_pc } in { p_line, p_reg_write_X_A, p_reg_write_X_CNT, p_instr_jmpz, p_instr_jmpz_param_l, p_instr_jmp, p_instr_jmp_param_l, p_instr_dec_CNT, p_instr_assert_zero, p_X_const, p_X_read_free, p_read_X_A, p_read_X_CNT, p_read_X_pc };

"#;
        let file_name = "../test_data/asm/simple_sum.asm";
        let contents = fs::read_to_string(file_name).unwrap();
        let graph = parse_analyse_and_compile::<GoldilocksField>(&contents);
        let pil = link(graph).unwrap();
        assert_eq!(format!("{pil}").trim(), expectation.trim());
    }

    #[test]
    pub fn compile_literal_number_args() {
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
pol constant first_step = [1] + [0]*;
pol commit pc;
(first_step * fp) = 0;
pol commit fp;
pol commit instr_inc_fp;
pol commit instr_inc_fp_param_amount;
pol commit instr_adjust_fp;
pol commit instr_adjust_fp_param_amount;
pol commit instr_adjust_fp_param_t;
fp' = ((((first_step' * 0) + (instr_inc_fp * (fp + instr_inc_fp_param_amount))) + (instr_adjust_fp * (fp + instr_adjust_fp_param_amount))) + ((1 - ((first_step' + instr_inc_fp) + instr_adjust_fp)) * fp));
pc' = ((1 - first_step') * ((instr_adjust_fp * label) + ((1 - instr_adjust_fp) * (pc + 1))));
pol constant p_line = [0, 1] + [1]*;
pol constant p_instr_adjust_fp = [0, 1] + [1]*;
pol constant p_instr_adjust_fp_param_amount = [0, -2] + [-2]*;
pol constant p_instr_adjust_fp_param_t = [0, 1] + [1]*;
pol constant p_instr_inc_fp = [1, 0] + [0]*;
pol constant p_instr_inc_fp_param_amount = [7, 0] + [0]*;
{ pc, instr_inc_fp, instr_inc_fp_param_amount, instr_adjust_fp, instr_adjust_fp_param_amount, instr_adjust_fp_param_t } in { p_line, p_instr_inc_fp, p_instr_inc_fp_param_amount, p_instr_adjust_fp, p_instr_adjust_fp_param_amount, p_instr_adjust_fp_param_t };
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
