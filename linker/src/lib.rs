use std::{collections::BTreeSet, iter::once};

use ast::{
    object::{Location, PILGraph},
    parsed::{
        direct_reference, namespaced_reference, BinaryOperator, Expression, PILFile, PilStatement,
        PolynomialName, PolynomialReference, SelectedExpressions,
    },
};
use number::FieldElement;

fn input_at(i: usize) -> String {
    format!("_input_{}", i)
}

fn output_at(i: usize) -> String {
    format!("_output_{}", i)
}

fn public_output_at(i: usize) -> String {
    format!("_public_output_{}", i)
}

/// a monolithic linker which outputs a single circuit
pub fn link<T: FieldElement>(mut graph: PILGraph<T>) -> PILFile<T> {
    let mut queue = BTreeSet::from([(Location::default().join("main"), true)]);

    let mut pil = vec![];

    while let Some((location, is_main)) = queue.pop_first() {
        let object = graph.objects.remove(&location).unwrap();
        // create a namespace for this object
        pil.push(PilStatement::Namespace(
            0,
            location.to_string(),
            Expression::Number(T::from(object.degree)),
        ));
        pil.extend(object.pil);
        for link in object.links {
            // add the target submachine to the queue
            queue.insert((location.clone().join(link.to.loc.clone()), false));
            // add the link to this namespace as a lookup

            let from = link.from;
            let to = link.to;

            // the lhs is `instr_flag { inputs, outputs }`
            let lhs = SelectedExpressions {
                selector: Some(direct_reference(from.instr.flag)),
                expressions: once(Expression::Number(T::from(
                    to.operation.index.unwrap() as u64
                )))
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

            let params = to.operation.params.unwrap();

            let to_namespace = location.clone().join(to.loc.clone()).to_string();

            let rhs = SelectedExpressions {
                selector: Some(namespaced_reference(
                    to_namespace.clone(),
                    to.latch.unwrap(),
                )),
                expressions: once(namespaced_reference(to_namespace.clone(), "_operation_id"))
                    .chain(
                        params
                            .inputs
                            .params
                            .iter()
                            .chain(params.outputs.iter().flat_map(|o| o.params.iter()))
                            .map(|i| {
                                assert!(i.ty.is_none());
                                namespaced_reference(to_namespace.clone(), i.name.clone())
                            }),
                    )
                    .collect(),
            };

            let lookup = PilStatement::PlookupIdentity(0, lhs, rhs);
            pil.push(lookup);
        }

        if is_main {
            let params = graph.entry_point.params.clone().unwrap();
            let entry_point_id = graph.entry_point.index.unwrap() as u64;

            // the output columns are not constant in the whole block, but we want to expose the results on the first row
            // therefore, create a new column to expose the outputs, which is constant in each block

            pil.extend(params.outputs.iter().flat_map(|o| {
                o.params.iter().enumerate().flat_map(|(i, _o)| {
                    [
                        // declare a column
                        PilStatement::PolynomialCommitDeclaration(
                            0,
                            vec![PolynomialName {
                                name: format!("_public_output_{}", i),
                                array_size: None,
                            }],
                            None,
                        ),
                        // make it equal to the output when we return
                        PilStatement::PolynomialIdentity(
                            0,
                            Expression::BinaryOperation(
                                Box::new(Expression::PolynomialReference(PolynomialReference {
                                    namespace: None,
                                    name: "instr_return".into(),
                                    index: None,
                                    next: false,
                                })),
                                BinaryOperator::Mul,
                                Box::new(Expression::BinaryOperation(
                                    Box::new(Expression::PolynomialReference(
                                        PolynomialReference {
                                            namespace: None,
                                            name: public_output_at(i),
                                            index: None,
                                            next: false,
                                        },
                                    )),
                                    BinaryOperator::Sub,
                                    Box::new(Expression::PolynomialReference(
                                        PolynomialReference {
                                            namespace: None,
                                            name: output_at(i),
                                            index: None,
                                            next: false,
                                        },
                                    )),
                                )),
                            ),
                        ),
                        // make it constant in each block
                        PilStatement::PolynomialIdentity(
                            0,
                            Expression::BinaryOperation(
                                Box::new(Expression::PolynomialReference(PolynomialReference {
                                    namespace: None,
                                    name: "instr_return".into(),
                                    index: None,
                                    next: false,
                                })),
                                BinaryOperator::Mul,
                                Box::new(Expression::BinaryOperation(
                                    Box::new(Expression::PolynomialReference(
                                        PolynomialReference {
                                            namespace: None,
                                            name: public_output_at(i),
                                            index: None,
                                            next: true,
                                        },
                                    )),
                                    BinaryOperator::Sub,
                                    Box::new(Expression::PolynomialReference(
                                        PolynomialReference {
                                            namespace: None,
                                            name: public_output_at(i),
                                            index: None,
                                            next: false,
                                        },
                                    )),
                                )),
                            ),
                        ),
                    ]
                })
            }));

            // call the first operation by initialising _operation_id to the first operation
            pil.push(PilStatement::PolynomialIdentity(
                0,
                Expression::BinaryOperation(
                    Box::new(Expression::PolynomialReference(PolynomialReference {
                        namespace: None,
                        name: "first_step".into(),
                        index: None,
                        next: false,
                    })),
                    BinaryOperator::Mul,
                    Box::new(Expression::BinaryOperation(
                        Box::new(Expression::PolynomialReference(PolynomialReference {
                            namespace: None,
                            name: "_operation_id".into(),
                            index: None,
                            next: false,
                        })),
                        BinaryOperator::Sub,
                        Box::new(Expression::Number(T::from(entry_point_id))),
                    )),
                ),
            ));

            // make the inputs and outputs public
            pil.extend(
                params
                    .inputs
                    .params
                    .iter()
                    .enumerate()
                    .map(|(i, _param)| {
                        PilStatement::PublicDeclaration(
                            0,
                            format!("INPUT_{}", i),
                            PolynomialReference {
                                namespace: None,
                                name: input_at(i),
                                index: None,
                                next: false,
                            },
                            Expression::Number(T::from(0)),
                        )
                    })
                    .chain(params.outputs.iter().flat_map(|outputs| {
                        outputs.params.iter().enumerate().map(|(i, _param)| {
                            PilStatement::PublicDeclaration(
                                0,
                                format!("OUTPUT_{}", i),
                                PolynomialReference {
                                    namespace: None,
                                    name: public_output_at(i),
                                    index: None,
                                    next: false,
                                },
                                Expression::Number(T::from(0)),
                            )
                        })
                    })),
            );
        }
    }

    PILFile(pil)
}

#[cfg(test)]
mod test {
    use std::fs;

    use ast::object::PILGraph;
    use number::{FieldElement, GoldilocksField};

    use analysis::analyze;
    use parser::parse_asm;

    use pretty_assertions::assert_eq;

    use crate::link;

    fn parse_analyse_and_compile<T: FieldElement>(input: &str) -> PILGraph<T> {
        asm_to_pil::compile(analyze(parse_asm(None, input).unwrap()).unwrap())
    }

    #[test]
    pub fn compile_simple_sum() {
        let expectation = r#"
namespace main(1024);
pol constant first_step = [1] + [0]*;
pol commit pc;
pol commit X;
(first_step * A) = 0;
pol commit reg_write_X_A;
pol commit A;
(first_step * CNT) = 0;
pol commit reg_write_X_CNT;
pol commit CNT;
pol commit XInv;
pol commit XIsZero;
XIsZero = (1 - (X * XInv));
(XIsZero * X) = 0;
(XIsZero * (1 - XIsZero)) = 0;
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
        let pil = link(graph);
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

    program {
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
        let pil = link(graph);
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
    
    program {
        instro 9223372034707292161;
    }
}
"#;
        let graph = parse_analyse_and_compile::<GoldilocksField>(source);
        let _ = link(graph);
    }
}
