use ast::asm_analysis::{AnalysisASMFile, Expression, FunctionStatement, Machine};
use number::FieldElement;

pub fn infer<T: FieldElement>(file: AnalysisASMFile<T>) -> Result<AnalysisASMFile<T>, Vec<String>> {
    let mut errors = vec![];
    let mut res = AnalysisASMFile::default();

    for (name, m) in file.machines {
        match infer_machine(m) {
            Ok(m) => {
                res.machines.insert(name, m);
            }
            Err(e) => {
                errors.extend(e);
            }
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(res)
    }
}

fn infer_machine<T: FieldElement>(mut machine: Machine<T>) -> Result<Machine<T>, Vec<String>> {
    let mut errors = vec![];

    for f in machine.functions.iter_mut() {
        for s in f.body.statements.iter_mut() {
            if let FunctionStatement::Assignment(a) = s {
                let expr_reg = match &*a.rhs {
                    Expression::FunctionCall(c) => {
                        let instr = machine
                            .instructions
                            .iter()
                            .find(|i| i.name == c.id)
                            .unwrap();
                        let output = {
                            let outputs = instr.params.outputs.as_ref().unwrap();
                            assert!(outputs.params.len() == 1);
                            &outputs.params[0]
                        };
                        assert!(output.ty.is_none());
                        Some(output.name.clone())
                    }
                    _ => None,
                };

                match (&mut a.using_reg, expr_reg) {
                    (Some(using_reg), Some(expr_reg)) if *using_reg != expr_reg => {
                        errors.push(format!("Assignment register `{}` is incompatible with `{}`. Try replacing `<={}=` by `<==`.", using_reg, a.rhs, using_reg));
                    }
                    (Some(_), _) => {}
                    (None, Some(expr_reg)) => {
                        // infer the assignment register to that of the rhs
                        a.using_reg = Some(expr_reg);
                    }
                    (None, None) => {
                        errors.push(format!("Impossible to infer the assignment register for `{a}`. Try being more explicit."));
                    }
                }
            }
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(machine)
    }
}

#[cfg(test)]
mod tests {
    use ast::asm_analysis::AssignmentStatement;
    use number::Bn254Field;

    use crate::test_util::infer_str;

    use super::*;

    #[test]
    fn inferred() {
        let file = r#"
            machine Incompatible {
                reg pc[@pc];
                reg X[<=];
                reg Y[<=];
                reg A;

                instr foo -> X {}

                function main {
                    A <== foo();
                }
            }
        "#;

        let file = infer_str::<Bn254Field>(file).unwrap();

        if let FunctionStatement::Assignment(AssignmentStatement { using_reg, .. }) =
            &file.machines["Incompatible"].functions[0].body.statements[0]
        {
            assert_eq!(*using_reg, Some("X".to_string()));
        } else {
            panic!()
        }
    }

    #[test]
    fn compatible() {
        let file = r#"
            machine Incompatible {
                reg pc[@pc];
                reg X[<=];
                reg Y[<=];
                reg A;

                instr foo -> X {}

                function main {
                    A <=X= foo();
                }
            }
        "#;

        let file = infer_str::<Bn254Field>(file).unwrap();

        if let FunctionStatement::Assignment(AssignmentStatement { using_reg, .. }) =
            &file.machines["Incompatible"].functions[0].body.statements[0]
        {
            assert_eq!(*using_reg, Some("X".to_string()));
        } else {
            panic!()
        }
    }

    #[test]
    fn incompatible() {
        let file = r#"
            machine Incompatible {
                reg pc[@pc];
                reg X[<=];
                reg Y[<=];
                reg A;

                instr foo -> X {}

                function main {
                    A <=Y= foo();
                }
            }
        "#;

        assert_eq!(infer_str::<Bn254Field>(file).unwrap_err(), vec!["Assignment register `Y` is incompatible with `foo()`. Try replacing `<=Y=` by `<==`."]);
    }

    #[test]
    fn unclear() {
        let file = r#"
            machine Incompatible {
                reg pc[@pc];
                reg X[<=];
                reg Y[<=];
                reg A;

                function main {
                    A <== 1;
                }
            }
        "#;

        assert_eq!(infer_str::<Bn254Field>(file).unwrap_err(), vec!["Impossible to infer the assignment register for `A <== 1;`. Try being more explicit.".to_string()]);
    }
}
