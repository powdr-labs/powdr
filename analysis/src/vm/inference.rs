//! Infer assignment registers in asm statements

use powdr_ast::{
    asm_analysis::{AnalysisASMFile, Expression, FunctionStatement, Machine},
    parsed::asm::AssignmentRegister,
};

pub fn infer(mut file: AnalysisASMFile) -> Result<AnalysisASMFile, Vec<String>> {
    let mut errors = vec![];

    file.machines_mut()
        .for_each(|(_, m)| match infer_machine(m) {
            Ok(()) => {}
            Err(e) => {
                errors.extend(e);
            }
        });

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(file)
    }
}

fn infer_machine(machine: &mut Machine) -> Result<(), Vec<String>> {
    let mut errors = vec![];

    for f in machine.callable.functions_mut() {
        for s in f.body.statements.iter_mut() {
            if let FunctionStatement::Assignment(a) = s {
                // Map function calls to the list of assignment registers and all other expressions to a list of None.
                let expr_regs = match &*a.rhs {
                    Expression::FunctionCall(_, c) => {
                        let instr_name =
                            if let Expression::Reference(_, reference) = c.function.as_ref() {
                                reference.try_to_identifier().unwrap()
                            } else {
                                panic!("Only instructions allowed.");
                            };
                        let def = machine
                            .instructions
                            .iter()
                            .find(|i| i.name == *instr_name)
                            .unwrap_or_else(|| panic!("invalid instruction: {instr_name}"));

                        def.instruction
                            .params
                            .outputs
                            .iter()
                            .map(|o| {
                                assert!(o.ty.is_none());
                                AssignmentRegister::Register(o.name.clone())
                            })
                            .collect::<Vec<_>>()
                    }
                    _ => vec![AssignmentRegister::Wildcard; a.lhs_with_reg.len()],
                };

                assert_eq!(expr_regs.len(), a.lhs_with_reg.len());

                for ((w, reg), expr_reg) in a.lhs_with_reg.iter_mut().zip(expr_regs) {
                    match (&reg, expr_reg) {
                        (
                            AssignmentRegister::Register(using_reg),
                            AssignmentRegister::Register(expr_reg),
                        ) if *using_reg != expr_reg => {
                            errors.push(format!("Assignment register `{}` is incompatible with `{}`. Try using `<==` with no explicit assignment registers.", using_reg, a.rhs));
                        }
                        (AssignmentRegister::Register(_), _) => {}
                        (AssignmentRegister::Wildcard, AssignmentRegister::Register(expr_reg)) => {
                            // infer the assignment register to that of the rhs
                            *reg = AssignmentRegister::Register(expr_reg);
                        }
                        (AssignmentRegister::Wildcard, AssignmentRegister::Wildcard) => {
                            errors.push(format!("Impossible to infer the assignment register to write to register `{w}`"));
                        }
                    }
                }
            }
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use powdr_ast::{asm_analysis::AssignmentStatement, parsed::asm::parse_absolute_path};

    use crate::vm::test_utils::infer_str;

    use super::*;

    #[test]
    fn inferred() {
        let file = r#"
            machine Machine {
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

        let file = infer_str(file).unwrap();

        let machine = &file.get_machine(&parse_absolute_path("::Machine")).unwrap();
        if let FunctionStatement::Assignment(AssignmentStatement { lhs_with_reg, .. }) = machine
            .functions()
            .next()
            .unwrap()
            .body
            .statements
            .iter()
            .next()
            .unwrap()
        {
            assert_eq!(
                lhs_with_reg[0].1,
                AssignmentRegister::Register("X".to_string())
            );
        } else {
            panic!()
        };
    }

    #[test]
    fn compatible() {
        let file = r#"
            machine Machine {
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

        let file = infer_str(file).unwrap();

        let machine = &file.get_machine(&parse_absolute_path("::Machine")).unwrap();
        if let FunctionStatement::Assignment(AssignmentStatement { lhs_with_reg, .. }) = &machine
            .functions()
            .next()
            .unwrap()
            .body
            .statements
            .iter()
            .next()
            .unwrap()
        {
            assert_eq!(
                lhs_with_reg[0].1,
                AssignmentRegister::Register("X".to_string())
            );
        } else {
            panic!()
        };
    }

    #[test]
    fn incompatible() {
        let file = r#"
            machine Machine {
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

        assert_eq!(infer_str(file).unwrap_err(), vec!["Assignment register `Y` is incompatible with `foo()`. Try using `<==` with no explicit assignment registers."]);
    }

    #[test]
    fn unclear() {
        let file = r#"
            machine Machine {
                reg pc[@pc];
                reg X[<=];
                reg Y[<=];
                reg A;

                function main {
                    A <== 1;
                }
            }
        "#;

        assert_eq!(
            infer_str(file).unwrap_err(),
            vec![
                "Impossible to infer the assignment register to write to register `A`".to_string()
            ]
        );
    }
}
