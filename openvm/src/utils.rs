use std::{collections::BTreeMap, sync::Arc};

use crate::BusMap;
use itertools::Itertools;
use openvm_stark_backend::{
    air_builders::symbolic::{
        symbolic_expression::SymbolicExpression,
        symbolic_variable::{Entry, SymbolicVariable},
        SymbolicConstraints,
    },
    interaction::Interaction,
    p3_field::PrimeField32,
};
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, PolyID, PolynomialType,
};
use powdr_number::{BabyBearField, FieldElement};

pub fn algebraic_to_symbolic<T: PrimeField32>(
    expr: &AlgebraicExpression<T>,
) -> SymbolicExpression<T> {
    match expr {
        AlgebraicExpression::Number(n) => SymbolicExpression::Constant(*n),
        AlgebraicExpression::BinaryOperation(binary) => match binary.op {
            AlgebraicBinaryOperator::Add => SymbolicExpression::Add {
                x: Arc::new(algebraic_to_symbolic(&binary.left)),
                y: Arc::new(algebraic_to_symbolic(&binary.right)),
                degree_multiple: 0,
            },
            AlgebraicBinaryOperator::Sub => SymbolicExpression::Sub {
                x: Arc::new(algebraic_to_symbolic(&binary.left)),
                y: Arc::new(algebraic_to_symbolic(&binary.right)),
                degree_multiple: 0,
            },
            AlgebraicBinaryOperator::Mul => SymbolicExpression::Mul {
                x: Arc::new(algebraic_to_symbolic(&binary.left)),
                y: Arc::new(algebraic_to_symbolic(&binary.right)),
                degree_multiple: 0,
            },
            AlgebraicBinaryOperator::Pow => {
                // Assuming the right operand is a constant number
                let base = algebraic_to_symbolic(&binary.left);
                let exp = match *binary.right {
                    AlgebraicExpression::Number(n) => n,
                    _ => unimplemented!(),
                };

                if exp == T::ZERO {
                    SymbolicExpression::Constant(T::ONE)
                } else {
                    let mut result = base.clone();
                    let mut remaining = exp - T::ONE;

                    while remaining != T::ZERO {
                        result = SymbolicExpression::Mul {
                            x: Arc::new(result),
                            y: Arc::new(base.clone()),
                            degree_multiple: 0,
                        };
                        remaining -= T::ONE;
                    }
                    result
                }
            }
        },
        AlgebraicExpression::UnaryOperation(unary) => match unary.op {
            AlgebraicUnaryOperator::Minus => SymbolicExpression::Neg {
                x: Arc::new(algebraic_to_symbolic(&unary.expr)),
                degree_multiple: 0,
            },
        },
        AlgebraicExpression::Reference(algebraic_reference) => {
            let poly_id = algebraic_reference.poly_id;
            let next = algebraic_reference.next as usize;
            match poly_id.ptype {
                PolynomialType::Committed => SymbolicExpression::Variable(SymbolicVariable::new(
                    Entry::Main {
                        part_index: 0,
                        offset: next,
                    },
                    poly_id.id as usize,
                )),
                PolynomialType::Constant => SymbolicExpression::Variable(SymbolicVariable::new(
                    Entry::Preprocessed { offset: next },
                    poly_id.id as usize,
                )),
                PolynomialType::Intermediate => todo!(),
            }
        }
        AlgebraicExpression::PublicReference(_) => {
            unimplemented!()
        }
        AlgebraicExpression::Challenge(ch) => SymbolicExpression::Variable(SymbolicVariable::new(
            Entry::Challenge,
            ch.id.try_into().unwrap(),
        )),
    }
}
pub fn symbolic_to_algebraic<T: PrimeField32, P: FieldElement>(
    expr: &SymbolicExpression<T>,
    columns: &[String],
) -> AlgebraicExpression<P> {
    match expr {
        SymbolicExpression::Constant(c) => {
            AlgebraicExpression::Number(P::from_bytes_le(&c.as_canonical_u32().to_le_bytes()))
        }
        SymbolicExpression::Add { x, y, .. } => {
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left: Box::new(symbolic_to_algebraic(x, columns)),
                right: Box::new(symbolic_to_algebraic(y, columns)),
                op: AlgebraicBinaryOperator::Add,
            })
        }
        SymbolicExpression::Sub { x, y, .. } => {
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left: Box::new(symbolic_to_algebraic(x, columns)),
                right: Box::new(symbolic_to_algebraic(y, columns)),
                op: AlgebraicBinaryOperator::Sub,
            })
        }
        SymbolicExpression::Mul { x, y, .. } => {
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left: Box::new(symbolic_to_algebraic(x, columns)),
                right: Box::new(symbolic_to_algebraic(y, columns)),
                op: AlgebraicBinaryOperator::Mul,
            })
        }
        SymbolicExpression::Neg { x, .. } => {
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
                expr: Box::new(symbolic_to_algebraic(x, columns)),
                op: AlgebraicUnaryOperator::Minus,
            })
        }
        SymbolicExpression::Variable(SymbolicVariable { entry, index, .. }) => match entry {
            Entry::Main { offset, part_index } => {
                assert_eq!(*part_index, 0);
                let next = match offset {
                    0 => false,
                    1 => true,
                    _ => unimplemented!(),
                };
                let name = columns.get(*index).unwrap_or_else(|| {
                    panic!("Column index out of bounds: {index}\nColumns: {columns:?}");
                });
                AlgebraicExpression::Reference(AlgebraicReference {
                    name: name.clone(),
                    poly_id: PolyID {
                        id: *index as u64,
                        ptype: PolynomialType::Committed,
                    },
                    next,
                })
            }
            _ => unimplemented!(),
        },
        SymbolicExpression::IsFirstRow => AlgebraicExpression::Reference(AlgebraicReference {
            name: "is_first_row".to_string(),
            poly_id: PolyID {
                id: 0,
                ptype: PolynomialType::Constant,
            },
            next: false,
        }),
        SymbolicExpression::IsLastRow => AlgebraicExpression::Reference(AlgebraicReference {
            name: "is_last_row".to_string(),
            poly_id: PolyID {
                id: 1,
                ptype: PolynomialType::Constant,
            },
            next: false,
        }),
        SymbolicExpression::IsTransition => AlgebraicExpression::Reference(AlgebraicReference {
            name: "is_transition".to_string(),
            poly_id: PolyID {
                id: 2,
                ptype: PolynomialType::Constant,
            },
            next: false,
        }),
    }
}

pub fn get_pil<F: PrimeField32>(
    name: &str,
    constraints: &SymbolicConstraints<F>,
    columns: &Vec<String>,
    public_values: Vec<String>,
    bus_map: &BusMap,
) -> String {
    let mut pil = format!(
        "
namespace {name};
    // Preamble
    col fixed is_first_row = [1] + [0]*;
    col fixed is_last_row = [0] + [1]*;
    col fixed is_transition = [0] + [1]* + [0];

"
    );

    pil.push_str(
        &bus_map
            .inner()
            .iter()
            .map(|(id, bus_type)| format!("    let {bus_type} = {id};"))
            .join("\n"),
    );

    pil.push_str(
        "

    // Witness columns
",
    );

    // Declare witness columns
    for column in columns {
        pil.push_str(&format!("    col witness {column};\n"));
    }

    let bus_interactions_by_bus = constraints
        .interactions
        .iter()
        .map(|interaction| (interaction.bus_index, interaction))
        .into_group_map()
        .into_iter()
        // Use BTreeMap to sort by bus_index
        .collect::<BTreeMap<_, _>>();

    pil.push_str(
        "
    // Bus interactions (bus_index, fields, count)\n",
    );

    for (bus_index, interactions) in bus_interactions_by_bus {
        let bus_name = bus_map.bus_type(bus_index as u64).to_string();

        for interaction in interactions {
            format_bus_interaction(&mut pil, interaction, columns, &public_values, &bus_name);
        }
        pil.push('\n');
    }

    pil.push_str("    // Constraints\n");

    for constraint in &constraints.constraints {
        pil.push_str(&format!(
            "    {} = 0;\n",
            format_expr(constraint, columns, &public_values)
        ));
    }
    pil
}

fn format_bus_interaction<F: PrimeField32>(
    pil: &mut String,
    interaction: &Interaction<SymbolicExpression<F>>,
    columns: &[String],
    public_values: &[String],
    bus_name: &str,
) {
    let Interaction { message, count, .. } = interaction;
    // We do not know what is a send or a receive
    let function_name = "bus_interaction";

    pil.push_str(&format!(
        "    std::protocols::bus::{}({bus_name}, [{}], {});\n",
        function_name,
        message
            .iter()
            .map(|value| format_expr(value, columns, public_values))
            .collect::<Vec<String>>()
            .join(", "),
        format_expr(count, columns, public_values)
    ));
}

fn format_expr<F: PrimeField32>(
    expr: &SymbolicExpression<F>,
    columns: &[String],
    // TODO: Implement public references
    _public_values: &[String],
) -> String {
    symbolic_to_algebraic::<_, BabyBearField>(expr, columns).to_string()
}
