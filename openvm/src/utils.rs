use core::fmt;
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
use powdr_autoprecompiles::expression::AlgebraicReference;
use powdr_expression::AlgebraicExpression;
use powdr_expression::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicUnaryOperation,
    AlgebraicUnaryOperator,
};

pub enum OpenVmReference {
    /// Reference to a witness column. The boolean indicates if the reference is to the next row.
    WitnessColumn(AlgebraicReference, bool),
    IsFirstRow,
    IsLastRow,
    IsTransition,
}

impl fmt::Display for OpenVmReference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OpenVmReference::WitnessColumn(reference, next) => {
                write!(f, "{}{}", reference.name, if *next { "'" } else { "" })
            }
            OpenVmReference::IsFirstRow => write!(f, "is_first_row"),
            OpenVmReference::IsLastRow => write!(f, "is_last_row"),
            OpenVmReference::IsTransition => write!(f, "is_transition"),
        }
    }
}

/// An unsupported OpenVM reference appeared, i.e., a non-zero offset or a reference to
/// is_first_row, is_last_row, or is_transition.
#[derive(Debug)]
pub struct UnsupportedOpenVmReferenceError;

impl TryFrom<OpenVmReference> for AlgebraicReference {
    type Error = UnsupportedOpenVmReferenceError;

    fn try_from(value: OpenVmReference) -> Result<Self, Self::Error> {
        match value {
            OpenVmReference::WitnessColumn(reference, false) => Ok(reference),
            _ => Err(UnsupportedOpenVmReferenceError),
        }
    }
}

pub fn algebraic_to_symbolic<F: PrimeField32>(
    expr: &AlgebraicExpression<F, AlgebraicReference>,
) -> SymbolicExpression<F> {
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
        },
        AlgebraicExpression::UnaryOperation(unary) => match unary.op {
            AlgebraicUnaryOperator::Minus => SymbolicExpression::Neg {
                x: Arc::new(algebraic_to_symbolic(&unary.expr)),
                degree_multiple: 0,
            },
        },
        AlgebraicExpression::Reference(algebraic_reference) => {
            SymbolicExpression::Variable(SymbolicVariable::new(
                Entry::Main {
                    part_index: 0,
                    offset: 0,
                },
                algebraic_reference.id as usize,
            ))
        }
    }
}

pub fn symbolic_to_algebraic<F: PrimeField32>(
    expr: &SymbolicExpression<F>,
    columns: &[Arc<String>],
) -> AlgebraicExpression<F, OpenVmReference> {
    match expr {
        SymbolicExpression::Constant(c) => AlgebraicExpression::Number(*c),
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
                let next = match *offset {
                    0 => false,
                    1 => true,
                    _ => panic!("Unexpected offset: {offset}"),
                };
                let name = columns.get(*index).unwrap_or_else(|| {
                    panic!("Column index out of bounds: {index}\nColumns: {columns:?}");
                });
                AlgebraicExpression::Reference(OpenVmReference::WitnessColumn(
                    AlgebraicReference {
                        name: name.clone(),
                        id: *index as u64,
                    },
                    next,
                ))
            }
            _ => unimplemented!(),
        },
        SymbolicExpression::IsFirstRow => {
            AlgebraicExpression::Reference(OpenVmReference::IsFirstRow)
        }
        SymbolicExpression::IsLastRow => AlgebraicExpression::Reference(OpenVmReference::IsLastRow),
        SymbolicExpression::IsTransition => {
            AlgebraicExpression::Reference(OpenVmReference::IsTransition)
        }
    }
}

pub fn get_pil<F: PrimeField32>(
    name: &str,
    constraints: &SymbolicConstraints<F>,
    columns: &Vec<Arc<String>>,
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
            .all_types_by_id()
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

    let (bus_interactions_by_bus, new_buses): (BTreeMap<_, _>, BTreeMap<_, _>) = constraints
        .interactions
        .iter()
        .map(|interaction| (interaction.bus_index, interaction))
        .into_group_map()
        .into_iter()
        .partition::<BTreeMap<_, _>, _>(|(bus_index, _)| {
            bus_map.all_types_by_id().contains_key(&(*bus_index as u64))
        });

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

    for (bus_index, interactions) in new_buses {
        let bus_name = format!("bus_{bus_index}");
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
    columns: &[Arc<String>],
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
    columns: &[Arc<String>],
    // TODO: Implement public references
    _public_values: &[String],
) -> String {
    symbolic_to_algebraic(expr, columns).to_string()
}
