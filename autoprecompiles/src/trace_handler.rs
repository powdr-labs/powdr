use itertools::Itertools;
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_expression::{AlgebraicBinaryOperation, AlgebraicBinaryOperator};
use powdr_number::ExpressionConvertible;
use rayon::prelude::*;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::{Debug, Display};
use std::{cmp::Eq, hash::Hash};

use crate::adapter::Adapter;
use crate::expression::{AlgebraicExpression, AlgebraicReference};
use crate::expression_conversion::algebraic_to_grouped_expression;
use crate::powdr::UniqueReferences;
use crate::{Apc, InstructionHandler, SymbolicMachine};

/// Returns data needed for constructing the APC trace.
pub struct TraceData<'a, F> {
    /// The dummy trace values for each instruction.
    pub dummy_values: Vec<Vec<&'a [F]>>,
    /// The mapping from dummy trace index to APC index for each instruction.
    pub dummy_trace_index_to_apc_index_by_instruction: Vec<HashMap<usize, usize>>,
    /// The mapping from poly_id to the index in the list of apc columns.
    /// The values are always unique and contiguous.
    pub apc_poly_id_to_index: BTreeMap<u64, usize>,
    /// Indices of columns to compute and the way to compute them
    /// (from other values).
    pub columns_to_compute: BTreeMap<usize, ComputationMethod>,
}

pub struct Trace<F> {
    pub values: Vec<F>,
    pub width: usize,
}

impl<F> Trace<F> {
    pub fn new(values: Vec<F>, width: usize) -> Self {
        Self { values, width }
    }
}

pub enum ComputationMethod {
    /// A constant value.
    Constant(u64),
    /// The field inverse of a sum of values of other columns,
    /// given by their indices.
    InverseOfSum(Vec<usize>),
}

pub fn generate_trace<'a, IH>(
    air_id_to_dummy_trace: &'a HashMap<IH::AirId, Trace<IH::Field>>,
    instruction_handler: &'a IH,
    apc_call_count: usize,
    apc: &Apc<IH::Field, IH::Instruction>,
) -> TraceData<'a, IH::Field>
where
    IH: InstructionHandler,
    IH::Field: Display + Send + Sync,
    IH::AirId: Eq + Hash + Send + Sync,
{
    // Returns a vector with the same length as original instructions
    let original_instruction_air_ids = apc
        .instructions()
        .iter()
        .map(|instruction| {
            instruction_handler
                .get_instruction_air_and_id(instruction)
                .0
        })
        .collect::<Vec<_>>();

    let air_id_occurrences = original_instruction_air_ids.iter().counts();

    let apc_poly_id_to_index: BTreeMap<u64, usize> = apc
        .machine
        .main_columns()
        .enumerate()
        .map(|(index, c)| (c.id, index))
        .collect();

    // The index of the "is_valid" column.
    let is_valid_index = apc_poly_id_to_index[&apc.is_valid_poly_id()];

    let original_instruction_table_offsets = original_instruction_air_ids
        .iter()
        .scan(
            HashMap::default(),
            |counts: &mut HashMap<&IH::AirId, usize>, air_id| {
                let count = counts.entry(air_id).or_default();
                let current_count = *count;
                *count += 1;
                Some(current_count)
            },
        )
        .collect::<Vec<_>>();

    // The Poly IDs for which we have found a column in the dummy trace.
    let mut mapped_poly_ids = BTreeSet::new();

    let dummy_trace_index_to_apc_index_by_instruction = apc
        .subs
        .iter()
        .map(|subs| {
            let mut dummy_trace_index_to_apc_index = HashMap::new();
            for (dummy_index, poly_id) in subs.iter().enumerate() {
                if let Some(apc_index) = apc_poly_id_to_index.get(poly_id) {
                    dummy_trace_index_to_apc_index.insert(dummy_index, *apc_index);
                    mapped_poly_ids.insert(*poly_id);
                }
            }
            dummy_trace_index_to_apc_index
        })
        .collect::<Vec<_>>();

    let dummy_values = (0..apc_call_count)
        .into_par_iter()
        .map(|trace_row| {
            original_instruction_air_ids
                .iter()
                .zip_eq(original_instruction_table_offsets.iter())
                .map(|(air_id, dummy_table_offset)| {
                    let Trace { values, width } = air_id_to_dummy_trace.get(air_id).unwrap();
                    let occurrences_per_record = air_id_occurrences.get(air_id).unwrap();
                    let start = (trace_row * occurrences_per_record + dummy_table_offset) * width;
                    let end = start + width;
                    &values[start..end]
                })
                .collect_vec()
        })
        .collect();

    let mut columns_to_compute = [(is_valid_index, ComputationMethod::Constant(1u64))]
        .into_iter()
        .collect::<BTreeMap<_, _>>();
    // Column IDs for which we could not find a corresponding column in the dummy trace.
    // Those are newly created columns that we will need to compute.
    for column in apc
        .machine
        .main_columns()
        .filter(|id| !mapped_poly_ids.contains(&id.id) && id.id != apc.is_valid_poly_id())
    {
        columns_to_compute.insert(
            apc_poly_id_to_index[&column.id],
            derive_computation_method(&column, apc.machine()),
        );
    }

    TraceData {
        dummy_values,
        dummy_trace_index_to_apc_index_by_instruction,
        apc_poly_id_to_index,
        columns_to_compute,
    }
}

fn derive_computation_method<T: Display>(
    column: &AlgebraicReference,
    machine: &SymbolicMachine<T>,
) -> ComputationMethod {
    for constr in &machine.constraints {
        if !constr.unique_references().contains(column) {
            continue;
        }
        println!("{constr}");
        // // We try to match against the pattern `column * something - 1 = 0`.
        // // TODO cannot use GroupedExpression here, unfortunately.
        // // TODO avoid doing the conversion multiple times.
        // let expr = constr.expr.to_expression(
        //     &|n| GroupedExpression::<_, AlgebraicReference>::from_number(A::from_field(*n)),
        //     &|r| GroupedExpression::from_unknown_variable(r.clone()),
        // );
        // let (quadratic, linear, constant) = expr.components();
    }
    panic!("Could not derive a computation method for column {column}");
}

fn try_derive_method_from_constr<T: Display>(
    column: &AlgebraicReference,
    expr: &AlgebraicExpression<T>,
) -> Option<ComputationMethod> {
    match expr {
        AlgebraicExpression::Reference(_) => todo!(),
        AlgebraicExpression::Number(_) => None,
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            match op {
                AlgebraicBinaryOperator::Add | AlgebraicBinaryOperator::Sub => None,
                AlgebraicBinaryOperator::Mul => try_derive_method_from_constr(column, left)
                    .or_else(|| try_derive_method_from_constr(column, right)),
            }
        }
        AlgebraicExpression::UnaryOperation(_) => None,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_try_derive() {
        let x = AlgebraicReference {
            id: 0,
            name: "x".to_string().into(),
        };
        let x_expr = AlgebraicExpression::Reference(x.clone());
        let y = AlgebraicExpression::Reference(AlgebraicReference {
            id: 1,
            name: "y".to_string().into(),
        });
        let z = AlgebraicExpression::Reference(AlgebraicReference {
            id: 2,
            name: "z".to_string().into(),
        });
        // (1 - x * (y + z)) * z
        let expr = (AlgebraicExpression::from(1u64) - x_expr.clone() * (y.clone() + z.clone()))
            * z.clone();
        let result = try_derive_method_from_constr(&x, &expr);
        assert!(result.is_some());
        // TODO more
    }
}
