pub mod jit_processor;

use std::hash::{Hash, Hasher};

use itertools::Itertools;
use powdr_ast::analyzed::AlgebraicReference;
use powdr_number::FieldElement;
use witgen_inference::WitgenInference;

use super::{
    analysis::detect_connection_type_and_block_size, machines::MachineParts,
    util::try_to_simple_poly, FixedData,
};

mod cell;
mod eval_result;
mod witgen_inference;

pub fn infer_witgen<'a, T: FieldElement>(
    fixed_data: &'a FixedData<'a, T>,
    parts: &MachineParts<'a, T>,
) -> Option<()> {
    // TODO the nicer interface would be for the inference alg to determine the block size.
    let (_, block_size, latch_row) =
        detect_connection_type_and_block_size(fixed_data, &parts.connections)?;
    // Let's only look at the first connection. TODO
    let conn = parts.connections.values().next().unwrap().right;
    for num_known in 1..conn.expressions.len() {
        if num_known != 13 {
            continue;
        }
        for inputs_known in conn.expressions.iter().combinations(num_known) {
            log::debug!(
                "Trying to auto-generate witgen code for known inputs: {}",
                inputs_known.iter().map(|e| e.to_string()).join(", ")
            );
            let Some(inputs_known) = inputs_known
                .iter()
                .map(|e| try_to_simple_poly(e))
                .collect::<Option<Vec<_>>>()
            else {
                continue;
            };

            let inputs_known = inputs_known.into_iter().map(|p| p.poly_id);

            let mut inference =
                WitgenInference::new(fixed_data, parts, block_size, latch_row, inputs_known, conn);
            if inference.run() {
                log::info!("Successfully generated witgen code for some machine.");
                log::trace!("Generated code:\n{}", inference.code());
            }
        }
    }
    Some(())
}
