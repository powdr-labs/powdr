//! The main powdr lib, used to compile from assembly to PIL

#![deny(clippy::print_stdout)]

pub mod pipeline;
pub mod test_util;
pub mod util;
pub mod verify;

pub use backend::{BackendType, Proof};
use executor::witgen::QueryCallback;
use itertools::Itertools;

use number::FieldElement;

#[allow(clippy::print_stdout)]
pub fn inputs_to_query_callback<T: FieldElement>(inputs: Vec<T>) -> impl QueryCallback<T> {
    // TODO: Pass bootloader inputs into this function
    // Right now, accessing bootloader inputs will always fail, because it will be out of bounds
    let bootloader_inputs = [];

    move |query: &str| -> Result<Option<T>, String> {
        // TODO In the future, when match statements need to be exhaustive,
        // This function probably gets an Option as argument and it should
        // answer None by Ok(None).

        // We are expecting a tuple
        let query = query
            .strip_prefix('(')
            .and_then(|q| q.strip_suffix(')'))
            .ok_or_else(|| "Prover query has to be a tuple".to_string())?;
        let items = query.split(',').map(|s| s.trim()).collect::<Vec<_>>();
        match &items[..] {
            ["\"input\"", index] => {
                let index = index
                    .parse::<usize>()
                    .map_err(|e| format!("Error parsing index: {e})"))?;
                let value = inputs.get(index).cloned();
                if let Some(value) = value {
                    log::trace!("Input query: Index {index} -> {value}");
                    Ok(Some(value))
                } else {
                    Err(format!(
                        "Error accessing prover inputs: Index {index} out of bounds {}",
                        inputs.len()
                    ))
                }
            }
            ["\"data\"", index, what] => {
                let index = index
                    .parse::<usize>()
                    .map_err(|e| format!("Error parsing index: {e})"))?;
                let what = what
                    .parse::<usize>()
                    .map_err(|e| format!("Error parsing what: {e})"))?;
                assert_eq!(what, 0);

                let value = inputs.get(index).cloned();
                if let Some(value) = value {
                    log::trace!("Input query: Index {index} -> {value}");
                    Ok(Some(value))
                } else {
                    Err(format!(
                        "Error accessing prover inputs: Index {index} out of bounds {}",
                        inputs.len()
                    ))
                }
            }
            ["\"bootloader_input\"", index] => {
                let index = index
                    .parse::<usize>()
                    .map_err(|e| format!("Error parsing index: {e})"))?;
                let value = bootloader_inputs.get(index).cloned();
                if let Some(value) = value {
                    log::trace!("Bootloader input query: Index {index} -> {value}");
                    Ok(Some(value))
                } else {
                    Err(format!(
                        "Error accessing bootloader inputs: Index {index} out of bounds {}",
                        inputs.len()
                    ))
                }
            }
            ["\"print_char\"", ch] => {
                print!(
                    "{}",
                    ch.parse::<u8>()
                        .map_err(|e| format!("Invalid char to print: {e}"))?
                        as char
                );
                // We do not answer None because we don't want this function to be
                // called again.
                Ok(Some(0.into()))
            }
            ["\"hint\"", value] => Ok(Some(T::from_str(value))),
            k => Err(format!("Unsupported query: {}", k.iter().format(", "))),
        }
    }
}
