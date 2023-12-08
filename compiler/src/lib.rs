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

pub fn parse_query(query: &str) -> Result<Vec<&str>, String> {
    // We are expecting a tuple
    let query = query
        .strip_prefix('(')
        .and_then(|q| q.strip_suffix(')'))
        .ok_or_else(|| "Prover query has to be a tuple".to_string())?;
    Ok(query.split(',').map(|s| s.trim()).collect::<Vec<_>>())
}

pub fn access_element<T: FieldElement>(
    name: &str,
    elements: &[T],
    index_str: &str,
) -> Result<Option<T>, String> {
    let index = index_str
        .parse::<usize>()
        .map_err(|e| format!("Error parsing index: {e})"))?;
    let value = elements.get(index).cloned();
    if let Some(value) = value {
        log::trace!("Query for {name}: Index {index} -> {value}");
        Ok(Some(value))
    } else {
        Err(format!(
            "Error accessing {name}: Index {index} out of bounds {}",
            elements.len()
        ))
    }
}

#[allow(clippy::print_stdout)]
pub fn inputs_to_query_callback<T: FieldElement>(inputs: Vec<T>) -> impl QueryCallback<T> {
    move |query: &str| -> Result<Option<T>, String> {
        // TODO In the future, when match statements need to be exhaustive,
        // This function probably gets an Option as argument and it should
        // answer None by Ok(None).

        match &parse_query(query)?[..] {
            ["\"input\"", index] => access_element("prover inputs", &inputs, index),
            ["\"data\"", index, what] => {
                let what = what
                    .parse::<usize>()
                    .map_err(|e| format!("Error parsing what: {e})"))?;
                assert_eq!(what, 0);

                access_element("prover inputs", &inputs, index)
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
