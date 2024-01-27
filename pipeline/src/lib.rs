//! The main powdr lib, used to compile from assembly to PIL

#![deny(clippy::print_stdout)]

use std::marker::{Send, Sync};

pub mod pipeline;
pub mod test_util;
pub mod util;
pub mod verify;

pub use pipeline::Pipeline;
pub use pipeline::Stage;

use itertools::Itertools;
pub use powdr_backend::{BackendType, Proof};
use powdr_executor::witgen::QueryCallback;

use powdr_number::FieldElement;

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
pub fn serde_data_to_query_callback<T: FieldElement, S: serde::Serialize + Send + Sync>(
    channel: u32,
    data: &S,
) -> impl QueryCallback<T> {
    let bytes = serde_cbor::to_vec(&data).unwrap();
    move |query: &str| -> Result<Option<T>, String> {
        match &parse_query(query)?[..] {
            ["\"data_identifier\"", index, cb_channel] => {
                let cb_channel = cb_channel
                    .parse::<u32>()
                    .map_err(|e| format!("Error parsing callback data channel: {e})"))?;

                if channel != cb_channel {
                    return Ok(None);
                }

                let index = index
                    .parse::<usize>()
                    .map_err(|e| format!("Error parsing index: {e})"))?;

                // query index 0 means the length
                Ok(Some(match index {
                    0 => (bytes.len() as u64).into(),
                    index => (bytes[index - 1] as u64).into(),
                }))
            }
            k => Err(format!("Unsupported query: {}", k.iter().format(", "))),
        }
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
            ["\"hint\"", value] => Ok(Some(T::from_str(value).unwrap())),
            k => Err(format!("Unsupported query: {}", k.iter().format(", "))),
        }
    }
}
