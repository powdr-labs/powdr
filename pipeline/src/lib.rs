//! The main powdr lib, used to compile from assembly to PIL

#![deny(clippy::print_stdout)]

use std::marker::{Send, Sync};

pub mod pipeline;
pub mod test_util;
pub mod util;
pub mod verify;

pub use pipeline::Pipeline;

pub use powdr_backend::{BackendType, Proof};
use powdr_executor::witgen::QueryCallback;

use powdr_number::FieldElement;

// TODO at some point, we could also just pass evaluator::Values around - would be much faster.
pub fn parse_query(query: &str) -> Result<(&str, Vec<&str>), String> {
    // We are expecting an enum value
    if let Some(paren) = query.find('(') {
        let name = &query[..paren];
        let data = query[paren + 1..].strip_suffix(')').ok_or_else(|| {
            format!(
                "Error parsing query input \"{query}\". Could not find closing ')' in enum data."
            )
        })?;
        Ok((name, data.split(',').map(|s| s.trim()).collect::<Vec<_>>()))
    } else {
        Ok((query, vec![]))
    }
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

pub fn serde_data_to_query_callback<T: FieldElement, S: serde::Serialize + Send + Sync>(
    channel: u32,
    data: &S,
) -> impl QueryCallback<T> {
    let bytes = serde_cbor::to_vec(&data).unwrap();
    move |query: &str| -> Result<Option<T>, String> {
        let (id, data) = parse_query(query)?;
        match id {
            "None" => Ok(None),
            "DataIdentifier" => {
                let [index, cb_channel] = data[..] else {
                    panic!()
                };
                let cb_channel = cb_channel
                    .parse::<u32>()
                    .map_err(|e| format!("Error parsing callback data channel: {e})"))?;

                if channel != cb_channel {
                    return Err("Callback channel mismatch".to_string());
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
            _ => Err(format!("Unsupported query: {query}")),
        }
    }
}

pub fn inputs_to_query_callback<T: FieldElement>(inputs: Vec<T>) -> impl QueryCallback<T> {
    move |query: &str| -> Result<Option<T>, String> {
        let (id, data) = parse_query(query)?;
        match id {
            "None" => Ok(None),
            "Input" => {
                assert_eq!(data.len(), 1);
                access_element("prover inputs", &inputs, data[0])
            }
            _ => Err(format!("Unsupported query: {query}")),
        }
    }
}

#[allow(clippy::print_stdout)]
pub fn handle_simple_queries_callback<'a, T: FieldElement>() -> impl QueryCallback<T> + 'a {
    move |query: &str| -> Result<Option<T>, String> {
        let (id, data) = parse_query(query)?;
        match id {
            "None" => Ok(None),
            "PrintChar" => {
                assert_eq!(data.len(), 1);
                print!(
                    "{}",
                    data[0]
                        .parse::<u8>()
                        .map_err(|e| format!("Invalid char to print: {e}"))?
                        as char
                );
                Ok(Some(0.into()))
            }
            "Hint" => {
                assert_eq!(data.len(), 1);
                Ok(Some(T::from_str(data[0]).unwrap()))
            }
            _ => Err(format!("Unsupported query: {query}")),
        }
    }
}
