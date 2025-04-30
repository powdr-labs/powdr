//! The main powdr lib, used to compile from assembly to PIL

pub mod pipeline;
pub mod test_runner;
pub mod test_util;
pub mod util;

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use serde::de::DeserializeOwned;

pub use pipeline::Pipeline;

pub use powdr_backend::{BackendType, Proof};
use powdr_executor::witgen::QueryCallback;

use powdr_number::FieldElement;

#[derive(Clone)]
pub struct HostContext {
    /// Simulates a file system where the guest can write to stdout, stderr, or any other file descriptor.
    /// After witgen the host can read what the guest wrote.
    pub file_data: Arc<Mutex<BTreeMap<u32, Vec<u8>>>>,
}

impl HostContext {
    pub fn new<T: FieldElement>() -> (Self, Arc<dyn QueryCallback<T>>) {
        let ctx = Self {
            file_data: Arc::new(Mutex::new(BTreeMap::<u32, Vec<u8>>::new())),
        };
        let cb = ctx.query_callback();
        (ctx, cb)
    }

    pub fn clear(&mut self) {
        let mut fs = self.file_data.lock().unwrap();
        fs.clear();
    }

    pub fn read<T: DeserializeOwned>(&self, fd: u32) -> Result<T, String> {
        let fs = self.file_data.lock().unwrap();
        if let Some(data) = fs.get(&fd) {
            serde_cbor::from_slice(data).map_err(|e| format!("Error deserializing data: {e}"))
        } else {
            Err(format!("File descriptor {fd} not found"))
        }
    }

    pub fn read_bytes(&self, fd: u32) -> Result<Vec<u8>, String> {
        let fs = self.file_data.lock().unwrap();
        if let Some(data) = fs.get(&fd) {
            Ok(data.clone())
        } else {
            Err(format!("File descriptor {fd} not found"))
        }
    }

    fn query_callback<T: FieldElement>(&self) -> Arc<dyn QueryCallback<T>> {
        let fs = self.file_data.clone();
        Arc::new(move |query: &str| -> Result<Option<T>, String> {
            let (id, data) = parse_query(query)?;
            match id {
                "Output" => {
                    assert_eq!(data.len(), 2);
                    let fd = data[0]
                        .parse::<u32>()
                        .map_err(|e| format!("Invalid fd: {e}"))?;
                    let byte = data[1]
                        .parse::<u8>()
                        .map_err(|e| format!("Invalid char to print: {e}"))?
                        as char;
                    match fd {
                        // stdin cannot be used for Output
                        0 => return Err(format!("Unsupported file descriptor: {fd}")),
                        _ => {
                            let mut map = fs.lock().unwrap();
                            map.entry(fd).or_default().push(byte as u8);
                        }
                    }
                    Ok(Some(0.into()))
                }
                "Clear" => {
                    fs.lock().unwrap().clear();
                    Ok(Some(0.into()))
                }
                _ => Err(format!("Unsupported query: {query}")),
            }
        })
    }
}

// TODO at some point, we could also just pass evaluator::Values around - would be much faster.
pub fn parse_query(query: &str) -> Result<(&str, Vec<&str>), String> {
    // We are expecting an enum value
    let query = query.strip_prefix("std::prelude::Query::").unwrap_or(query);
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

pub fn serde_data_to_query_callback<T: FieldElement>(
    channel: u32,
    bytes: Vec<u8>,
) -> impl QueryCallback<T> {
    move |query: &str| -> Result<Option<T>, String> {
        let (id, data) = parse_query(query)?;
        match id {
            "Input" => {
                let [cb_channel, index] = data[..] else {
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

pub fn dict_data_to_query_callback<T: FieldElement>(
    dict: BTreeMap<u32, Vec<T>>,
) -> impl QueryCallback<T> {
    move |query: &str| -> Result<Option<T>, String> {
        let (id, data) = parse_query(query)?;
        match id {
            "Input" => {
                let [cb_channel, index] = data[..] else {
                    panic!()
                };
                let cb_channel = cb_channel
                    .parse::<u32>()
                    .map_err(|e| format!("Error parsing callback data channel: {e})"))?;

                let Some(elems) = dict.get(&cb_channel) else {
                    return Err("Callback channel mismatch".to_string());
                };

                let index = index
                    .parse::<usize>()
                    .map_err(|e| format!("Error parsing index: {e})"))?;

                // query index 0 means the length
                Ok(Some(match index {
                    0 => (elems.len() as u64).into(),
                    index => elems[index - 1],
                }))
            }
            _ => Err(format!("Unsupported query: {query}")),
        }
    }
}

pub fn inputs_to_query_callback<T: FieldElement>(inputs: Vec<T>) -> impl QueryCallback<T> {
    let mut dict = BTreeMap::new();
    dict.insert(0, inputs);
    dict_data_to_query_callback(dict)
}

#[allow(clippy::print_stdout)]
pub fn handle_simple_queries_callback<'a, T: FieldElement>() -> impl QueryCallback<T> + 'a {
    move |query: &str| -> Result<Option<T>, String> {
        let (id, data) = parse_query(query)?;
        match id {
            "None" => Ok(None),
            "Output" => {
                assert_eq!(data.len(), 2);
                let fd = data[0]
                    .parse::<u32>()
                    .map_err(|e| format!("Invalid fd: {e}"))?;
                if fd != 0 {
                    return Err("Debug print requires output fd 0".to_string());
                }
                let byte = data[1]
                    .parse::<u8>()
                    .map_err(|e| format!("Invalid char to print: {e}"))?
                    as char;
                print!("{byte}");
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
