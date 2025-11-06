use std::{fs::read, path::PathBuf, str::FromStr};

use eyre::Result;
use openvm_sdk::{StdIn, F};
use openvm_stark_backend::p3_field::FieldAlgebra;

/// Input can be either:
/// (1) one single hex string
/// (2) A JSON file containing an array of hex strings.
/// Each hex string (either in the file or the direct input) is either:
/// - Hex strings of bytes, which is prefixed with 0x01
/// - Hex strings of native field elements (represented as u32, little endian), prefixed with 0x02
#[derive(Debug, Clone)]
pub enum Input {
    FilePath(PathBuf),
    HexBytes(Vec<u8>),
}

impl FromStr for Input {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(bytes) = decode_hex_string(s) {
            Ok(Input::HexBytes(bytes))
        } else if PathBuf::from(s).exists() {
            Ok(Input::FilePath(PathBuf::from(s)))
        } else {
            Err("Input must be a valid file path or a hex string of even length.".to_string())
        }
    }
}

pub fn decode_hex_string(s: &str) -> Result<Vec<u8>> {
    // Remove 0x prefix if present (exactly once)
    let s = s.strip_prefix("0x").unwrap_or(s);
    if s.len() % 2 != 0 {
        return Err(eyre::eyre!("The hex string must be of even length"));
    }
    if !s.chars().all(|c| c.is_ascii_hexdigit()) {
        return Err(eyre::eyre!("The hex string must consist of hex digits"));
    }
    if s.starts_with("02") {
        if s.len() % 8 != 2 {
            return Err(eyre::eyre!(
                "If the hex value starts with 02, a whole number of 32-bit elements must follow"
            ));
        }
    } else if !s.starts_with("01") {
        return Err(eyre::eyre!("The hex value must start with 01 or 02"));
    }
    hex::decode(s).map_err(|e| eyre::eyre!("Invalid hex: {}", e))
}

pub fn read_bytes_into_stdin(stdin: &mut StdIn, bytes: &[u8]) -> Result<()> {
    // should either write_bytes or write_field
    match bytes.first() {
        Some(0x01) => {
            stdin.write_bytes(&bytes[1..]);
            Ok(())
        }
        Some(0x02) => {
            let data = &bytes[1..];
            if data.len() % 4 != 0 {
                return Err(eyre::eyre!(
                    "Invalid input format: incorrect number of bytes"
                ));
            }
            let mut fields = Vec::with_capacity(data.len() / 4);
            for chunk in data.chunks_exact(4) {
                let value = u32::from_le_bytes(chunk.try_into().unwrap());
                fields.push(F::from_canonical_u32(value));
            }
            stdin.write_field(&fields);
            Ok(())
        }
        _ => Err(eyre::eyre!(
            "Invalid input format: the first byte must be 0x01 or 0x02"
        )),
    }
}

pub fn read_to_stdin(input: &Option<Input>) -> Result<StdIn> {
    match input {
        Some(Input::FilePath(path)) => {
            let mut stdin = StdIn::default();
            // read the json
            let bytes = read(path)?;
            let json: serde_json::Value = serde_json::from_slice(&bytes)?;
            json["input"]
                .as_array()
                .ok_or_else(|| eyre::eyre!("Input must be an array under 'input' key"))?
                .iter()
                .try_for_each(|inner| {
                    inner
                        .as_str()
                        .ok_or_else(|| eyre::eyre!("Each value must be a hex string"))
                        .and_then(|s| match decode_hex_string(s) {
                            Err(msg) => Err(eyre::eyre!("Invalid hex string: {}", msg)),
                            Ok(bytes) => {
                                read_bytes_into_stdin(&mut stdin, &bytes).expect("Fail: input validation accepted an input, but the deserialization rejected it");
                                Ok(())
                            }
                        })
                })?;

            Ok(stdin)
        }
        Some(Input::HexBytes(bytes)) => {
            let mut stdin = StdIn::default();
            read_bytes_into_stdin(&mut stdin, bytes)?;
            Ok(stdin)
        }
        None => Ok(StdIn::default()),
    }
}
