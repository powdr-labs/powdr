use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use powdr_ast::analyzed::Analyzed;
use powdr_number::{FieldElement, KnownField};

// This is the magic number for the .pilo file format. It spells "powdr" in ASCII.
// The UNIX magic file format for this file would be
// 8       bestring16          powdr      Powdr PIL binary object
const MAGIC: [u8; 5] = [0x70, 0x6f, 0x77, 0x64, 0x72];

#[derive(Serialize, Deserialize)]
pub struct SerializedAnalyzed {
    magic: [u8; 5],
    version: u32,
    field: KnownField,
    analyzed: Vec<u8>,
}

impl<T: FieldElement> TryFrom<&Analyzed<T>> for SerializedAnalyzed {
    type Error = String;

    fn try_from(analyzed: &Analyzed<T>) -> Result<Self, Self::Error> {
        Ok(Self {
            magic: MAGIC,
            version: include!("../analyzed_type.version"),
            field: T::known_field().ok_or("Field not known")?,
            analyzed: analyzed.serialize()?,
        })
    }
}

impl<T: FieldElement> TryFrom<SerializedAnalyzed> for Analyzed<T> {
    type Error = String;

    fn try_from(serialized: SerializedAnalyzed) -> Result<Self, Self::Error> {
        serialized.check::<T>()?;
        Analyzed::deserialize(&serialized.analyzed)
    }
}

impl SerializedAnalyzed {
    pub fn check<T: FieldElement>(&self) -> Result<(), String> {
        let actual_version = include!("../analyzed_type.version");

        if self.magic != MAGIC {
            return Err("Invalid .pilo magic number".to_string());
        }

        if self.version != actual_version {
            return Err(format!(
                "Invalid .pilo version number. Expected {} but got {}",
                actual_version, self.version
            )
            .to_string());
        }

        let actual_field = T::known_field().ok_or("Field not known")?;

        if self.field != actual_field {
            return Err(format!(
                "Invalid .pilo field. Expected {:?} but got {:?}",
                self.field, actual_field
            )
            .to_string());
        }

        Ok(())
    }

    pub fn serialize_to(&self, path: PathBuf) -> Result<(), String> {
        serde_cbor::to_writer(
            &mut std::fs::File::create(path).map_err(|e| format!("Failed to create file: {e}"))?,
            self,
        )
        .map_err(|e| format!("Failed to serialize to file: {e}"))
    }

    pub fn deserialize_from(path: PathBuf) -> Result<Self, String> {
        serde_cbor::from_reader(
            std::fs::File::open(path).map_err(|e| format!("Failed to open file: {e}"))?,
        )
        .map_err(|e| format!("Failed to deserialize from file: {e}"))
    }
}
