//! Utilities for generating initialization code that stores data objects in memory.

use std::collections::BTreeMap;

use crate::{
    data_parser::DataValue,
    utils::{alignment_size, next_aligned},
};

/// A single 32-bit data value.
pub enum SingleDataValue {
    /// A literal value.
    Value(u32),
    /// The value of a pointer to a text label. Since there is no 1-to-1
    /// correspondence between RISC-V and Powdr ASM instructions, this is
    /// passed unresolved to the code generator.
    LabelReference(String),
    /// Currently not supported.
    Offset(String, String),
}

struct WordWriter<'a> {
    data_writer: &'a mut dyn FnMut(Option<String>, u32, SingleDataValue),
    partial: u32,
    current_pos: u32,
    latest_label: Option<String>,
}

impl<'a> WordWriter<'a> {
    fn new(
        starting_pos: u32,
        data_writer: &'a mut dyn FnMut(Option<String>, u32, SingleDataValue),
    ) -> Self {
        // sanitary alignment to 8 bytes
        let current_pos = next_aligned(starting_pos as usize, 8) as u32;
        Self {
            partial: 0,
            current_pos,
            data_writer,
            latest_label: None,
        }
    }

    fn current_position(&self) -> u32 {
        self.current_pos
    }

    fn set_label(&mut self, label: String) {
        self.latest_label = Some(label)
    }

    fn advance(&mut self, bytes: u32) {
        let next_pos = self.current_pos + bytes;

        // if changed words, flush
        let curr_word = self.current_pos & (!0b11);
        if (next_pos & (!0b11) != curr_word) && (self.partial != 0) {
            (*self.data_writer)(
                std::mem::take(&mut self.latest_label),
                curr_word,
                SingleDataValue::Value(self.partial),
            );
            self.partial = 0;
        }
        self.current_pos = next_pos;
    }

    fn align(&mut self, alignment: u32, pad_value: u8) {
        let padding_size = alignment_size(self.current_pos as usize, alignment as usize);
        if padding_size != 0 {
            if pad_value == 0 {
                self.advance(padding_size as u32);
            } else {
                self.write_bytes(std::iter::repeat(pad_value).take(padding_size));
            }
        }
    }

    fn write_bytes<I: IntoIterator<Item = u8>>(&mut self, bytes: I) {
        for b in bytes {
            self.partial |= (b as u32) << (8 * (self.current_pos % 4));
            self.advance(1);
        }
    }

    fn write_label_reference(&mut self, label: String) {
        assert_eq!(
            self.current_pos % 4,
            0,
            "reference to code labels in misaligned data section is not supported"
        );

        (*self.data_writer)(
            std::mem::take(&mut self.latest_label),
            self.current_pos,
            SingleDataValue::LabelReference(label),
        );

        assert_eq!(self.partial, 0);
        self.current_pos += 4;
    }

    fn finish(mut self) {
        // ensure the latest partial word is written
        self.advance(4);
    }
}

pub fn store_data_objects(
    sections: Vec<Vec<(Option<String>, Vec<DataValue>)>>,
    memory_start: u32,
    code_gen: &mut dyn FnMut(Option<String>, u32, SingleDataValue),
    positions: &mut BTreeMap<String, u32>,
) {
    let mut writer = WordWriter::new(memory_start, code_gen);

    let mut current_pos = writer.current_position();
    for (name, data) in sections.iter().flatten() {
        if let Some(name) = name {
            positions.insert(name.clone(), current_pos);
        }
        for d in data.iter() {
            current_pos += d.size(current_pos as usize) as u32;
        }
    }

    for (name, data) in sections.into_iter().flatten() {
        if let Some(name) = name {
            writer.set_label(name);
        }
        for item in data {
            match item {
                DataValue::Zero(length) => {
                    // We can assume memory to be zero-initialized, so we
                    // just have to advance.
                    writer.advance(length as u32);
                }
                DataValue::Direct(bytes) => {
                    writer.write_bytes(bytes.iter().copied());
                }
                DataValue::Reference(sym) => {
                    if let Some(p) = positions.get(&sym) {
                        writer.write_bytes(p.to_le_bytes().iter().copied());
                    } else {
                        // code reference
                        writer.write_label_reference(sym);
                    }
                }
                DataValue::Alignment(bytes, pad_value) => {
                    writer.align(bytes as u32, pad_value);
                }
                DataValue::Offset(_l, _r) => unimplemented!(),
            }
        }
    }
    writer.finish();
}
