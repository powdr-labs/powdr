//! Utilites for generating initialization code that stores data objects in memory.

use std::collections::BTreeMap;

use crate::{data_parser::DataValue, utils::next_multiple_of_four};

pub enum SingleDataValue<'a> {
    Value(u32),
    LabelReference(&'a String),
    Offset(&'a String, &'a String),
}

pub fn store_data_objects<'a>(
    objects: impl IntoIterator<Item = &'a (String, Vec<DataValue>)> + Copy,
    mut memory_start: u32,
    code_gen: &mut dyn FnMut(u32, SingleDataValue) -> Vec<String>,
) -> (Vec<String>, BTreeMap<String, u32>) {
    memory_start = ((memory_start + 7) / 8) * 8;
    let mut current_pos = memory_start;
    let mut positions = BTreeMap::new();
    for (name, data) in objects.into_iter() {
        // TODO check if we need to use multiples of four.
        let size: u32 = data
            .iter()
            .map(|d| next_multiple_of_four(d.size()) as u32)
            .sum();
        positions.insert(name.clone(), current_pos);
        current_pos += size;
    }

    let code = objects
        .into_iter()
        .filter(|(_, data)| !data.is_empty())
        .flat_map(|(name, data)| {
            let mut object_code = vec![];
            let mut pos = positions[name];
            for item in data {
                match &item {
                    DataValue::Zero(_length) => {
                        // We can assume memory to be zero-initialized,
                        // so we do nothing.
                    }
                    DataValue::Direct(bytes) => {
                        for i in (0..bytes.len()).step_by(4) {
                            let v = (0..4)
                                .map(|j| {
                                    (bytes.get(i + j).cloned().unwrap_or_default() as u32)
                                        << (j * 8)
                                })
                                .reduce(|a, b| a | b)
                                .unwrap();
                            // We can assume memory to be zero-initialized.
                            if v != 0 {
                                object_code
                                    .extend(code_gen(pos + i as u32, SingleDataValue::Value(v)));
                            }
                        }
                    }
                    DataValue::Reference(sym) => {
                        object_code.extend(if let Some(p) = positions.get(sym) {
                            code_gen(pos, SingleDataValue::Value(*p))
                        } else {
                            // code reference
                            code_gen(pos, SingleDataValue::LabelReference(sym))
                        })
                    }
                    DataValue::Offset(l, r) => {
                        object_code.extend(code_gen(pos, SingleDataValue::Offset(l, r)));
                    }
                }
                pos += item.size() as u32;
            }
            if let Some(first_line) = object_code.first_mut() {
                *first_line = format!("// data {name}\n") + first_line;
            }
            object_code
        })
        .collect();
    (code, positions)
}
