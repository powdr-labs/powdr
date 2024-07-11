use std::collections::HashMap;

use gimli::{
    read::Attribute, AttrsIter, CloneStableDeref, DebugStr, Dwarf, EndianReader, LittleEndian, Unit,
};
use goblin::elf::{Elf, SectionHeader};

pub struct DebugInfo;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("no debug information available")]
    NoDebugInfo,
    #[error("failed to parse debug information: {0}")]
    ParseError(#[from] gimli::Error),
}

impl DebugInfo {
    /// Extracts debug information from the ELF file, if available.
    pub fn new(elf: &Elf, file_buffer: &[u8]) -> Result<DebugInfo, Error> {
        // Index the sections by their names:
        let debug_sections: HashMap<&str, &SectionHeader> = elf
            .section_headers
            .iter()
            .filter_map(|shdr| {
                elf.shdr_strtab
                    .get_at(shdr.sh_name)
                    .map(|name| (name, shdr))
            })
            .collect();

        if debug_sections.is_empty() {
            return Err(Error::NoDebugInfo);
        }

        let dwarf = gimli::Dwarf::load(move |section| {
            Ok::<_, ()>(EndianReader::new(
                debug_sections
                    .get(section.name())
                    .map(|shdr| {
                        &file_buffer
                            [shdr.sh_offset as usize..(shdr.sh_offset + shdr.sh_size) as usize]
                    })
                    .unwrap_or(&[]),
                gimli::LittleEndian,
            ))
        })
        .unwrap();

        let mut units_iter = dwarf.units();
        while let Some(unit) = units_iter.next()? {
            let unit = dwarf.unit(unit)?;

            let mut level = 0;
            //let mut current_compilation_unit = None;

            let mut entries = unit.entries();
            while let Some((level_delta, entry)) = entries.next_dfs()? {
                level += level_delta;
                match entry.tag() {
                    gimli::DW_TAG_compile_unit => {
                        let mut name = None;
                        let mut path = None;

                        let mut attrs = entry.attrs();
                        while let Some(attr) = attrs.next()? {
                            match attr.name() {
                                gimli::DW_AT_name => {
                                    name = Some(std::str::from_utf8(
                                        dwarf.attr_string(&unit, attr.value())?.bytes(),
                                    ));
                                }
                                gimli::DW_AT_comp_dir => {
                                    path = Some(std::str::from_utf8(
                                        dwarf.attr_string(&unit, attr.value())?.bytes(),
                                    ));
                                }
                                _ => {}
                            }
                        }
                    }
                    gimli::DW_TAG_subprogram => {}
                    _ => {}
                };

                println!(
                    "{:indent$}# {}",
                    "",
                    entry.tag().static_string().unwrap(),
                    indent = level as usize * 2
                );
                let mut attrs = entry.attrs();
                while let Some(attr) = attrs.next()? {
                    println!(
                        "{:indent$}→ {}",
                        "",
                        attr.name(),
                        indent = level as usize * 2 + 2
                    );
                }
            }
        }
        todo!()
    }
}
