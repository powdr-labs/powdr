use std::collections::HashMap;

use gimli::{
    read::Attribute, AttributeValue, AttrsIter, CloneStableDeref, DebugLineOffset, DebugStr,
    DebuggingInformationEntry, Dwarf, EndianSlice, LittleEndian, LocationListsOffset, Unit,
    UnitRef,
};
use goblin::elf::{Elf, SectionHeader};
use itertools::Itertools;

pub struct DebugInfo;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("no debug information available")]
    NoDebugInfo,
    #[error("unexpected organization of debug information")]
    UnexpectedOrganization,
    #[error("failed to parse debug information: {0}")]
    ParseError(#[from] gimli::Error),
}

type Reader<'a> = EndianSlice<'a, LittleEndian>;

impl DebugInfo {
    /// Extracts debug information from the ELF file, if available.
    pub fn new(elf: &Elf, file_buffer: &[u8]) -> Result<DebugInfo, Error> {
        let dwarf = Self::load_dwarf_sections(elf, file_buffer)?;

        // Iterate over the compilation units:
        let mut units_iter = dwarf.units();
        while let Some(unit) = units_iter.next()? {
            let unit = dwarf.unit(unit)?;
            // Shadows the Unit with a reference to itself, because it is more
            // convenient to work with a UnitRef.
            let unit = UnitRef::new(&dwarf, &unit);

            let mut level = 0;
            let mut tag_path = Vec::new();

            // Traverse the in which the information about the compilation unit is stored.
            let mut entries = unit.entries();
            while let Some((level_delta, entry)) = entries.next_dfs()? {
                level += level_delta;

                match level_delta {
                    delta if delta > 1 => return Err(Error::UnexpectedOrganization),
                    1 => tag_path.push(entry.tag()),
                    _ => tag_path.truncate(level as usize),
                }

                match entry.tag() {
                    // This is the entry for a function or method.
                    gimli::DW_TAG_subprogram => {
                        let name = as_str(unit, find_attr(&entry, gimli::DW_AT_linkage_name)?)?;
                        println!("{}\n  {}) ", tag_path.iter().format("/"), name);
                    }
                    _ => {}
                };

                // Traverse all the line locations for the compilation unit.
                if let Some(line_program) = unit.line_program.clone() {
                    let mut rows = line_program.rows();
                    while let Some((header, row)) = rows.next_row()? {
                        println!(
                            "{:08x}: {} {:?} {:?}",
                            row.address(),
                            row.file_index(),
                            row.line(),
                            row.column()
                        );
                    }
                }

                /*
                println!(
                    "{:indent$}# {}",
                    "",
                    entry.tag().static_string().unwrap(),
                    indent = level as usize * 2
                );
                let mut attrs = entry.attrs();

                while let Some(attr) = attrs.next()? {
                    let value_type = match attr.value() {
                        AttributeValue::Addr(_) => "Addr",
                        AttributeValue::Block(_) => "Block",
                        AttributeValue::Data1(_) => "Data1",
                        AttributeValue::Data2(_) => "Data2",
                        AttributeValue::Data4(_) => "Data4",
                        AttributeValue::Data8(_) => "Data8",
                        AttributeValue::Sdata(_) => "Sdata",
                        AttributeValue::Udata(_) => "Udata",
                        AttributeValue::Exprloc(_) => "Exprloc",
                        AttributeValue::Flag(_) => "Flag",
                        AttributeValue::SecOffset(_) => "SecOffset",
                        AttributeValue::DebugAddrBase(_) => "DebugAddrBase",
                        AttributeValue::DebugAddrIndex(_) => "DebugAddrIndex",
                        AttributeValue::UnitRef(_) => "UnitRef",
                        AttributeValue::DebugInfoRef(_) => "DebugInfoRef",
                        AttributeValue::DebugInfoRefSup(_) => "DebugInfoRefSup",
                        AttributeValue::DebugLineRef(_) => "DebugLineRef",
                        AttributeValue::LocationListsRef(_) => "LocationListsRef",
                        AttributeValue::DebugLocListsBase(_) => "DebugLocListsBase",
                        AttributeValue::DebugLocListsIndex(_) => "DebugLocListsIndex",
                        AttributeValue::DebugMacinfoRef(_) => "DebugMacinfoRef",
                        AttributeValue::DebugMacroRef(_) => "DebugMacroRef",
                        AttributeValue::RangeListsRef(_) => "RangeListsRef",
                        AttributeValue::DebugRngListsBase(_) => "DebugRngListsBase",
                        AttributeValue::DebugRngListsIndex(_) => "DebugRngListsIndex",
                        AttributeValue::DebugTypesRef(_) => "DebugTypesRef",
                        AttributeValue::DebugStrRef(_) => "DebugStrRef",
                        AttributeValue::DebugStrRefSup(_) => "DebugStrRefSup",
                        AttributeValue::DebugStrOffsetsBase(_) => "DebugStrOffsetsBase",
                        AttributeValue::DebugStrOffsetsIndex(_) => "DebugStrOffsetsIndex",
                        AttributeValue::DebugLineStrRef(_) => "DebugLineStrRef",
                        AttributeValue::String(_) => "String",
                        AttributeValue::Encoding(_) => "Encoding",
                        AttributeValue::DecimalSign(_) => "DecimalSign",
                        AttributeValue::Endianity(_) => "Endianity",
                        AttributeValue::Accessibility(_) => "Accessibility",
                        AttributeValue::Visibility(_) => "Visibility",
                        AttributeValue::Virtuality(_) => "Virtuality",
                        AttributeValue::Language(_) => "Language",
                        AttributeValue::AddressClass(_) => "AddressClass",
                        AttributeValue::IdentifierCase(_) => "IdentifierCase",
                        AttributeValue::CallingConvention(_) => "CallingConvention",
                        AttributeValue::Inline(_) => "Inline",
                        AttributeValue::Ordering(_) => "Ordering",
                        AttributeValue::FileIndex(_) => "FileIndex",
                        AttributeValue::DwoId(_) => "DwoId",
                    };

                    println!(
                        "{:indent$}→ {}: {}({})",
                        "",
                        attr.name(),
                        value_type,
                        as_str(unit, attr.value()).unwrap_or("<not a string>"),
                        indent = level as usize * 2 + 2
                    );
                }*/
            }
        }
        todo!()
    }

    fn load_dwarf_sections<'a>(
        elf: &Elf,
        file_buffer: &'a [u8],
    ) -> Result<Dwarf<Reader<'a>>, Error> {
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

        // Load the DWARF sections:
        Ok(gimli::Dwarf::load(move |section| {
            Ok::<_, ()>(Reader::new(
                debug_sections
                    .get(section.name())
                    .map(|shdr| {
                        &file_buffer
                            [shdr.sh_offset as usize..(shdr.sh_offset + shdr.sh_size) as usize]
                    })
                    .unwrap_or(&[]),
                Default::default(),
            ))
        })
        .unwrap())
    }
}

fn find_attr<'a>(
    entry: &DebuggingInformationEntry<Reader<'a>>,
    attr_type: gimli::DwAt,
) -> Result<AttributeValue<Reader<'a>>, Error> {
    let mut attrs = entry.attrs();
    while let Some(attr) = attrs.next().unwrap() {
        if attr.name() == attr_type {
            return Ok(attr.value());
        }
    }
    Err(Error::UnexpectedOrganization)
}

fn as_str<'a>(
    unit: UnitRef<Reader<'a>>,
    attr: AttributeValue<Reader<'a>>,
) -> Result<&'a str, gimli::Error> {
    std::str::from_utf8(unit.attr_string(attr)?.slice()).map_err(|_| gimli::Error::BadUtf8)
}
