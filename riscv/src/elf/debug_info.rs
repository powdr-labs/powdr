use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap},
};

use gimli::{
    read::AttributeValue, DebuggingInformationEntry, Dwarf, EndianSlice, LittleEndian, Operation,
    Unit, UnitRef,
};
use goblin::elf::{Elf, SectionHeader};

use super::dedup_names;

type Reader<'a> = EndianSlice<'a, LittleEndian>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("no debug information available")]
    NoDebugInfo,
    #[error("unexpected organization of debug information")]
    UnexpectedOrganization,
    #[error("failed to parse debug information: {0}")]
    Parsing(#[from] gimli::Error),
}

/// Debug information extracted from the ELF file.
pub struct DebugInfo {
    /// List of source files: (directory, file name).
    pub file_list: Vec<(String, String)>,
    /// Maps addresses to line locations: (file index, line, column).
    pub line_locations: BTreeMap<u32, (usize, u32, u32)>,
    /// Maps (addresses, disambiguator) to symbol names. The disambiguator is
    /// used to distinguish between multiple symbols at the same address. (i.e.
    /// turns BTreeMap into a multimap.)
    pub symbol_map: BTreeMap<(u32, u32), String>,
}

impl DebugInfo {
    /// Extracts debug information from the ELF file, if available.
    pub fn new<D>(
        elf: &Elf,
        file_buffer: &[u8],
        data_entries: &BTreeMap<u32, D>,
        jump_targets: &BTreeSet<u32>,
    ) -> Result<Self, Error> {
        let dwarf = load_dwarf_sections(elf, file_buffer)?;

        let mut file_list = Vec::new();
        let mut line_locations = BTreeMap::new();
        let mut text_symbols = Vec::new();
        let mut symbol_map = BTreeMap::new();

        let mut map_disambiguator = 0u32..;

        // Iterate over the compilation units:
        let mut units_iter = dwarf.units();
        while let Some(unit) = units_iter.next()? {
            let unit = dwarf.unit(unit)?;
            // Shadows the Unit with a reference to itself, because it is more
            // convenient to work with a UnitRef.
            let unit = UnitRef::new(&dwarf, &unit);

            // Traverse all the line locations for the compilation unit.
            let file_idx_delta = file_list.len() as isize - 1;
            if let Some(line_program) = unit.line_program.clone() {
                // Get the source file listing
                for file_entry in line_program.header().file_names() {
                    let directory = file_entry
                        .directory(line_program.header())
                        .map(|attr| as_str(unit, attr))
                        .transpose()?
                        .unwrap_or("");
                    let path = as_str(unit, file_entry.path_name())?;

                    file_list.push((directory.to_owned(), path.to_owned()));
                }

                // Get the locations indexed by address
                let mut rows = line_program.rows();
                while let Some((_, row)) = rows.next_row()? {
                    line_locations.insert(
                        row.address() as u32,
                        (
                            (row.file_index() as isize + file_idx_delta) as usize,
                            match row.line() {
                                None => 0,
                                Some(v) => v.get() as u32,
                            },
                            match row.column() {
                                gimli::ColumnType::LeftEdge => 0,
                                gimli::ColumnType::Column(v) => v.get() as u32,
                            },
                        ),
                    );
                }
            }

            // Traverse the tree in which the information about the compilation
            // unit is stored. To simplify the algorithm, we start the name
            // stack with a placeholder value.
            let mut full_name = vec![Cow::default()];
            let mut entries = unit.entries();
            while let Some((level_delta, entry)) = entries.next_dfs()? {
                // Get the entry name as a human readable string (this is used in a comment)
                let name = find_attr(entry, gimli::DW_AT_name)
                    .map(|name| unit.attr_string(name).map(|s| s.to_string_lossy()))
                    .transpose()?
                    .unwrap_or(Cow::Borrowed("?"));

                match level_delta {
                    delta if delta > 1 => return Err(Error::UnexpectedOrganization),
                    1 => (),
                    _ => {
                        full_name.truncate((full_name.len() as isize + level_delta - 1) as usize);
                    }
                }
                full_name.push(name);

                match entry.tag() {
                    // This is the entry for a function or method.
                    gimli::DW_TAG_subprogram => {
                        let Some(linkage_name) = find_attr(entry, gimli::DW_AT_linkage_name)
                            .map(|ln| unit.attr_string(ln))
                            .transpose()?
                        else {
                            log::warn!("No linkage name for function or method in debug symbols. Ignoring.");
                            continue;
                        };

                        let Some(address) = parse_address(&unit, entry)? else {
                            continue;
                        };

                        if jump_targets.contains(&address) {
                            text_symbols.push((linkage_name.to_string()?.to_owned(), address));
                        }
                    }
                    // This is the entry for a variable.
                    gimli::DW_TAG_variable => {
                        let Some(address) = parse_address(&unit, entry)? else {
                            continue;
                        };

                        if !data_entries.contains_key(&address) {
                            continue;
                        }

                        let mut file_line = None;
                        if let Some(AttributeValue::FileIndex(file_idx)) =
                            find_attr(entry, gimli::DW_AT_decl_file)
                        {
                            if let Some(AttributeValue::Udata(line)) =
                                find_attr(entry, gimli::DW_AT_decl_line)
                            {
                                file_line =
                                    Some(((file_idx as isize + file_idx_delta) as usize, line));
                            }
                        }

                        let value = format!(
                            "{}{}",
                            full_name.join("::"),
                            if let Some((file, line)) = file_line {
                                format!(" at file {file} line {line}")
                            } else {
                                String::new()
                            }
                        );

                        symbol_map.insert((address, map_disambiguator.next().unwrap()), value);
                    }
                    _ => {}
                };
            }
        }

        // Deduplicate the text symbols
        dedup_names(&mut text_symbols);

        symbol_map.extend(text_symbols.into_iter().map(|(name, address)| {
            (
                (address, map_disambiguator.next().unwrap()),
                name.to_string(),
            )
        }));

        Ok(DebugInfo {
            file_list,
            line_locations,
            symbol_map,
        })
    }
}

fn load_dwarf_sections<'a>(elf: &Elf, file_buffer: &'a [u8]) -> Result<Dwarf<Reader<'a>>, Error> {
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
                    &file_buffer[shdr.sh_offset as usize..(shdr.sh_offset + shdr.sh_size) as usize]
                })
                .unwrap_or(&[]),
            Default::default(),
        ))
    })
    .unwrap())
}

fn find_attr<'a>(
    entry: &DebuggingInformationEntry<Reader<'a>>,
    attr_type: gimli::DwAt,
) -> Option<AttributeValue<Reader<'a>>> {
    let mut attrs = entry.attrs();
    while let Some(attr) = attrs.next().unwrap() {
        if attr.name() == attr_type {
            return Some(attr.value());
        }
    }
    None
}

fn as_str<'a>(
    unit: UnitRef<Reader<'a>>,
    attr: AttributeValue<Reader<'a>>,
) -> Result<&'a str, gimli::Error> {
    unit.attr_string(attr)?.to_string()
}

fn parse_address(
    unit: &Unit<Reader>,
    entry: &DebuggingInformationEntry<Reader>,
) -> Result<Option<u32>, gimli::Error> {
    let Some(AttributeValue::Exprloc(address)) = find_attr(entry, gimli::DW_AT_location) else {
        log::warn!("Address of a debug symbol in an unsupported format. Ignoring.");
        return Ok(None);
    };

    // Do the magic to find the variable address
    let mut ops = address.operations(unit.encoding());
    let first_op = ops.next()?;
    let second_op = ops.next()?;
    let (Some(Operation::Address { address }), None) = (first_op, second_op) else {
        log::warn!("Address of a debug symbol in an unsupported format. Ignoring.");
        return Ok(None);
    };

    Ok(Some(address as u32))
}
