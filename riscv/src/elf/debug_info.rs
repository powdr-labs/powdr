use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap},
};

use gimli::{
    read::AttributeValue, DebuggingInformationEntry, Dwarf, EndianSlice, LittleEndian, Operation,
    Unit, UnitRef,
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

struct DataInfo<'a> {
    pub var_full_name: Vec<Cow<'a, str>>,
    pub file_line: Option<(u64, u64)>,
}

impl DebugInfo {
    /// Extracts debug information from the ELF file, if available.
    pub fn new<D>(
        elf: &Elf,
        file_buffer: &[u8],
        data_entries: &BTreeMap<u32, D>,
        jump_targets: &BTreeSet<u32>,
    ) -> Result<DebugInfo, Error> {
        let dwarf = Self::load_dwarf_sections(elf, file_buffer)?;

        let mut file_list = Vec::new();
        let mut line_locations = BTreeMap::new();
        let mut text_symbols = Vec::new();
        // The code gen API allows for just one symbol per address, so we
        // can use a map directly.
        let mut data_symbols = BTreeMap::new();

        // Iterate over the compilation units:
        let mut units_iter = dwarf.units();
        while let Some(unit) = units_iter.next()? {
            let unit = dwarf.unit(unit)?;
            // Shadows the Unit with a reference to itself, because it is more
            // convenient to work with a UnitRef.
            let unit = UnitRef::new(&dwarf, &unit);

            // Traverse all the line locations for the compilation unit.
            let file_idx_delta = file_list.len() as i64 - 1;
            if let Some(line_program) = unit.line_program.clone() {
                // Get the source file listing
                for file_entry in line_program.header().file_names() {
                    let directory = file_entry
                        .directory(line_program.header())
                        .map(|attr| as_str(unit, attr))
                        .transpose()?
                        .unwrap_or("");
                    let path = as_str(unit, file_entry.path_name())?;

                    file_list.push((directory, path));
                }

                // Get the locations indexed by address
                let mut rows = line_program.rows();
                while let Some((_, row)) = rows.next_row()? {
                    line_locations.insert(
                        row.address() as u32,
                        (
                            (row.file_index() as i64 + file_idx_delta) as u64,
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

            // Traverse the in which the information about the compilation unit is stored.
            // We need to start the stack with a placeholder value, because the update
            // algorithm replaces the top element.
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
                            text_symbols.push((Cow::Borrowed(linkage_name.to_string()?), address));
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
                                file_line = Some(((file_idx as i64 + file_idx_delta) as u64, line));
                            }
                        }

                        data_symbols.insert(
                            address,
                            DataInfo {
                                var_full_name: full_name.clone(),
                                file_line,
                            },
                        );
                    }
                    _ => {}
                };
            }
        }

        // Deduplicate the text symbols
        dedup_names(&mut text_symbols);
        let text_symbols = text_symbols
            .into_iter()
            .map(|(name, address)| (address, name))
            .sorted()
            .collect::<Vec<_>>();

        // TODO: assemble the DebugInfo struct and return it

        Ok(DebugInfo)
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

/// Deduplicates by removing identical entries and appending the address to
/// repeated names. The vector ends up sorted.
fn dedup_names(symbols: &mut Vec<(Cow<str>, u32)>) {
    while dedup_names_pass(symbols) {}
}

/// Deduplicates the names of the symbols by appending one level of address to
/// the name.
///
/// Returns `true` if the names were deduplicated.
fn dedup_names_pass(symbols: &mut Vec<(Cow<str>, u32)>) -> bool {
    symbols.sort_unstable();
    symbols.dedup();

    let mut deduplicated = false;
    let mut iter = symbols.iter_mut();

    // The first unique name defines a group, which ends on the next unique name.
    // The whole group is deduplicated if it contains more than one element.
    let mut next_group = iter.next().map(|(name, address)| (name, *address));
    while let Some((group_name, group_address)) = next_group {
        let mut group_deduplicated = false;
        next_group = None;

        // Find duplicates and update names in the group
        for (name, address) in &mut iter {
            if name == group_name {
                group_deduplicated = true;
                deduplicated = true;
                *name = Cow::Owned(format!("{name}_{address:08x}"));
            } else {
                next_group = Some((name, *address));
                break;
            }
        }

        // If there were duplicates in the group, update the group leader, too.
        if group_deduplicated {
            *group_name = Cow::Owned(format!("{group_name}_{group_address:08x}"));
        }
    }

    deduplicated
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_pass_dedup_names() {
        let mut symbols = vec![
            (Cow::Borrowed("baz"), 0x8000),
            (Cow::Borrowed("bar"), 0x3000),
            (Cow::Borrowed("foo"), 0x1000),
            (Cow::Borrowed("bar"), 0x5000),
            (Cow::Borrowed("foo"), 0x2000),
            (Cow::Borrowed("baz"), 0x7000),
            (Cow::Borrowed("baz"), 0x9000),
            (Cow::Borrowed("doo"), 0x0042),
            (Cow::Borrowed("baz"), 0xa000),
            (Cow::Borrowed("baz"), 0x6000),
            (Cow::Borrowed("bar"), 0x4000),
        ];

        super::dedup_names(&mut symbols);

        let expected = vec![
            (Cow::Borrowed("bar_00003000"), 0x3000),
            (Cow::Borrowed("bar_00004000"), 0x4000),
            (Cow::Borrowed("bar_00005000"), 0x5000),
            (Cow::Borrowed("baz_00006000"), 0x6000),
            (Cow::Borrowed("baz_00007000"), 0x7000),
            (Cow::Borrowed("baz_00008000"), 0x8000),
            (Cow::Borrowed("baz_00009000"), 0x9000),
            (Cow::Borrowed("baz_0000a000"), 0xa000),
            (Cow::Borrowed("doo"), 0x0042),
            (Cow::Borrowed("foo_00001000"), 0x1000),
            (Cow::Borrowed("foo_00002000"), 0x2000),
        ];
        assert_eq!(symbols, expected);
    }

    #[test]
    fn multi_pass_dedup_names() {
        let mut symbols = vec![
            (Cow::Borrowed("john"), 0x42),
            (Cow::Borrowed("john"), 0x87),
            (Cow::Borrowed("john"), 0x1aa),
            (Cow::Borrowed("john_000001aa"), 0x1aa),
            (Cow::Borrowed("john_00000042"), 0x103),
            (Cow::Borrowed("john_00000087"), 0x103),
        ];

        super::dedup_names(&mut symbols);

        let expected = vec![
            (Cow::Borrowed("john_00000042_00000042"), 0x42),
            (Cow::Borrowed("john_00000042_00000103"), 0x103),
            (Cow::Borrowed("john_00000087_00000087"), 0x87),
            (Cow::Borrowed("john_00000087_00000103"), 0x103),
            (Cow::Borrowed("john_000001aa"), 0x1aa),
        ];

        assert_eq!(symbols, expected);
    }
}
