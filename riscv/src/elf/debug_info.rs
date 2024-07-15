use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap},
};

use gimli::{
    read::AttributeValue, DebuggingInformationEntry, Dwarf, EndianSlice, LittleEndian, Operation,
    Unit, UnitRef,
};
use goblin::elf::{
    sym::{STT_FUNC, STT_OBJECT},
    Elf, SectionHeader,
};
use itertools::Itertools;

use super::AddressMap;

type Reader<'a> = EndianSlice<'a, LittleEndian>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("no debug information available")]
    NoDebugInfo,
    #[error("DIE tree traversal skipped a level")]
    UnexpectedLevel,
    #[error("failed to parse debug information: {0}")]
    Parsing(#[from] gimli::Error),
}

/// Debug information extracted from the ELF file.
#[derive(Default)]
pub struct DebugInfo {
    /// List of source files: (directory, file name).
    pub file_list: Vec<(String, String)>,
    /// Relates addresses to source locations.
    pub source_locations: Vec<SourceLocationInfo>,
    /// Maps (addresses, disambiguator) to symbol names. The disambiguator is
    /// used to distinguish between multiple symbols at the same address. (i.e.
    /// turns BTreeMap into a multimap.)
    pub symbols: SymbolTable,
    /// Human readable notes about an address
    pub notes: HashMap<u32, String>,
}

pub struct SourceLocationInfo {
    pub address: u32,
    pub file: u64,
    pub line: u64,
    pub col: u64,
}

impl DebugInfo {
    /// Extracts debug information from the ELF file, if available.
    pub fn new<D>(
        elf: &Elf,
        file_buffer: &[u8],
        address_map: &AddressMap,
        data_entries: &BTreeMap<u32, D>,
        jump_targets: &BTreeSet<u32>,
    ) -> Result<Self, Error> {
        let dwarf = load_dwarf_sections(elf, file_buffer)?;

        let mut file_list = Vec::new();
        let mut source_locations = Vec::new();
        let mut text_symbols = Vec::new();
        let mut symbol_map = BTreeMap::new();
        let mut notes = HashMap::new();

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
                    source_locations.push(SourceLocationInfo {
                        address: row.address() as u32,
                        file: (row.file_index() as isize + file_idx_delta) as u64,
                        line: match row.line() {
                            None => 0,
                            Some(v) => v.get(),
                        },
                        col: match row.column() {
                            gimli::ColumnType::LeftEdge => 0,
                            gimli::ColumnType::Column(v) => v.get(),
                        },
                    })
                }
            }

            // Traverse the tree in which the information about the compilation
            // unit is stored. To simplify the algorithm, we start the name
            // stack with a placeholder value.
            let mut full_name = vec![None];
            let mut entries = unit.entries();
            while let Some((level_delta, entry)) = entries.next_dfs()? {
                // Get the entry name as a human readable string (this is used in a comment)
                let name = find_attr(entry, gimli::DW_AT_name)
                    .map(|name| unit.attr_string(name).map(|s| s.to_string_lossy()))
                    .transpose()?;

                match level_delta {
                    delta if delta > 1 => return Err(Error::UnexpectedLevel),
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

                        if full_name.last().is_some() {
                            // The human readable name of the variable is available,
                            // so we assemble a pretty note to go into the comment.
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
                                full_name
                                    .iter()
                                    .map(|s| match s {
                                        Some(s) => s,
                                        None => &Cow::Borrowed("?"),
                                    })
                                    .join("::"),
                                if let Some((file, line)) = file_line {
                                    format!(" at file {file} line {line}")
                                } else {
                                    String::new()
                                }
                            );

                            notes.insert(address, value);
                        }

                        // The variable symbol name is only used as a fallback
                        // in case there is no pretty note.
                        if let Some(linkage_name) = find_attr(entry, gimli::DW_AT_linkage_name)
                            .map(|ln| unit.attr_string(ln))
                            .transpose()?
                        {
                            symbol_map.insert(
                                (address, map_disambiguator.next().unwrap()),
                                linkage_name.to_string()?.to_owned(),
                            );
                        }
                    }
                    _ => {}
                };
            }
        }

        // Filter out the source locations that are not in the text section
        filter_locations_in_text(&mut source_locations, address_map);

        println!("##### number of text symbols: {}", text_symbols.len());
        // Deduplicate the text symbols
        dedup_names(&mut text_symbols);
        println!("##### number of text symbols: {}", text_symbols.len());

        symbol_map.extend(
            text_symbols
                .into_iter()
                .map(|(name, address)| ((address, map_disambiguator.next().unwrap()), name)),
        );

        Ok(DebugInfo {
            file_list,
            source_locations,
            symbols: SymbolTable(symbol_map),
            notes,
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

fn filter_locations_in_text(locations: &mut Vec<SourceLocationInfo>, address_map: &AddressMap) {
    locations.sort_unstable_by_key(|loc| loc.address);

    let mut done_idx = 0;
    for (&start_addr, &header) in address_map.0.iter() {
        // Remove all entries that are in between done and the start address.
        let start_idx = find_first_idx(&locations[done_idx..], start_addr) + done_idx;
        locations.drain(done_idx..start_idx);

        // The end address is one past the last byte of the section.
        let end_addr = start_addr + header.p_memsz as u32;
        done_idx += find_first_idx(&locations[done_idx..], end_addr);
    }
}

fn find_first_idx(slice: &[SourceLocationInfo], addr: u32) -> usize {
    match slice.binary_search_by_key(&addr, |loc| loc.address) {
        Ok(mut idx) => {
            while idx > 0 && slice[idx - 1].address == addr {
                idx -= 1;
            }
            idx
        }
        Err(idx) => idx,
    }
}

/// Index the symbols by their addresses.
#[derive(Default)]
pub struct SymbolTable(BTreeMap<(u32, u32), String>);

impl SymbolTable {
    pub fn new(elf: &Elf) -> SymbolTable {
        let mut symbols = elf
            .syms
            .iter()
            .filter_map(|sym| {
                // We only care about global symbols that have string names, and are
                // either functions or variables.
                if sym.st_name != 0 && (sym.st_type() == STT_OBJECT || sym.st_type() == STT_FUNC) {
                    Some((elf.strtab[sym.st_name].to_owned(), sym.st_value as u32))
                } else {
                    None
                }
            })
            .collect();

        dedup_names(&mut symbols);

        let mut disambiguator = 0..;
        SymbolTable(
            symbols
                .into_iter()
                .map(|(name, addr)| ((addr, disambiguator.next().unwrap()), name.to_string()))
                .collect(),
        )
    }

    /// Returns an iterator over all symbols of a given address.
    fn all_iter(&self, addr: u32) -> impl Iterator<Item = &str> {
        self.0
            .range((addr, 0)..=(addr, u32::MAX))
            .map(|(_, name)| name.as_ref())
    }

    fn default_label(addr: u32) -> Cow<'static, str> {
        Cow::Owned(format!("L{addr:08x}"))
    }

    /// Get a symbol, if the address has one.
    pub fn try_get_one(&self, addr: u32) -> Option<&str> {
        self.all_iter(addr).next()
    }

    /// Get a symbol, or a default label formed from the address value.
    pub fn get_one(&self, addr: u32) -> Cow<str> {
        match self.try_get_one(addr) {
            Some(s) => Cow::Borrowed(s),
            None => Self::default_label(addr),
        }
    }

    /// Get all symbol, or a default label formed from the address value.
    pub fn get_all(&self, addr: u32) -> impl Iterator<Item = Cow<str>> {
        let mut iter = self.all_iter(addr).peekable();
        let default = if iter.peek().is_none() {
            Some(Self::default_label(addr))
        } else {
            None
        };
        iter.map(Cow::Borrowed).chain(default)
    }
}

/// Deduplicates by removing identical entries and appending the address to
/// repeated names. The vector ends up sorted.
fn dedup_names(symbols: &mut Vec<(String, u32)>) {
    while dedup_names_pass(symbols) {}
}

/// Deduplicates the names of the symbols by appending one level of address to
/// the name.
///
/// Returns `true` if the names were deduplicated.
fn dedup_names_pass(symbols: &mut Vec<(String, u32)>) -> bool {
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
                *name = format!("{name}_{address:08x}");
            } else {
                next_group = Some((name, *address));
                break;
            }
        }

        // If there were duplicates in the group, update the group leader, too.
        if group_deduplicated {
            *group_name = format!("{group_name}_{group_address:08x}");
        }
    }

    deduplicated
}

#[cfg(test)]
mod tests {
    #[test]
    fn single_pass_dedup_names() {
        let mut symbols = vec![
            ("baz".to_string(), 0x8000),
            ("bar".to_string(), 0x3000),
            ("foo".to_string(), 0x1000),
            ("bar".to_string(), 0x5000),
            ("foo".to_string(), 0x2000),
            ("baz".to_string(), 0x7000),
            ("baz".to_string(), 0x9000),
            ("doo".to_string(), 0x0042),
            ("baz".to_string(), 0xa000),
            ("baz".to_string(), 0x6000),
            ("bar".to_string(), 0x4000),
        ];

        super::dedup_names(&mut symbols);

        let expected = vec![
            ("bar_00003000".to_string(), 0x3000),
            ("bar_00004000".to_string(), 0x4000),
            ("bar_00005000".to_string(), 0x5000),
            ("baz_00006000".to_string(), 0x6000),
            ("baz_00007000".to_string(), 0x7000),
            ("baz_00008000".to_string(), 0x8000),
            ("baz_00009000".to_string(), 0x9000),
            ("baz_0000a000".to_string(), 0xa000),
            ("doo".to_string(), 0x0042),
            ("foo_00001000".to_string(), 0x1000),
            ("foo_00002000".to_string(), 0x2000),
        ];
        assert_eq!(symbols, expected);
    }

    #[test]
    fn multi_pass_dedup_names() {
        let mut symbols = vec![
            ("john".to_string(), 0x42),
            ("john".to_string(), 0x87),
            ("john".to_string(), 0x1aa),
            ("john_000001aa".to_string(), 0x1aa),
            ("john_00000042".to_string(), 0x103),
            ("john_00000087".to_string(), 0x103),
        ];

        super::dedup_names(&mut symbols);

        let expected = vec![
            ("john_00000042_00000042".to_string(), 0x42),
            ("john_00000042_00000103".to_string(), 0x103),
            ("john_00000087_00000087".to_string(), 0x87),
            ("john_00000087_00000103".to_string(), 0x103),
            ("john_000001aa".to_string(), 0x1aa),
        ];

        assert_eq!(symbols, expected);
    }
}
