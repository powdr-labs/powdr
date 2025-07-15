use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap},
    path::Path,
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
    /// Maps addresses to symbol names.
    pub symbols: SymbolTable,
    /// Human readable notes about an address
    pub notes: HashMap<u32, String>,
}

#[derive(Debug)]
pub struct SourceLocationInfo {
    pub address: u32,
    pub file: u64,
    pub line: u64,
    pub col: u64,
}

impl DebugInfo {
    /// Extracts debug information from the ELF file, if available.
    pub fn new(
        elf: &Elf,
        file_buffer: &[u8],
        address_map: &AddressMap,
        is_data_addr: &dyn Fn(u32) -> bool,
        jump_targets: &BTreeSet<u32>,
    ) -> Result<Self, Error> {
        let dwarf = load_dwarf_sections(elf, file_buffer)?;

        let mut file_list = Vec::new();
        let mut source_locations = Vec::new();
        let mut notes = HashMap::new();

        // Read the ELF symbol table, to be joined with symbols from the DWARF.
        let mut symbols = read_symbol_table(elf);

        // Iterate over the compilation units:
        let mut units_iter = dwarf.units();
        while let Some(unit) = units_iter.next()? {
            let unit = dwarf.unit(unit)?;
            // Shadows the Unit with a reference to itself, because it is more
            // convenient to work with a UnitRef.
            let unit = UnitRef::new(&dwarf, &unit);

            // Read the source locations for this compilation unit.
            let file_index_delta =
                read_source_locations(unit, &mut file_list, &mut source_locations)?;

            read_unit_symbols(
                &dwarf,
                unit,
                file_index_delta,
                is_data_addr,
                jump_targets,
                &mut symbols,
                &mut notes,
            )?;
        }

        // Filter out the source locations that are not in the text section
        filter_locations_in_text(&mut source_locations, address_map);

        // Deduplicate the symbols
        dedup_names(&mut symbols);

        // Index by address, not by name.
        let symbols = SymbolTable(
            symbols
                .into_iter()
                .map(|(name, address)| (address, name))
                .into_group_map()
                .into_iter()
                .collect(),
        );

        Ok(DebugInfo {
            file_list,
            source_locations,
            symbols,
            notes,
        })
    }
}

/// Reads the source locations for a compilation unit.
fn read_source_locations(
    unit: UnitRef<Reader>,
    file_list: &mut Vec<(String, String)>,
    source_locations: &mut Vec<SourceLocationInfo>,
) -> Result<u64, gimli::Error> {
    // Traverse all the line locations for the compilation unit.
    let base_dir = Path::new(
        unit.comp_dir
            .map(|s| s.to_string())
            .transpose()?
            .unwrap_or(""),
    );
    let file_idx_delta = file_list.len() as u64;
    if let Some(line_program) = unit.line_program.clone() {
        // Get the source file listing
        for file_entry in line_program.header().file_names() {
            let directory = file_entry
                .directory(line_program.header())
                .map(|attr| as_str(unit, attr))
                .transpose()?
                .unwrap_or("");

            // This unwrap can not panic because both base_dir and
            // directory have been validated as UTF-8 strings.
            let directory = base_dir
                .join(directory)
                .into_os_string()
                .into_string()
                .unwrap();

            let path = as_str(unit, file_entry.path_name())?;

            file_list.push((directory, path.to_owned()));
        }

        // Get the locations indexed by address
        let mut rows = line_program.rows();
        while let Some((_, row)) = rows.next_row()? {
            // End markers point to the address after the end, so we skip them.
            if row.prologue_end() || row.end_sequence() {
                continue;
            }

            source_locations.push(SourceLocationInfo {
                address: row.address() as u32,
                file: row.file_index() + file_idx_delta,
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

    Ok(file_idx_delta)
}

/// Traverse the tree in which the information about the compilation
/// unit is stored and extract function and variable names.
fn read_unit_symbols(
    dwarf: &Dwarf<Reader>,
    unit: UnitRef<Reader>,
    file_idx_delta: u64,
    is_data_addr: &dyn Fn(u32) -> bool,
    jump_targets: &BTreeSet<u32>,
    symbols: &mut Vec<(String, u32)>,
    notes: &mut HashMap<u32, String>,
) -> Result<(), Error> {
    // To simplify the algorithm, we start the name stack with a placeholder value.
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
                let attr = find_attr(entry, gimli::DW_AT_linkage_name);
                let Some(linkage_name) = attr.map(|ln| unit.attr_string(ln)).transpose()? else {
                    // This function has no linkage name in DWARF, so it
                    // must be in ELFs symbol table.
                    continue;
                };

                let start_addresses = get_function_start(dwarf, &unit, entry)?;
                let name = linkage_name.to_string()?;
                for address in start_addresses {
                    if jump_targets.contains(&address) {
                        symbols.push((name.to_owned(), address));
                    }
                }
            }
            // This is the entry for a variable.
            gimli::DW_TAG_variable => {
                let Some(address) = get_static_var_address(&unit, entry)? else {
                    continue;
                };

                if !is_data_addr(address) {
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
                            file_line = Some((file_idx + file_idx_delta, line));
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
                    symbols.push((linkage_name.to_string()?.to_owned(), address));
                }
            }
            _ => {}
        };
    }

    Ok(())
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

/// This function linear searches for an attribute of an entry.
///
/// My first idea was to iterate over the attribute list once, matching for all
/// attributes I was interested in. But then I figured out this operation is
/// N*M, where N is the number of attributes in the list and M is the number of
/// attributes I am interested in. So doing the inverse is easier and has the
/// same complexity. Since it is hard to tell in practice which one is faster, I
/// went with the easier approach.
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

fn get_static_var_address(
    unit: &Unit<Reader>,
    entry: &DebuggingInformationEntry<Reader>,
) -> Result<Option<u32>, gimli::Error> {
    let Some(attr) = find_attr(entry, gimli::DW_AT_location) else {
        // No location available
        return Ok(None);
    };

    let AttributeValue::Exprloc(address) = attr else {
        // Not an static variable
        return Ok(None);
    };

    // Do the magic to find the variable address
    let mut ops = address.operations(unit.encoding());
    let first_op = ops.next()?;
    let second_op = ops.next()?;
    let (Some(Operation::Address { address }), None) = (first_op, second_op) else {
        // The address is not a constant
        return Ok(None);
    };

    Ok(Some(address as u32))
}

fn get_function_start(
    dwarf: &Dwarf<Reader>,
    unit: &Unit<Reader>,
    entry: &DebuggingInformationEntry<Reader>,
) -> Result<Vec<u32>, gimli::Error> {
    let mut ret = Vec::new();

    if let Some(low_pc) = find_attr(entry, gimli::DW_AT_low_pc)
        .map(|val| dwarf.attr_address(unit, val))
        .transpose()?
        .flatten()
    {
        ret.push(low_pc as u32);
    }

    if let Some(ranges) = find_attr(entry, gimli::DW_AT_ranges)
        .map(|val| dwarf.attr_ranges_offset(unit, val))
        .transpose()?
        .flatten()
    {
        let mut iter = dwarf.ranges(unit, ranges)?;
        while let Some(range) = iter.next()? {
            ret.push(range.begin as u32);
        }
    }

    Ok(ret)
}

/// Filter out source locations that are not in a text section.
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
pub struct SymbolTable(BTreeMap<u32, Vec<String>>);

impl SymbolTable {
    pub fn new(elf: &Elf) -> SymbolTable {
        let mut symbols = read_symbol_table(elf);

        dedup_names(&mut symbols);

        SymbolTable(
            symbols
                .into_iter()
                .map(|(name, addr)| (addr, name.to_string()))
                .into_group_map()
                .into_iter()
                .collect(),
        )
    }

    fn default_label(addr: u32) -> Cow<'static, str> {
        Cow::Owned(format!("__.L{addr:08x}"))
    }

    /// Get a symbol, if the address has one.
    pub fn try_get_one(&self, addr: u32) -> Option<&str> {
        self.0
            .get(&addr)
            .and_then(|v| v.first().map(|s| s.as_str()))
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
        static EMPTY: Vec<String> = Vec::new();
        let elems = self.0.get(&addr).unwrap_or(&EMPTY);
        let default = if elems.is_empty() {
            Some(Self::default_label(addr))
        } else {
            None
        };
        elems
            .iter()
            .map(|s| Cow::Borrowed(s.as_str()))
            .chain(default)
    }

    /// Returns a symbol at the address or at the first address before this one that has a symbol.
    /// Also returns the offset of the provided address relative to that symbol.
    pub fn try_get_one_or_preceding(&self, addr: u32) -> Option<(&str, u32)> {
        self.0
            .range(..=addr)
            .last()
            .and_then(|(a, v)| v.first().map(|s| (s.as_str(), addr - a)))
    }
}

fn read_symbol_table(elf: &Elf) -> Vec<(String, u32)> {
    elf.syms
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
        .collect()
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

    // The first different name defines a group, which ends on the next
    // different name. The whole group is deduplicated if it contains more than
    // one element.
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
    fn dedup_names() {
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
