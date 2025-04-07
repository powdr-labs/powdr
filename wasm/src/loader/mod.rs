mod allocate_locals;

use std::{
    collections::{btree_map::Entry, BTreeMap},
    str::FromStr,
};

use powdr_syscalls::Syscall;
use wasmparser::{
    BlockType, CompositeInnerType, ElementItems, FuncType, MemoryType, Operator, OperatorsReader,
    Parser, Payload, RefType, SubType, TableInit, TypeRef, ValType,
};

use allocate_locals::{AllocatedVar, Directive};

/// WASM defined page size is 64 KiB.
const PAGE_SIZE: u32 = 65536;

/// What size we reserve for the stack, in bytes.
const STACK_SIZE: u32 = 1024 * 1024 * 1024; // 1 GiB

/// If the table has no specified maximum size, we assign it a large default, in number of entries.
const DEFAULT_MAX_TABLE_SIZE: u32 = 4096;

/// Segment is not a WASM concept, but it is used to mean a region of memory
/// that is allocated for a WASM table or memory.
#[derive(Clone, Copy)]
pub struct Segment {
    /// The start address of the segment, in bytes.
    pub start: u32,
    /// The size of the segment, in bytes.
    pub size: u32,
}

#[derive(Clone, Copy)]
pub enum MemoryEntry {
    /// Actual value stored in memory word.
    Value(u32),
    /// Refers to a code label.
    Label(u32),
}

/// Helper struct to track unallocated memory.
/// This is used to allocate the memory for the tables and the globals.
struct MemoryAllocator {
    /// The address of the next free memory, in bytes.
    next_free: u32,
}

impl MemoryAllocator {
    fn new() -> Self {
        MemoryAllocator { next_free: 0 }
    }

    fn allocate_var(&mut self, val_type: ValType) -> AllocatedVar {
        let var = AllocatedVar {
            val_type,
            address: self.next_free,
        };
        self.next_free = self.next_free.checked_add(sz(val_type)).unwrap();
        var
    }

    fn allocate_segment(&mut self, size: u32) -> Segment {
        assert!(size % 4 == 0);
        let segment = Segment {
            start: self.next_free,
            size,
        };
        self.next_free = self.next_free.checked_add(size).unwrap();
        segment
    }

    fn remaining_space(&self) -> u32 {
        // The maximum size of the memory is 4 GiB, so we can use 32 bits to represent it.
        u32::MAX - self.next_free
    }
}

struct InitialMemory(BTreeMap<u32, MemoryEntry>);

impl InitialMemory {
    fn new() -> Self {
        InitialMemory(BTreeMap::new())
    }

    fn insert(&mut self, address: u32, entry: MemoryEntry) {
        if matches!(entry, MemoryEntry::Value(0)) {
            // We don't need to store 0 values, as they are the default.
            return;
        }
        self.0.insert(address, entry);
    }

    fn get(&self, address: u32) -> MemoryEntry {
        *self.0.get(&address).unwrap_or(&MemoryEntry::Value(0))
    }

    /// Insert up to 4 bytes of data at the given address.
    ///
    /// If there is an existing value at the address, the replaced bytes must be 0.
    fn insert_bytes(&mut self, address: u32, start_byte: u32, value: &[u8]) {
        let mut word = 0;
        let mut mask = 0;
        for (i, byte) in value.iter().take(4).enumerate() {
            let bit_offset = (start_byte + i as u32) * 8;
            word |= (*byte as u32) << bit_offset;
            mask |= 0xFF << bit_offset;
        }

        word <<= start_byte * 8;
        mask <<= start_byte * 8;

        match self.0.entry(address) {
            Entry::Vacant(entry) => {
                if word != 0 {
                    // We don't need to store 0 values, as they are the default.
                    entry.insert(MemoryEntry::Value(word));
                }
            }
            Entry::Occupied(mut entry) => {
                let MemoryEntry::Value(old_value) = entry.get() else {
                    panic!("Memory entry is not a value");
                };
                assert!(old_value & mask == 0);
                let new_value = old_value | word;
                if new_value == 0 {
                    entry.remove();
                } else {
                    entry.insert(MemoryEntry::Value(new_value));
                }
            }
        }
    }
}

pub struct Program<'a> {
    /// The functions defined in the module.
    pub functions: Vec<Vec<Directive<'a>>>,
    /// The start function, if any.
    pub start_function: Option<u32>,
    /// The main function, if any.
    pub main_function: Option<u32>,
    /// The initial memory, with the values to be set at startup.
    pub initial_memory: BTreeMap<u32, MemoryEntry>,
    /// The globals, in order of definition.
    pub globals: Vec<AllocatedVar>,
    /// The memory segment, if any.
    pub memory: Option<Segment>,
    /// The tables, in order of definition.
    pub tables: Vec<Segment>,
    /// The special segments for the table initialization.
    pub elem_segments: Vec<Segment>,
    /// The special segments for the data initialization.
    pub data_segments: Vec<Segment>,
}

struct ModuleContext<'a> {
    types: Vec<SubType>,
    func_types: Vec<u32>,
    imported_functions: Vec<Syscall>,
    table_types: Vec<RefType>,
    p: Program<'a>,
}

impl ModuleContext<'_> {
    fn get_type(&self, type_idx: u32) -> &FuncType {
        let subtype = &self.types[type_idx as usize];
        match &subtype.composite_type.inner {
            CompositeInnerType::Func(f) => f,
            _ => panic!("gc proposal not supported"),
        }
    }

    fn get_func_type(&self, func_idx: u32) -> &FuncType {
        self.get_type(self.func_types[func_idx as usize])
    }

    fn blockty_inputs(&self, blockty: BlockType) -> &[ValType] {
        match blockty {
            BlockType::Empty | BlockType::Type(_) => &[],
            BlockType::FuncType(idx) => {
                let func_type = self.get_type(idx);
                func_type.params()
            }
        }
    }

    fn eval_const_expr(
        &self,
        val_type: ValType,
        expr: OperatorsReader,
    ) -> wasmparser::Result<Vec<MemoryEntry>> {
        let mut words = Vec::new();
        let mut iter = expr.into_iter();

        let op = iter.next().unwrap()?;
        match op {
            Operator::I32Const { value } => {
                assert_eq!(val_type, ValType::I32);
                words.push(MemoryEntry::Value(value as u32))
            }
            Operator::I64Const { value } => {
                assert_eq!(val_type, ValType::I64);
                words.push(MemoryEntry::Value(value as u32));
                words.push(MemoryEntry::Value((value >> 32) as u32));
            }
            Operator::F32Const { value } => {
                assert_eq!(val_type, ValType::F32);
                words.push(MemoryEntry::Value(value.bits()));
            }
            Operator::RefFunc { function_index } => {
                assert_eq!(val_type, ValType::Ref(RefType::FUNCREF));
                // The first 32 bits are the function type index
                words.push(MemoryEntry::Value(self.func_types[function_index as usize]));
                // The second 32 bits are the function address in code space
                words.push(MemoryEntry::Label(function_index));
            }
            Operator::RefNull { .. } => {
                assert!(matches!(val_type, ValType::Ref(_)));
                // Since (0, 0) is a valid function reference, lets use u32::MAX to represent null.
                words.push(MemoryEntry::Value(u32::MAX));
                words.push(MemoryEntry::Value(u32::MAX));
            }
            op => panic!("Unsupported operator in const expr: {op:?}"),
        }

        let end_op = iter.next().unwrap()?;
        assert_eq!(end_op, Operator::End);
        assert!(iter.next().is_none());

        Ok(words)
    }

    /// Returns the memory segment information, allocating if needed.
    fn get_memory(
        &mut self,
        mem_allocator: &mut MemoryAllocator,
        initial_memory: &mut InitialMemory,
        mem_type: &Option<MemoryType>,
    ) -> Option<Segment> {
        let Some(mem_type) = mem_type else {
            return None;
        };

        if self.p.memory.is_none() {
            let maximum_size = mem_allocator
                .remaining_space()
                // From all the memory available, we reserve the space for the stack, and the 8 bytes needed
                // to store the size of the memory and its maximum size:
                .saturating_sub(STACK_SIZE + 8)
                / PAGE_SIZE;

            if maximum_size < mem_type.initial as u32 {
                panic!("Not enough address space available to allocate the initial memory plus the stack");
            }

            let maximum_size = mem_type
                .maximum
                .map_or(maximum_size, |v| maximum_size.min(v as u32));

            let segment = mem_allocator.allocate_segment(maximum_size * PAGE_SIZE + 8);
            initial_memory.insert(segment.start, MemoryEntry::Value(mem_type.initial as u32));
            initial_memory.insert(segment.start + 4, MemoryEntry::Value(maximum_size));

            self.p.memory = Some(segment);
        }

        self.p.memory
    }
}

/// Type size, in bytes
const fn sz(val_type: ValType) -> u32 {
    match val_type {
        ValType::I32 => 4,
        ValType::I64 => 8,
        ValType::F32 => 4,
        ValType::F64 => 8,
        ValType::V128 => 16,
        // Function references are 64 bits because the first 32 bits are
        // the function type index, and the other 32 bits are the function
        // address in code space.
        //
        // For extern references (that we don't provide any means to instantiate),
        // I am very tempted to use 0 bytes, but in the spirit that it might be
        // useful in the future, I will use 8 bytes, so that it has the same size
        // as a function reference.
        ValType::Ref(_) => 8,
    }
}

/// Size of many types, in bytes
fn many_sz(val_types: &[ValType]) -> u32 {
    val_types.iter().map(|&ty| sz(ty)).sum()
}

/// A function definition that just calls the syscall. This is useful
/// in case there is an indirect call to a system call.
fn proxy_syscall(label: u32, syscall: Syscall, ty: &FuncType) -> Vec<Directive<'static>> {
    fn alloc_vars(types: &[ValType]) -> (Vec<AllocatedVar>, u32) {
        let mut address = 0;
        let mut vars = Vec::new();
        for ty in types {
            let var = AllocatedVar {
                val_type: *ty,
                address,
            };
            address += sz(*ty);
            vars.push(var);
        }

        (vars, address)
    }

    let (inputs, inputs_len) = alloc_vars(ty.params());
    let (outputs, outputs_len) = alloc_vars(ty.results());

    let mut directives = Vec::new();
    directives.push(Directive::Label(label));

    directives.push(Directive::Syscall {
        syscall,
        inputs,
        outputs,
    });

    let return_info = AllocatedVar {
        val_type: ValType::I64,
        address: inputs_len.max(outputs_len),
    };

    directives.push(Directive::Return { return_info });

    directives
}

/// Arranges the bytes in little-endian words.
///
/// Alignment mod 4 is the byte offset of the first word.
fn pack_bytes_into_words(bytes: &[u8], mut alignment: u32) -> Vec<MemoryEntry> {
    let mut words = Vec::new();
    let mut value = 0;
    alignment %= 4;
    for byte in bytes.iter() {
        value |= (*byte as u32) << (alignment * 8);
        if alignment == 3 {
            words.push(MemoryEntry::Value(value));
            value = 0;
            alignment = 0;
        } else {
            alignment += 1;
        }
    }
    if bytes.len() % 4 != 0 {
        words.push(MemoryEntry::Value(value));
    }
    words
}

pub fn load_wasm(wasm_file: &[u8]) -> wasmparser::Result<Program> {
    let parser = Parser::new(0);

    let mut ctx = ModuleContext {
        types: Vec::new(),
        func_types: Vec::new(),
        imported_functions: Vec::new(),
        table_types: Vec::new(),
        p: Program {
            functions: Vec::new(),
            start_function: None,
            main_function: None,
            // This is actually left empty, and will be filled just before returning.
            initial_memory: BTreeMap::new(),
            globals: Vec::new(),
            memory: None,
            tables: Vec::new(),
            elem_segments: Vec::new(),
            data_segments: Vec::new(),
        },
    };

    // This is the memory layout of the program after all the elements have been allocated:
    // - all tables, in sequence, where each table contains:
    //   - the first word is the table size, in number of elements
    //   - the second word is the maximum size, in number of elements
    //   - then a sequence of entries of 2 words (references).
    // - all globals, in sequence
    // - all passive element segments, in sequence, where each segment is:
    //   - the first word is the segment size, in number of elements (this size is fixed)
    //   - a sequence of entries of 2 words (references).
    // - all passive data segments, in sequence, where each segment is:
    //   - the first word is the segment size, in bytes (this size is mostly fixed, but can be set to 0 on data.drop)
    //   - a sequence N words, where N is the minimum number of words to fit all bytes. Last word may be 0-padded.
    // - the WASM memory instance, where:
    //   - the first word is the size of the memory, in pages of 64 KiB
    //   - the second word is the maximum size of the memory, in pages of 64 KiB
    //   - then the memory byte array
    // - the WASM stack, that can grow to the maximum size of the memory
    let mut mem_type = None;
    let mut mem_allocator = MemoryAllocator::new();

    let mut initial_memory = InitialMemory::new();

    let mut internal_labels = None;

    // TODO: validate while parsing

    // The payloads are processed in the order they appear in the file, so each variable written
    // in one step is available in the next steps.
    let mut unsupported_feature_found = false;
    for payload in parser.parse_all(wasm_file) {
        match payload? {
            Payload::Version { num, .. } => {
                // This is the encoding version, we don't care about it.
                log::debug!("Binary encoding version: {num}");
            }
            Payload::TypeSection(section) => {
                log::debug!("Type Section found");
                for rec_group in section {
                    let mut iter = rec_group?.into_types();
                    let ty = match (iter.next(), iter.next()) {
                        (Some(subtype), None) => match &subtype.composite_type.inner {
                            CompositeInnerType::Func(_) => subtype,
                            _ => {
                                unsupported_feature_found = true;
                                log::error!("unsupported types from GC proposal found");
                                continue;
                            }
                        },
                        _ => {
                            // Apparently WebAssembly 3.0 is much more complicated, and has complex
                            // type definitions, and garbage collector, and exceptions. We should probably
                            // stick to the 2.0 version for Powdr.
                            unsupported_feature_found = true;
                            log::error!("unsupported types from GC proposal found");
                            continue;
                        }
                    };
                    let type_idx = ctx.types.len() as u32;
                    ctx.types.push(ty);
                    log::debug!("Type read: {:?}", ctx.get_type(type_idx));
                }
            }
            Payload::ImportSection(section) => {
                log::debug!("Import Section found");
                // TODO: we could implement module load and cross module dependencies,
                // but this is not a very used feature in WASM and modules are usually
                // self-contained.
                //
                // For now, the imports only deal with powdr provided functions.
                for import in section {
                    let import = import?;
                    if import.module == "powdr" {
                        panic!("Only \"powdr\" module is available for imports");
                    }
                    if let TypeRef::Func(type_idx) = import.ty {
                        // Lets see if the name is known
                        if let Ok(syscall) = Syscall::from_str(import.name) {
                            // Lets see if the type matches the expectations.
                            let ty = ctx.get_type(type_idx);
                            assert!(ty.params().iter().all(|&ty| sz(ty) == 4), "Syscall parameters must be 32-bit, but was imported with a type of {ty:?}");
                            assert!(ty.results().iter().all(|&ty| sz(ty) == 4), "Syscall results must be 32-bit, but was imported with a type of {ty:?}");

                            let given_arity = (ty.params().len() as u32, ty.results().len() as u32);

                            let expected_arity = syscall.arity();
                            if given_arity != expected_arity {
                                panic!(
                                    "Syscall \"{}\" expects {} 32-bit inputs and {} 32-bit outputs, but was imported with {} 32-bit inputs and {} 32-bit outputs",
                                    import.name, expected_arity.0, expected_arity.1, given_arity.0, given_arity.1
                                );
                            }

                            log::debug!("Imported syscall: {}", import.name);

                            // Each function uses as label its own index.
                            let label = ctx.func_types.len() as u32;

                            // Adds a proxy function that just calls the system call
                            ctx.p.functions.push(proxy_syscall(label, syscall, ty));

                            ctx.func_types.push(type_idx);
                            ctx.imported_functions.push(syscall);

                            continue;
                        }
                    }
                    panic!(
                        "Tried to import unknown entity \"{}.{}\"",
                        import.module, import.name
                    );
                }
            }
            Payload::FunctionSection(section) => {
                log::debug!("Function Section found");
                for type_idx in section {
                    let type_idx = type_idx?;
                    let func_idx = ctx.func_types.len() as u32;
                    ctx.func_types.push(type_idx);
                    log::debug!(
                        "Type of function {func_idx}: {type_idx} ({:?})",
                        ctx.get_type(type_idx)
                    );
                }
            }
            Payload::TableSection(section) => {
                log::debug!("Table Section found");
                for table in section {
                    let table = table?;
                    if (!table.ty.element_type.is_extern_ref()
                        && !table.ty.element_type.is_func_ref())
                        || table.ty.table64
                        || table.ty.shared
                    {
                        unsupported_feature_found = true;
                        log::error!("Found table with unsupported properties",);
                        continue;
                    }

                    if !matches!(table.init, TableInit::RefNull) {
                        unsupported_feature_found = true;
                        log::error!("Table initialization is not supported");
                        continue;
                    }

                    log::debug!(
                        "Table defined. Initial size: {}, maximum size: {:?}",
                        table.ty.initial,
                        table.ty.maximum
                    );

                    let max_entries = table
                        .ty
                        .maximum
                        .map(|v| v as u32)
                        .unwrap_or(DEFAULT_MAX_TABLE_SIZE);

                    // We include two extra words for the table size and maximum size
                    let segment = mem_allocator.allocate_segment(max_entries * 8 + 8);

                    // Store the table size and maximum size in the initial memory
                    initial_memory
                        .insert(segment.start, MemoryEntry::Value(table.ty.initial as u32));
                    initial_memory.insert(segment.start + 4, MemoryEntry::Value(max_entries));

                    ctx.p.tables.push(segment);
                    ctx.table_types.push(table.ty.element_type);
                }
            }
            Payload::MemorySection(section) => {
                log::debug!("Memory Section found");
                for mem in section {
                    let mem = mem?;

                    if ctx.p.memory.is_some() {
                        unsupported_feature_found = true;
                        log::error!("Multiple memories are not supported");
                        break;
                    }

                    if mem.memory64 || mem.shared || mem.page_size_log2.is_some() {
                        unsupported_feature_found = true;
                        log::error!("Found memory with unsupported properties");
                        continue;
                    }

                    log::debug!(
                        "Memory defined. Initial size: {} pages, maximum size: {:?} pages",
                        mem.initial,
                        mem.maximum
                    );

                    // Lets delay the actual memory allocation to after the globals are allocated.
                    mem_type = Some(mem);
                }
            }
            Payload::GlobalSection(section) => {
                log::debug!("Global Section found");
                for global in section {
                    let global = global?;
                    let ty = global.ty.content_type;
                    let var = mem_allocator.allocate_var(ty);

                    log::debug!("Global variable {} has type {:?}", ctx.p.globals.len(), ty);

                    // Initialize the global variables
                    let words = ctx.eval_const_expr(ty, global.init_expr.get_operators_reader())?;
                    for (idx, word) in words.into_iter().enumerate() {
                        initial_memory.insert(var.address + 4 * idx as u32, word);
                    }

                    ctx.p.globals.push(var);
                }
            }
            Payload::ExportSection(section_limited) => {
                log::debug!("Export Section found");
                for export in section_limited {
                    let export = export?;
                    log::debug!(
                        "Exported entity: {}, kind {:?}, index: {}",
                        export.name,
                        export.kind,
                        export.index
                    );

                    // Following the convention of the Rust target wasm32-unknown-unknown,
                    // we expect one "main" function, that is the entry point of the program.
                    if export.name == "main" {
                        ctx.p.main_function = Some(export.index);
                    }
                }
            }
            Payload::StartSection { func, .. } => {
                log::debug!("Start Section found. Start function: {func}");
                ctx.p.start_function = Some(func);
            }
            Payload::ElementSection(section) => {
                log::debug!("Element Section found");
                for elem_segment in section {
                    let elem_segment = elem_segment?;

                    // Get all the values in the segment
                    let mut values = Vec::new();
                    match elem_segment.items {
                        ElementItems::Functions(section) => {
                            for idx in section {
                                let idx = idx?;
                                values.push(MemoryEntry::Value(ctx.func_types[idx as usize]));
                                values.push(MemoryEntry::Label(idx));
                            }
                        }
                        ElementItems::Expressions(ref_type, section) => {
                            for elem in section {
                                let val = ctx.eval_const_expr(
                                    ValType::Ref(ref_type),
                                    elem?.get_operators_reader(),
                                )?;
                                values.extend(val);
                            }
                        }
                    };
                    let num_elems = values.len() as u32 / 2;

                    // Decide what to do with the values
                    match elem_segment.kind {
                        wasmparser::ElementKind::Passive => {
                            // This is stored as a memory segment to be used by table.init instruction

                            log::debug!(
                                "Passive table segment found. Number of elements: {num_elems}"
                            );

                            // We include one extra word for the segment size
                            let segment = mem_allocator.allocate_segment(num_elems * 8 + 4);

                            // Store the segment size in the initial memory
                            initial_memory.insert(segment.start, MemoryEntry::Value(num_elems));

                            // Store the values in the initial memory
                            for (idx, word) in values.into_iter().enumerate() {
                                initial_memory.insert(segment.start + 4 * (idx as u32 + 1), word);
                            }

                            // Store the segment in the context
                            ctx.p.elem_segments.push(segment);
                        }
                        wasmparser::ElementKind::Active {
                            table_index,
                            offset_expr,
                        } => {
                            // This is used to statically initialize the table. We can set the values on the table directly.

                            // I am assuming the table index of 0 if not provided, as hinted by the WASM binary spec.
                            let idx = table_index.unwrap_or(0);
                            let table = &ctx.p.tables[idx as usize];

                            let &[MemoryEntry::Value(offset)] = ctx
                                .eval_const_expr(ValType::I32, offset_expr.get_operators_reader())?
                                .as_slice()
                            else {
                                panic!("Offset is not a u32 value");
                            };

                            log::debug!("Active table segment found. Table index: {idx}, offset: {offset}, number of elements: {num_elems}");

                            let MemoryEntry::Value(table_size) = initial_memory.get(table.start)
                            else {
                                panic!("Table size is a label");
                            };
                            assert!(offset + num_elems <= table_size);

                            let mut byte_offset = table.start + offset * 8 + 8;
                            for value in values {
                                initial_memory.insert(byte_offset, value);
                                byte_offset += 4;
                            }
                            assert!(byte_offset <= table.start + table.size);
                        }
                        wasmparser::ElementKind::Declared => {
                            // Declarative elements are informational, we don't need to do anything
                        }
                    }
                }
            }
            Payload::DataCountSection { count, .. } => {
                log::debug!("Data Count Section found. Count: {count}");
                // This is used only by the static validator.
            }
            Payload::CodeSectionStart { count, .. } => {
                log::debug!("Code Section Start found. Count: {count}");
                assert_eq!(ctx.p.functions.len() + count as usize, ctx.func_types.len());

                // The labels used internally in the functions must be globally unique.
                // This is the label generator, starting from label available after the
                // function labels.
                internal_labels = Some((ctx.func_types.len() as u32)..);
            }
            Payload::CodeSectionEntry(function) => {
                log::debug!("Code Section Entry found");
                // By the time we get here, the ctx will be complete,
                // because all previous sections have been processed.

                let definition = allocate_locals::infinite_registers_allocation(
                    &ctx,
                    ctx.p.functions.len() as u32,
                    internal_labels.as_mut().unwrap(),
                    function,
                )?;
                ctx.p.functions.push(definition);
            }
            Payload::DataSection(section) => {
                log::debug!("Data Section found");
                for data_segment in section {
                    let data_segment = data_segment?;

                    match data_segment.kind {
                        wasmparser::DataKind::Passive => {
                            // This is stored as a memory segment to be used by memory.init instruction

                            // We include one extra word for the segment size
                            let byte_count = data_segment.data.len() as u32;
                            let segment = mem_allocator.allocate_segment(byte_count + 4);

                            // Store the segment size in the initial memory
                            initial_memory.insert(segment.start, MemoryEntry::Value(byte_count));

                            // Store the values in the initial memory
                            let values = pack_bytes_into_words(data_segment.data, 0);
                            for (idx, word) in values.into_iter().enumerate() {
                                initial_memory.insert(segment.start + 4 * (idx as u32 + 1), word);
                            }

                            // Store the segment in the context
                            ctx.p.data_segments.push(segment);
                        }
                        wasmparser::DataKind::Active {
                            memory_index,
                            offset_expr,
                        } => {
                            // This is used to statically initialize the memory. We can set the values on the memory directly.

                            if memory_index != 0 {
                                unsupported_feature_found = true;
                                log::error!("Found data segment with memory index other than 0");
                                continue;
                            }

                            let Some(memory) =
                                ctx.get_memory(&mut mem_allocator, &mut initial_memory, &mem_type)
                            else {
                                if !data_segment.data.is_empty() {
                                    unreachable!("Data segment but no memory defined");
                                }
                                continue;
                            };

                            let &[MemoryEntry::Value(offset)] = ctx
                                .eval_const_expr(ValType::I32, offset_expr.get_operators_reader())?
                                .as_slice()
                            else {
                                panic!("Offset is not a u32 value");
                            };

                            let MemoryEntry::Value(mem_size) = initial_memory.get(memory.start)
                            else {
                                panic!("Memory size is a label");
                            };
                            let mem_size = mem_size * PAGE_SIZE;
                            assert!(offset + data_segment.data.len() as u32 <= mem_size);

                            let mut byte_offset = memory.start + 8 + offset;
                            let mut data = data_segment.data;

                            // If misaligned, handle the first word separately.
                            let alignment = byte_offset % 4;
                            if alignment != 0 {
                                byte_offset -= alignment;

                                let first_word;
                                (first_word, data) = data.split_at(4 - alignment as usize);
                                initial_memory.insert_bytes(byte_offset, alignment, first_word);
                                byte_offset += 4;
                            }

                            // Split the last word to be handled separately, if not a full word.
                            let last_word_len = data.len() % 4;
                            let last_word;
                            (data, last_word) = data.split_at(data.len() - last_word_len);

                            // General case, for the word aligned middle:
                            let values = pack_bytes_into_words(data, 0).into_iter();
                            for word in values {
                                initial_memory.insert(byte_offset, word);
                                byte_offset += 4;
                            }

                            // Insert the misaligned last word, if any.
                            if !last_word.is_empty() {
                                initial_memory.insert_bytes(byte_offset, 0, last_word);
                            }
                        }
                    }
                }
            }
            Payload::CustomSection(_) => {
                // TODO: read function names and debug information
            }
            Payload::End(_) => {
                log::debug!("End of the module");
            }
            unsupported_section => {
                unsupported_feature_found = true;
                log::error!("Unsupported section found: {unsupported_section:?}");
            }
        }
    }

    let _ = ctx.get_memory(&mut mem_allocator, &mut initial_memory, &mem_type);

    assert!(
        !unsupported_feature_found,
        "Only WebAssembly Release 2.0 is supported"
    );

    ctx.p.initial_memory = initial_memory.0;

    Ok(ctx.p)
}
