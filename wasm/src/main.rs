use std::{
    collections::{btree_map::Entry, BTreeMap},
    ops::RangeFrom,
    str::FromStr,
};

use powdr_syscalls::Syscall;
use wasmparser::{
    BlockType, CompositeInnerType, ElementItems, FuncType, FunctionBody, LocalsReader, MemoryType,
    Operator, OperatorsIterator, OperatorsReader, Parser, Payload, RefType, SubType, TableInit,
    TypeRef, ValType,
};

/// WASM defined page size is 64 KiB.
const PAGE_SIZE: u32 = 65536;

/// What size we reserve for the stack.
const STACK_SIZE: u32 = 1024 * 1024 * 1024; // 1 GiB

/// If the table has no specified maximum size, we assign it a large default, in number of entries.
const DEFAULT_MAX_TABLE_SIZE: u32 = 4096;

/// Segment is not a WASM concept, but it is used to mean a region of memory
/// that is allocated for a WASM table or memory.
#[derive(Clone, Copy)]
struct Segment {
    /// The start address of the segment, in bytes.
    start: u32,
    /// The size of the segment, in bytes.
    size: u32,
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

struct ModuleContext {
    types: Vec<SubType>,
    func_types: Vec<u32>,
    imported_functions: Vec<Syscall>,
    tables: Vec<Segment>,
    table_types: Vec<RefType>,
    memory: Option<Segment>,
    globals: Vec<AllocatedVar>,
    elem_segments: Vec<Segment>,
    data_segments: Vec<Segment>,
}

impl ModuleContext {
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
        for op in expr {
            match op? {
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
                _ => panic!("Unsupported operator in const expr"),
            }
        }
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
            self.memory?;
            unreachable!();
        };

        if self.memory.is_none() {
            let maximum_size = mem_allocator.remaining_space().saturating_sub(STACK_SIZE);

            if maximum_size < mem_type.initial as u32 {
                panic!("Not enough address space available to allocate the initial memory plus the stack");
            }

            let maximum_size =
                maximum_size.min(mem_type.maximum.map(|v| v as u32).unwrap_or(u32::MAX));

            let maximum_size_aligned = (maximum_size + 3) & !3;

            let segment = mem_allocator.allocate_segment(maximum_size_aligned);
            initial_memory.insert(segment.start, MemoryEntry::Value(mem_type.initial as u32));
            initial_memory.insert(segment.start + 4, MemoryEntry::Value(maximum_size));

            self.memory = Some(segment);
        }

        self.memory
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

    fn get(&self, address: u32) -> Option<&MemoryEntry> {
        self.0.get(&address)
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

fn main() -> wasmparser::Result<()> {
    // TODO: do proper command line argument parsing
    let args: Vec<String> = std::env::args().collect();
    let wasm_file = std::fs::read(&args[1]).unwrap();

    let parser = Parser::new(0);

    let mut ctx = ModuleContext {
        types: Vec::new(),
        func_types: Vec::new(),
        imported_functions: Vec::new(),
        tables: Vec::new(),
        table_types: Vec::new(),
        memory: None,
        globals: Vec::new(),
        elem_segments: Vec::new(),
        data_segments: Vec::new(),
    };

    let mut functions = Vec::new();

    let mut start_function = None;

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
    for payload in parser.parse_all(&wasm_file) {
        match payload? {
            Payload::Version {
                num,
                encoding,
                range,
            } => {
                // This is the encoding version, we don't care about it.
                log::debug!("WebAssembly version: {num}");
            }
            Payload::TypeSection(section) => {
                for rec_group in section {
                    let mut iter = rec_group?.into_types();
                    let ty = match (iter.next(), iter.next()) {
                        (Some(ty), None) => ty,
                        _ => {
                            // Apparently WebAssembly 3.0 is much more complicated, and has complex
                            // type definitions, and garbage collector, and exceptions. We should probably
                            // stick to the 2.0 version for Powdr.
                            unsupported_feature_found = true;
                            log::error!("unsupported types from GC proposal found");
                            continue;
                        }
                    };
                    ctx.types.push(ty);
                }
            }
            Payload::ImportSection(section) => {
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
                            functions.push(proxy_syscall(label, syscall, ty));

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
                for ty in section {
                    ctx.func_types.push(ty?);
                }
            }
            Payload::TableSection(section) => {
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

                    ctx.tables.push(segment);
                    ctx.table_types.push(table.ty.element_type);
                }
            }
            Payload::MemorySection(section) => {
                for mem in section {
                    let mem = mem?;

                    if ctx.memory.is_some() {
                        unsupported_feature_found = true;
                        log::error!("Multiple memories are not supported");
                        break;
                    }

                    if mem.memory64 || mem.shared || mem.page_size_log2.is_some() {
                        unsupported_feature_found = true;
                        log::error!("Found memory with unsupported properties");
                        continue;
                    }

                    // Lets delay the actual memory allocation to after the globals are allocated.
                    mem_type = Some(mem);
                }
            }
            Payload::GlobalSection(section) => {
                for global in section {
                    let global = global?;
                    let ty = global.ty.content_type;
                    let var = mem_allocator.allocate_var(ty);

                    // Initialize the global variables
                    let words = ctx.eval_const_expr(ty, global.init_expr.get_operators_reader())?;
                    for (idx, word) in words.into_iter().enumerate() {
                        initial_memory.insert(var.address + 4 * idx as u32, word);
                    }

                    ctx.globals.push(var);
                }
            }
            Payload::ExportSection(section_limited) => {
                // TODO: find out the entry point exported by Rust target wasm32-unknown-unknown.
                for export in section_limited {
                    let export = export?;
                    log::debug!(
                        "Exported entity: {}, kind {:?}, index: {}",
                        export.name,
                        export.kind,
                        export.index
                    );
                }
            }
            Payload::StartSection { func, .. } => start_function = Some(func),
            Payload::ElementSection(section) => {
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

                    // Decide what to do with the values
                    match elem_segment.kind {
                        wasmparser::ElementKind::Passive => {
                            // This is stored as a memory segment to be used by table.init instruction

                            // We include one extra word for the segment size
                            let num_elems = values.len() as u32;
                            let segment = mem_allocator.allocate_segment(num_elems * 8 + 4);

                            // Store the segment size in the initial memory
                            initial_memory.insert(segment.start, MemoryEntry::Value(num_elems));

                            // Store the values in the initial memory
                            for (idx, word) in values.into_iter().enumerate() {
                                initial_memory.insert(segment.start + 4 * (idx as u32 + 1), word);
                            }

                            // Store the segment in the context
                            ctx.elem_segments.push(segment);
                        }
                        wasmparser::ElementKind::Active {
                            table_index,
                            offset_expr,
                        } => {
                            // This is used to statically initialize the table. We can set the values on the table directly.

                            // I am assuming the table index of 0 if not provided, as hinted by the WASM binary spec.
                            let idx = table_index.unwrap_or(0);
                            let table = &ctx.tables[idx as usize];

                            let &[MemoryEntry::Value(offset)] = ctx
                                .eval_const_expr(ValType::I32, offset_expr.get_operators_reader())?
                                .as_slice()
                            else {
                                panic!("Offset is not a u32 value");
                            };

                            let Some(&MemoryEntry::Label(table_size)) =
                                initial_memory.get(table.start)
                            else {
                                panic!("Table size not found");
                            };
                            assert!(offset + values.len() as u32 <= table_size);

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
                // This is used only by the static validator.
            }
            Payload::CodeSectionStart { count, .. } => {
                assert_eq!(functions.len() + count as usize, ctx.func_types.len());

                // The labels used internally in the functions must be globally unique.
                // This is the label generator, starting from label available after the
                // function labels.
                internal_labels = Some((ctx.func_types.len() as u32)..);
            }
            Payload::CodeSectionEntry(function) => {
                // By the time we get here, the ctx will be complete,
                // because all previous sections have been processed.

                let definition = infinite_registers_allocation(
                    &ctx,
                    functions.len() as u32,
                    internal_labels.as_mut().unwrap(),
                    function,
                )?;
                functions.push(definition);
            }
            Payload::DataSection(section) => {
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
                            ctx.data_segments.push(segment);
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

                            let Some(&MemoryEntry::Label(mem_size)) =
                                initial_memory.get(memory.start)
                            else {
                                panic!("Memory size not found");
                            };
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
                            initial_memory.insert_bytes(byte_offset, 0, last_word);
                        }
                    }
                }
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

    Ok(())
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

enum MemoryEntry {
    /// Actual value stored in memory word.
    Value(u32),
    /// Refers to a code label.
    Label(u32),
}

#[derive(Clone, Copy)]
struct AllocatedVar {
    val_type: ValType,
    /// If it is a local or stack, this address is relative to the stack base.
    /// If it is a global, this address is absolute.
    address: u32,
}

enum Directive<'a> {
    WasmOp {
        op: Operator<'a>,
        inputs: Vec<AllocatedVar>,
        output: Option<AllocatedVar>,
    },
    Syscall {
        syscall: Syscall,
        inputs: Vec<AllocatedVar>,
        outputs: Vec<AllocatedVar>,
    },
    Label(u32),
    Call {
        function_index: u32,
        /// How much to increase the frame pointer for the called function.
        new_fp_delta: u32,
        /// Where to save the current frame pointer and return address,
        /// so that "return" can restore it.
        save_return_info_to: AllocatedVar,
    },
    Return {
        return_info: AllocatedVar,
    },
    /// Uncoditional jump to a label.
    /// Primitive used to implement "br".
    Jump {
        target: u32,
    },
    /// Jump to a label if the condition is zero.
    /// Primitive used to implement "if" and "br_if".
    JumpIfZero {
        target: u32,
        condition: AllocatedVar,
    },
    /// Minimum between an immediate u32 value and a parameter.
    /// Primitive used to implement "br_table".
    ImmMin {
        immediate: u32,
        input: AllocatedVar,
        output: AllocatedVar,
    },
    /// Skips the "multiple * count" of the next instructions.
    /// Primitive used to implement "br_table".
    SkipMultiple {
        multiple: u32,
        count: AllocatedVar,
    },
    /// Moves a 32-bit value from one relative address to another.
    /// The materialization of local.get, local.set and local.tee.
    MoveRel32 {
        src: u32,
        dest: u32,
    },
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

/// Allocates the locals and the stack at addresses starting
/// from 0, assuming one byte per address.
fn infinite_registers_allocation<'a>(
    module: &ModuleContext,
    func_idx: u32,
    labels: &mut RangeFrom<u32>,
    body: FunctionBody<'a>,
) -> wasmparser::Result<Vec<Directive<'a>>> {
    // Tracks the frame stack. Used to calculate arity.
    let (mut tracker, first_explicit_local) =
        StackTracker::new(module, func_idx, body.get_locals_reader()?)?;

    // The first directive is the entry point label of the function.
    let mut directives = vec![Directive::Label(func_idx)];

    // Zeroing of the explicit locals.
    directives.extend(
        (first_explicit_local..tracker.stack.base_bytes())
            .step_by(4)
            .map(|address| Directive::WasmOp {
                op: Operator::I32Const { value: 0 },
                inputs: Vec::new(),
                output: Some(AllocatedVar {
                    val_type: ValType::I32,
                    address,
                }),
            }),
    );

    // The rest of the directives are taken from the function body definition.
    let mut op_reader = body.get_operators_reader()?.into_iter();
    while let Some(operator) = op_reader.next() {
        // There shouldn't be any more operators after the outmost
        // block (the function itself) has ended.
        assert!(!tracker.control_stack.is_empty());

        // Match first the control operators, which require special handling.
        match operator? {
            Operator::Block { blockty } => {
                tracker.assert_block_args(blockty);
                tracker.control_stack.push(Frame {
                    stack_height: tracker.frame_height(blockty),
                    blockty,
                    frame_kind: FrameKind::Block {
                        target_label: labels.next().unwrap(),
                    },
                });
            }
            Operator::Loop { blockty } => {
                tracker.assert_block_args(blockty);
                let target_label = labels.next().unwrap();
                tracker.control_stack.push(Frame {
                    stack_height: tracker.frame_height(blockty),
                    blockty,
                    frame_kind: FrameKind::Loop { target_label },
                });
                directives.push(Directive::Label(target_label));
            }
            Operator::If { blockty } => {
                tracker.assert_block_args(blockty);
                let condition = tracker.stack.pop();
                assert!(condition.val_type == ValType::I32);

                // If condition is zero, it means false, so jump to the else block.
                let else_label = labels.next().unwrap();
                directives.push(Directive::JumpIfZero {
                    target: else_label,
                    condition,
                });

                tracker.control_stack.push(Frame {
                    stack_height: tracker.frame_height(blockty),
                    blockty,
                    frame_kind: FrameKind::If {
                        target_label: labels.next().unwrap(),
                        else_label,
                    },
                });
            }
            Operator::Else => {
                // Else happens at the same level as the corresponding if.
                let last_frame = tracker.control_stack.last_mut().unwrap();

                let FrameKind::If {
                    target_label,
                    else_label,
                } = last_frame.frame_kind
                else {
                    panic!("Else without If");
                };
                last_frame.frame_kind = FrameKind::Else { target_label };

                directives.push(Directive::Label(else_label));

                // Since we just parsed "If", the stack contains the outputs of the "If".
                // For the "Else", we must reset the stack to how it was before the "If",
                // so that the same arguments are available for the "Else" block.
                match last_frame.blockty {
                    BlockType::Empty => {
                        // There is nothing to do, input and output are equaly empty.
                    }
                    BlockType::Type(val_type) => {
                        // One output to drop, no inputs to include:
                        let if_output = tracker.stack.pop();
                        assert_eq!(if_output.val_type, val_type);
                    }
                    BlockType::FuncType(idx) => {
                        // Has both outputs to drop and inputs to include.
                        let ty = module.get_type(idx);
                        let results = ty.results();

                        // Assert the if output is what was expected and drop it.
                        tracker.assert_types_on_stack(results);
                        tracker.stack.drop_n_from_top(results.len());

                        // Push the inputs expected by the else.
                        let inputs = ty.params();
                        for &ty in inputs {
                            tracker.stack.push(ty);
                        }
                    }
                }
            }
            Operator::End => {
                let last_frame = tracker.control_stack.pop().unwrap();
                match last_frame.frame_kind {
                    FrameKind::If {
                        target_label,
                        else_label,
                    } => {
                        // The else is missing, so the else_label matches the target_label.
                        directives.push(Directive::Label(else_label));
                        directives.push(Directive::Label(target_label));
                    }
                    FrameKind::Block { target_label } | FrameKind::Else { target_label } => {
                        directives.push(Directive::Label(target_label));
                    }
                    FrameKind::Loop { .. } => {
                        // Do nothing, as the loop label was already emited.
                    }
                    FrameKind::Function => {
                        // The function has ended, we need to insert an explicit return.
                        directives.extend(tracker.return_code());
                    }
                }
            }
            Operator::Br { relative_depth } => {
                directives.extend(tracker.br_code(relative_depth));
                tracker.discard_unreachable_and_fix_the_stack(&mut op_reader)?;
            }
            Operator::BrIf { relative_depth } => {
                let br_not_taken = labels.next().unwrap();

                let condition = tracker.stack.pop();
                assert!(condition.val_type == ValType::I32);

                // Jump to after the br code if the condition is zero (false).
                directives.push(Directive::JumpIfZero {
                    target: br_not_taken,
                    condition,
                });

                directives.extend(tracker.br_code(relative_depth));
                directives.push(Directive::Label(br_not_taken));
            }
            Operator::BrTable { targets } => {
                let targets = targets
                    .targets()
                    .collect::<wasmparser::Result<Vec<u32>>>()?;

                // Ensure the table selector is within range, defaulting to the
                // last one if out of range.
                let selector = tracker.stack.pop();
                assert_eq!(selector.val_type, ValType::I32);
                directives.push(Directive::ImmMin {
                    immediate: targets.len() as u32 - 1,
                    input: selector,
                    output: selector,
                });

                // Each target is a br, which expands to a different number of instructions.
                // So we generate all the br's, and pad each to the maximum size.
                //
                // NOTE: This assumes there is a one to one mapping from these operations
                // to Powdr-ASM instructions.
                let mut max_size = 0;
                let mut breaks = Vec::new();
                for target in targets {
                    let br_code = tracker.br_code(target);
                    max_size = max_size.max(br_code.len());
                    breaks.push(br_code);
                }

                // Do the padding
                for br_code in breaks.iter_mut() {
                    br_code.resize_with(max_size, || Directive::WasmOp {
                        op: Operator::Nop,
                        inputs: vec![],
                        output: None,
                    });
                }

                // Emit the SkipMultiple instruction that will land in the correct "br" code.
                directives.push(Directive::SkipMultiple {
                    multiple: max_size as u32,
                    count: selector,
                });
                directives.extend(breaks.into_iter().flatten());

                // The code after the br_table is unreachable.
                tracker.discard_unreachable_and_fix_the_stack(&mut op_reader)?;
            }
            Operator::Return => {
                directives.extend(tracker.return_code());
                tracker.discard_unreachable_and_fix_the_stack(&mut op_reader)?;
            }
            Operator::Unreachable => {
                directives.push(Directive::WasmOp {
                    op: Operator::Unreachable,
                    inputs: vec![],
                    output: None,
                });
                tracker.discard_unreachable_and_fix_the_stack(&mut op_reader)?;
            }
            Operator::Call { function_index } => {
                // Consume the function arguments and place the outputs on the stack.
                let func_type = module.get_func_type(function_index);
                let (bottom_addr, inputs, outputs) =
                    tracker.apply_operation_to_stack(func_type.params(), func_type.results());

                // This can either be a normal function call or a system call.
                if (function_index as usize) < module.imported_functions.len() {
                    // This is a system call, we can inline it.
                    let syscall = module.imported_functions[function_index as usize];
                    directives.push(Directive::Syscall {
                        syscall,
                        inputs,
                        outputs: outputs.to_vec(),
                    });
                } else {
                    // This is a normal function call, we emit the call

                    // Return info is written on the stack, after ther function inputs or outputs,
                    // whichever is bigger.
                    let address = tracker
                        .stack
                        .top_bytes()
                        .max(bottom_addr + many_sz(func_type.results()));

                    let return_info = AllocatedVar {
                        // For the lack of a better type, the return info is a i64, where
                        // the first 32 bits (address-wise) are the saved frame pointer, and the
                        // other 32 bits are the return address.
                        val_type: ValType::I64,
                        address,
                    };

                    directives.push(Directive::Call {
                        function_index,
                        new_fp_delta: bottom_addr,
                        save_return_info_to: return_info,
                    });
                }
            }
            Operator::CallIndirect {
                type_index,
                table_index,
            } => todo!(),
            op => {
                let (inputs, output) = tracker.get_operator_type(&op).unwrap();

                let (_, inputs, output) =
                    tracker.apply_operation_to_stack(&inputs, output.as_slice());

                assert!(output.len() <= 1);
                directives.push(Directive::WasmOp {
                    op,
                    inputs,
                    output: output.first().copied(),
                });
            }
        }
    }

    Ok(directives)
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum FrameKind {
    Function,
    Block { target_label: u32 },
    Loop { target_label: u32 },
    If { target_label: u32, else_label: u32 },
    Else { target_label: u32 },
}

struct Frame {
    blockty: BlockType,
    frame_kind: FrameKind,
    /// The stack height of the block, in bytes, not counting the inputs or outputs.
    stack_height: u32,
}

struct Stack {
    /// The stack is a vector of variables, each one with its type and address.
    /// The address is relative to the stack base.
    stack: Vec<AllocatedVar>,
    stack_base_bytes: u32,
    stack_top_bytes: u32,
}

impl Stack {
    fn new(stack_base_bytes: u32) -> Self {
        Stack {
            stack: Vec::new(),
            stack_base_bytes,
            stack_top_bytes: stack_base_bytes,
        }
    }

    fn push(&mut self, val_type: ValType) {
        self.stack.push(AllocatedVar {
            val_type,
            address: self.stack_top_bytes,
        });
        self.stack_top_bytes += sz(val_type);
    }

    fn pop(&mut self) -> AllocatedVar {
        let var = self.stack.pop().unwrap();
        self.stack_top_bytes -= sz(var.val_type);
        assert_eq!(self.stack_top_bytes, var.address);
        assert!(self.stack_top_bytes >= self.stack_base_bytes);
        var
    }

    fn last(&self) -> Option<&AllocatedVar> {
        self.stack.last()
    }

    fn split_n_from_top(&mut self, count: usize) -> Vec<AllocatedVar> {
        let at = self.stack.len() - count;
        let vars = self.stack.split_off(at);
        if let Some(&var) = vars.first() {
            self.stack_top_bytes = var.address;
            assert!(self.stack_top_bytes >= self.stack_base_bytes);
        }
        vars
    }

    fn drop_n_from_top(&mut self, count: usize) {
        let new_len = self.stack.len() - count;
        if let Some(&var) = self.stack.get(new_len) {
            self.stack_top_bytes = var.address;
            assert!(self.stack_top_bytes >= self.stack_base_bytes);
            self.stack.truncate(new_len);
        }
    }

    fn base_bytes(&self) -> u32 {
        self.stack_base_bytes
    }

    fn top_bytes(&self) -> u32 {
        self.stack_top_bytes
    }

    fn slice(&self) -> &[AllocatedVar] {
        &self.stack
    }

    fn len(&self) -> usize {
        self.stack.len()
    }
}

struct StackTracker<'a> {
    module: &'a ModuleContext,
    func_type: &'a FuncType,
    locals: Vec<AllocatedVar>,
    /// In the middle of the locals, right after the function arguments, in a place
    /// "call" will be able to write, we have the return address and the frame pointer
    /// of the previous frame, which must be restored by the "return" instruction.
    return_info: AllocatedVar,
    stack: Stack,
    control_stack: Vec<Frame>,
}

impl<'a> StackTracker<'a> {
    fn new(
        module: &'a ModuleContext,
        func_idx: u32,
        locals_reader: LocalsReader<'a>,
    ) -> wasmparser::Result<(Self, u32)> {
        // We start by reading the input and local variables.
        let func_type = module.get_func_type(func_idx);

        // The locals are the function arguments and the explicit locals declaration.
        let mut stack_top = 0;
        let mut locals = Vec::new();

        // Function arguments are the first locals. They are the last thing the
        // caller wrote to its stack, so they end up at the bottom of our stack.
        for &val_type in func_type.params() {
            locals.push(AllocatedVar {
                val_type,
                address: stack_top,
            });
            stack_top += sz(val_type);
        }

        // Right before adjusting the frame pointer and jumping, "call" writes the
        // return address and the frame pointer of the previous frame on the top of
        // its stack, giving room for the function outputs, if it happens to be bigger
        // than the inputs. We must also skip this space to find the return info.
        stack_top = stack_top.max(many_sz(func_type.results()));

        let return_info = AllocatedVar {
            val_type: ValType::I64,
            address: stack_top,
        };
        stack_top += sz(return_info.val_type);

        let first_explicit_local = stack_top;

        // Explicitly declared locals comes next.
        for local in locals_reader {
            let (count, val_type) = local?;
            for _ in 0..count {
                locals.push(AllocatedVar {
                    val_type,
                    address: stack_top,
                });
                stack_top += sz(val_type);
            }
        }

        Ok((
            StackTracker {
                module,
                func_type,
                control_stack: vec![Frame {
                    stack_height: 0,
                    blockty: BlockType::FuncType(func_idx),
                    frame_kind: FrameKind::Function,
                }],
                locals,
                return_info,
                stack: Stack::new(stack_top),
            },
            first_explicit_local,
        ))
    }

    /// Asserts the arguments of a block are at the top of the stack.
    fn assert_block_args(&self, blockty: BlockType) {
        let args = self.module.blockty_inputs(blockty);
        self.assert_types_on_stack(args);
    }

    fn assert_types_on_stack(&self, types: &[ValType]) {
        let stack = self.stack.slice();
        assert!(stack.len() >= types.len());
        assert!(stack[stack.len() - types.len()..]
            .iter()
            .zip(types)
            .all(|(stack_var, ty)| stack_var.val_type == *ty));
    }

    /// Return the height of the stack where the frame inputs and outputs sits on top of.
    fn frame_height(&self, blockty: BlockType) -> u32 {
        // When we enter a block, the stack contains the inputs to the block.
        // We must subtract the height of the inputs to know the stack height
        // where the outputs must be placed.
        let input_size = match blockty {
            BlockType::Empty | BlockType::Type(_) => 0,
            BlockType::FuncType(idx) => {
                let func_type = self.module.get_type(idx);
                many_sz(func_type.params())
            }
        };

        self.stack.top_bytes() - input_size
    }

    /// Generate the code of a return, ensuring the outputs are at the expected height.
    fn return_code<'b>(&self) -> Vec<Directive<'b>> {
        self.br_code(self.control_stack.len() as u32 - 1)
    }

    /// Generate the code of a break ("br"), ensuring the outputs are at the expected height.
    fn br_code<'b>(&self, relative_depth: u32) -> Vec<Directive<'b>> {
        // When breaking, the stack might be bigger than the required height for the target label.
        // If so, we must copy the outputs to the expected height.
        let cs_len = self.control_stack.len();
        assert!(relative_depth < cs_len as u32);

        let single_arg;

        let target_frame = &self.control_stack[cs_len - relative_depth as usize - 1];

        let args = if let FrameKind::Loop { .. } = target_frame.frame_kind {
            // Loop is special because br sends the execution to
            // the top of the loop, so the arguments are the inputs.
            self.module.blockty_inputs(target_frame.blockty)
        } else {
            match target_frame.blockty {
                BlockType::Empty => &[][..],
                BlockType::Type(val_type) => {
                    single_arg = [val_type];
                    &single_arg
                }
                BlockType::FuncType(idx) => {
                    let func_type = self.module.get_type(idx);
                    func_type.results()
                }
            }
        };

        self.assert_types_on_stack(args);

        // Copy the outputs to the expected height, if needed.
        let mut directives = Vec::new();

        let jump_directive = match target_frame.frame_kind {
            FrameKind::Function => Directive::Return {
                return_info: self.return_info,
            },
            FrameKind::Block { target_label }
            | FrameKind::Loop { target_label }
            | FrameKind::If { target_label, .. }
            | FrameKind::Else { target_label } => Directive::Jump {
                target: target_label,
            },
        };

        let stack = self.stack.slice();
        if stack[stack.len() - args.len()].address != target_frame.stack_height {
            let src_args = &stack[(stack.len() - args.len())..];

            let dest_pos = stack.partition_point(|v| v.address < target_frame.stack_height);

            let mut dest_stack_top = stack
                .get(dest_pos)
                .map(|v| v.address)
                .unwrap_or(self.stack.top_bytes());
            assert_eq!(dest_stack_top, target_frame.stack_height);

            for src in src_args {
                let var_size = sz(src.val_type);

                // Emit one move instruction for each 32-bit chunk.
                assert!(var_size % 4 == 0);
                for i in (0..var_size).step_by(4) {
                    directives.push(Directive::MoveRel32 {
                        src: src.address + i,
                        dest: dest_stack_top + i,
                    });
                }
                dest_stack_top += var_size;
            }
        }

        directives.push(jump_directive);

        directives
    }

    /// The usual operation of a wasm instruction is to consume some values from the stack,
    /// and produce some other values on the stack. This function performs this operation,
    /// given the instruciton type.
    ///
    /// Returns the lowest address it got into the stack after popping all the inputs,
    /// followed by the input vars.
    fn apply_operation_to_stack(
        &mut self,
        inputs: &[ValType],
        outputs: &[ValType],
    ) -> (u32, Vec<AllocatedVar>, &[AllocatedVar]) {
        // Check we have the correct number and types of inputs on the stack
        self.assert_types_on_stack(inputs);

        // Pop the inputs
        let input_vars = self.stack.split_n_from_top(inputs.len());

        let bottom_addr = self.stack.top_bytes();

        // Sanit check the stack
        let bottom_limit = self
            .control_stack
            .last()
            .map_or(self.stack.base_bytes(), |frame| frame.stack_height);
        assert!(bottom_addr >= bottom_limit);

        let outputs_start = self.stack.len();
        for &ty in outputs {
            self.stack.push(ty);
        }

        let output_vars = &self.stack.slice()[outputs_start..];

        (bottom_addr, input_vars, output_vars)
    }

    /// Some instructions unconditionally divert the control flow, leaving everithing between
    /// themselves and the end of the current block unreachable, and leaving the stack in an
    /// undefined state.
    ///
    /// Call this function after processing such instructions to discard the unreachable code
    /// and fix the stack.
    fn discard_unreachable_and_fix_the_stack(
        &mut self,
        op_reader: &mut OperatorsIterator<'_>,
    ) -> wasmparser::Result<()> {
        // Discard unreachable code
        let mut stack_count = 0;
        for operator in op_reader {
            match operator? {
                Operator::Block { .. } | Operator::Loop { .. } | Operator::If { .. } => {
                    stack_count += 1;
                }
                Operator::Else => {
                    if stack_count == 0 {
                        break;
                    }
                }
                Operator::End => {
                    if stack_count == 0 {
                        break;
                    }
                    stack_count -= 1;
                }
                _ => {}
            }
        }

        // Fix the stack
        // Clear the leftovers from this frame:
        let last_frame = self.control_stack.last().unwrap();
        while self
            .stack
            .last()
            .is_some_and(|var| var.address >= last_frame.stack_height)
        {
            self.stack.pop();
        }
        assert!(self.stack.top_bytes() == last_frame.stack_height);

        // Create the expected outputs:
        match last_frame.blockty {
            BlockType::Empty => {}
            BlockType::Type(val_type) => {
                self.stack.push(val_type);
            }
            BlockType::FuncType(idx) => {
                let func_type = self.module.get_type(idx);
                for &ty in func_type.results() {
                    self.stack.push(ty);
                }
            }
        }

        Ok(())
    }

    /// Returns the list of input types and output types of an operator.
    fn get_operator_type(&self, op: &Operator) -> Option<(Vec<ValType>, Option<ValType>)> {
        let ty = match op {
            // # Numeric instructions
            // ## const
            Operator::I32Const { .. } => (vec![], Some(ValType::I32)),
            Operator::I64Const { .. } => (vec![], Some(ValType::I64)),
            Operator::F32Const { .. } => (vec![], Some(ValType::F32)),
            Operator::F64Const { .. } => (vec![], Some(ValType::F64)),
            // ## unop
            Operator::I32Clz
            | Operator::I32Ctz
            | Operator::I32Popcnt
            | Operator::I32Extend8S
            | Operator::I32Extend16S => (vec![ValType::I32], Some(ValType::I32)),
            Operator::I64Clz
            | Operator::I64Ctz
            | Operator::I64Popcnt
            | Operator::I64Extend8S
            | Operator::I64Extend16S
            | Operator::I64Extend32S => (vec![ValType::I64], Some(ValType::I64)),
            Operator::F32Abs
            | Operator::F32Neg
            | Operator::F32Sqrt
            | Operator::F32Ceil
            | Operator::F32Floor
            | Operator::F32Trunc
            | Operator::F32Nearest => (vec![ValType::F32], Some(ValType::F32)),
            Operator::F64Abs
            | Operator::F64Neg
            | Operator::F64Sqrt
            | Operator::F64Ceil
            | Operator::F64Floor
            | Operator::F64Trunc
            | Operator::F64Nearest => (vec![ValType::F64], Some(ValType::F64)),
            // ## binop
            Operator::I32Add
            | Operator::I32Sub
            | Operator::I32Mul
            | Operator::I32DivU
            | Operator::I32DivS
            | Operator::I32RemU
            | Operator::I32RemS
            | Operator::I32And
            | Operator::I32Or
            | Operator::I32Xor
            | Operator::I32Shl
            | Operator::I32ShrU
            | Operator::I32ShrS
            | Operator::I32Rotl
            | Operator::I32Rotr => (vec![ValType::I32, ValType::I32], Some(ValType::I32)),
            Operator::I64Add
            | Operator::I64Sub
            | Operator::I64Mul
            | Operator::I64DivU
            | Operator::I64DivS
            | Operator::I64RemU
            | Operator::I64RemS
            | Operator::I64And
            | Operator::I64Or
            | Operator::I64Xor
            | Operator::I64Shl
            | Operator::I64ShrU
            | Operator::I64ShrS
            | Operator::I64Rotl
            | Operator::I64Rotr => (vec![ValType::I64, ValType::I64], Some(ValType::I64)),
            Operator::F32Add
            | Operator::F32Sub
            | Operator::F32Mul
            | Operator::F32Div
            | Operator::F32Min
            | Operator::F32Max
            | Operator::F32Copysign => (vec![ValType::F32, ValType::F32], Some(ValType::F32)),
            Operator::F64Add
            | Operator::F64Sub
            | Operator::F64Mul
            | Operator::F64Div
            | Operator::F64Min
            | Operator::F64Max
            | Operator::F64Copysign => (vec![ValType::F64, ValType::F64], Some(ValType::F64)),
            // ## testop
            Operator::I32Eqz => (vec![ValType::I32], Some(ValType::I32)),
            Operator::I64Eqz => (vec![ValType::I64], Some(ValType::I32)),
            // ## relop
            Operator::I32Eq
            | Operator::I32Ne
            | Operator::I32LtU
            | Operator::I32LtS
            | Operator::I32GtU
            | Operator::I32GtS
            | Operator::I32LeU
            | Operator::I32LeS
            | Operator::I32GeU
            | Operator::I32GeS => (vec![ValType::I32, ValType::I32], Some(ValType::I32)),
            Operator::I64Eq
            | Operator::I64Ne
            | Operator::I64LtU
            | Operator::I64LtS
            | Operator::I64GtU
            | Operator::I64GtS
            | Operator::I64LeU
            | Operator::I64LeS
            | Operator::I64GeU
            | Operator::I64GeS => (vec![ValType::I64, ValType::I64], Some(ValType::I32)),
            Operator::F32Eq
            | Operator::F32Ne
            | Operator::F32Lt
            | Operator::F32Gt
            | Operator::F32Le
            | Operator::F32Ge => (vec![ValType::F32, ValType::F32], Some(ValType::I32)),
            // ## cvtop
            Operator::I32WrapI64 => (vec![ValType::I64], Some(ValType::I32)),
            Operator::I64ExtendI32U | Operator::I64ExtendI32S => {
                (vec![ValType::I32], Some(ValType::I64))
            }
            Operator::I32TruncF32U
            | Operator::I32TruncF32S
            | Operator::I32TruncSatF32U
            | Operator::I32TruncSatF32S
            | Operator::I32ReinterpretF32 => (vec![ValType::F32], Some(ValType::I32)),
            Operator::I64TruncF32U
            | Operator::I64TruncF32S
            | Operator::I64TruncSatF32U
            | Operator::I64TruncSatF32S => (vec![ValType::F32], Some(ValType::I64)),
            Operator::I32TruncF64U
            | Operator::I32TruncF64S
            | Operator::I32TruncSatF64U
            | Operator::I32TruncSatF64S => (vec![ValType::F64], Some(ValType::I32)),
            Operator::I64TruncF64U
            | Operator::I64TruncF64S
            | Operator::I64TruncSatF64U
            | Operator::I64TruncSatF64S
            | Operator::I64ReinterpretF64 => (vec![ValType::F64], Some(ValType::I64)),
            Operator::F32DemoteF64 => (vec![ValType::F64], Some(ValType::F32)),
            Operator::F64PromoteF32 => (vec![ValType::F32], Some(ValType::F64)),
            Operator::F32ConvertI32U | Operator::F32ConvertI32S | Operator::F32ReinterpretI32 => {
                (vec![ValType::I32], Some(ValType::F32))
            }
            Operator::F64ConvertI32U | Operator::F64ConvertI32S => {
                (vec![ValType::I32], Some(ValType::F64))
            }
            Operator::F32ConvertI64U | Operator::F32ConvertI64S => {
                (vec![ValType::I64], Some(ValType::F32))
            }
            Operator::F64ConvertI64U | Operator::F64ConvertI64S | Operator::F64ReinterpretI64 => {
                (vec![ValType::I64], Some(ValType::F64))
            }

            // # Reference instructions
            Operator::RefNull { hty } => (
                vec![],
                Some(ValType::Ref(RefType::new(true, *hty).unwrap())),
            ),
            Operator::RefIsNull => {
                let ValType::Ref(ref_type) = self.stack.last().unwrap().val_type else {
                    panic!("ref.is_null expects a reference type")
                };
                assert!(ref_type.is_func_ref() || ref_type.is_extern_ref());
                (vec![ValType::Ref(ref_type)], Some(ValType::I32))
            }
            Operator::RefFunc { .. } => (vec![], Some(ValType::Ref(RefType::FUNCREF))),

            // TODO: # Vector instructions

            // # Parametric instructions
            Operator::Drop => (vec![self.stack.last().unwrap().val_type], None),
            Operator::Select => {
                let len = self.stack.len();
                let choices = &self.stack.slice()[(len - 3)..(len - 1)];
                let ty = choices[0].val_type;
                assert_eq!(ty, choices[1].val_type);
                (vec![ty, ty, ValType::I32], Some(ty))
            }

            // # Variable instructions
            Operator::LocalGet { local_index } => {
                let local = &self.locals[*local_index as usize];
                (vec![], Some(local.val_type))
            }
            Operator::LocalSet { local_index } => {
                let local = &self.locals[*local_index as usize];
                (vec![local.val_type], None)
            }
            Operator::LocalTee { local_index } => {
                let local = &self.locals[*local_index as usize];
                (vec![local.val_type], Some(local.val_type))
            }
            Operator::GlobalGet { global_index } => {
                let global = &self.module.globals[*global_index as usize];
                (vec![], Some(global.val_type))
            }
            Operator::GlobalSet { global_index } => {
                let global = &self.module.globals[*global_index as usize];
                (vec![global.val_type], None)
            }

            // # Table instructions
            Operator::TableGet { table } => {
                let ty = &self.module.table_types[*table as usize];
                (vec![ValType::I32], Some(ValType::Ref(*ty)))
            }
            Operator::TableSet { table } => {
                let ty = &self.module.table_types[*table as usize];
                (vec![ValType::I32, ValType::Ref(*ty)], None)
            }
            Operator::TableSize { .. } => (vec![], Some(ValType::I32)),
            Operator::TableGrow { table } => {
                let ty = &self.module.table_types[*table as usize];
                (vec![ValType::Ref(*ty), ValType::I32], Some(ValType::I32))
            }
            Operator::TableFill { table } => {
                let ty = &self.module.table_types[*table as usize];
                (vec![ValType::I32, ValType::Ref(*ty), ValType::I32], None)
            }
            Operator::TableCopy { .. } | Operator::TableInit { .. } => {
                (vec![ValType::I32, ValType::I32, ValType::I32], None)
            }
            Operator::ElemDrop { .. } => (vec![], None),

            // # Memory instructions
            // TODO: implement the vector instructions
            Operator::I32Load { .. }
            | Operator::I32Load8U { .. }
            | Operator::I32Load8S { .. }
            | Operator::I32Load16U { .. }
            | Operator::I32Load16S { .. }
            | Operator::MemoryGrow { .. } => (vec![ValType::I32], Some(ValType::I32)),
            Operator::I64Load { .. }
            | Operator::I64Load8U { .. }
            | Operator::I64Load8S { .. }
            | Operator::I64Load16U { .. }
            | Operator::I64Load16S { .. }
            | Operator::I64Load32U { .. }
            | Operator::I64Load32S { .. } => (vec![ValType::I32], Some(ValType::I64)),
            Operator::F32Load { .. } => (vec![ValType::I32], Some(ValType::F32)),
            Operator::F64Load { .. } => (vec![ValType::I32], Some(ValType::F64)),
            Operator::I32Store { .. }
            | Operator::I32Store8 { .. }
            | Operator::I32Store16 { .. } => (vec![ValType::I32, ValType::I32], None),
            Operator::I64Store { .. }
            | Operator::I64Store8 { .. }
            | Operator::I64Store16 { .. }
            | Operator::I64Store32 { .. } => (vec![ValType::I32, ValType::I64], None),
            Operator::F32Store { .. } => (vec![ValType::I32, ValType::F32], None),
            Operator::F64Store { .. } => (vec![ValType::I32, ValType::F64], None),
            Operator::MemorySize { .. } => (vec![], Some(ValType::I32)),
            Operator::MemoryFill { .. }
            | Operator::MemoryCopy { .. }
            | Operator::MemoryInit { .. } => (vec![ValType::I32, ValType::I32, ValType::I32], None),
            Operator::DataDrop { .. } => (vec![], None),

            // # Control instructions
            Operator::Nop => (vec![], None),
            // Most control instructions must be handled separately.
            // We return None for them:
            Operator::Unreachable
            | Operator::Call { .. }
            | Operator::CallIndirect { .. }
            | Operator::Block { .. }
            | Operator::Loop { .. }
            | Operator::If { .. }
            | Operator::Else
            | Operator::End
            | Operator::Br { .. }
            | Operator::BrIf { .. }
            | Operator::BrTable { .. }
            | Operator::Return => return None,
            _ => todo!(),
        };

        Some(ty)
    }
}
