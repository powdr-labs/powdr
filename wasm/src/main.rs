use std::{result, str::FromStr};

use powdr_syscalls::Syscall;
use wasmparser::{
    BlockType, CompositeInnerType, FrameKind, FuncType, FunctionBody, LocalsReader, Operator,
    OperatorsIterator, Parser, Payload, RefType, SubType, TableInit, TableType, TypeRef, ValType,
};

const MEM_ALLOCATION_START: u32 = 0x800;

struct ModuleContext {
    types: Vec<SubType>,
    func_types: Vec<u32>,
    tables: Vec<TableType>,
    globals: Vec<AllocatedVar>,
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
}

fn main() -> wasmparser::Result<()> {
    // TODO: do proper command line argument parsing
    let args: Vec<String> = std::env::args().collect();
    let wasm_file = std::fs::read(&args[1]).unwrap();

    let parser = Parser::new(0);

    let mut ctx = ModuleContext {
        types: Vec::new(),
        func_types: Vec::new(),
        tables: Vec::new(),
        globals: Vec::new(),
    };

    let mut imported_functions = Vec::new();
    let mut defined_functions = Vec::new();

    let mut start_function = None;

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
                    if import.module != "powdr" {
                        panic!("Only \"powdr\" module is available for imports");
                    }
                    if let TypeRef::Func(type_idx) = import.ty {
                        // Lets see if the name is known
                        if let Ok(syscall) = Syscall::from_str(import.name) {
                            // Lets see if the type matches the expectations.
                            let ty = ctx.get_type(type_idx);
                            let given_arity = (many_sz(ty.params()) / 4, many_sz(ty.results()) / 4);

                            let expected_arity = syscall.arity();
                            if given_arity != expected_arity {
                                panic!(
                                    "Syscall \"{}\" expects {} 32-bit inputs and {} 32-bit outputs, but was imported with {} 32-bit inputs and {} 32-bit outputs",
                                    import.name, expected_arity.0, expected_arity.1, given_arity.0, given_arity.1
                                );
                            }

                            log::debug!("Imported syscall: {}", import.name);
                            imported_functions.push(syscall);

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
                    ctx.tables.push(table.ty);

                    if !matches!(table.init, TableInit::RefNull) {
                        unsupported_feature_found = true;
                        log::error!("Table initialization is not supported");
                    }
                }
            }
            Payload::MemorySection(section_limited) => todo!(),
            Payload::GlobalSection(section) => {
                let mut globals_addr = MEM_ALLOCATION_START;
                for global in section {
                    // Allocate the globals sequentially
                    let val_type = global?.ty.content_type;
                    let size = sz(val_type);
                    ctx.globals.push(AllocatedVar {
                        val_type,
                        address: globals_addr,
                    });
                    globals_addr += size;

                    // TODO: initialize the globals with the init_expr
                }
            }
            Payload::ExportSection(section_limited) => {
                // TODO: support modules loading and cross module dependencies.
                // While we don't support that, export does nothing and can be ignored.
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
            Payload::StartSection { func, range } => start_function = Some(func),
            Payload::ElementSection(section_limited) => todo!(),
            Payload::DataCountSection { count, range } => todo!(),
            Payload::CodeSectionEntry(function) => {
                // By the time we get here, the ctx will be complete,
                // because all previous sections have been processed.

                let definition =
                    infinite_registers_allocation(&ctx, defined_functions.len() as u32, function)?;
                defined_functions.push(definition);
            }
            Payload::DataSection(section_limited) => todo!(),
            Payload::End(_) => {
                log::debug!("End of the module");
            }
            unsupported_section => {
                unsupported_feature_found = true;
                log::error!("Unsupported section found: {unsupported_section:?}");
            }
        }
    }

    assert!(
        !unsupported_feature_found,
        "Only WebAssembly Release 2.0 is supported"
    );

    Ok(())
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

/// Allocates the locals and the stack at addresses starting
/// from 0, assuming one byte per address.
fn infinite_registers_allocation<'a>(
    module: &ModuleContext,
    func_idx: u32,
    body: FunctionBody<'a>,
) -> wasmparser::Result<Vec<Directive<'a>>> {
    // Tracks the frame stack. Used to calculate arity.
    let (mut tracker, first_explicit_local) =
        StackTracker::new(module, func_idx, body.get_locals_reader()?)?;

    // The fist directives are zeroing the explicit locals.
    let mut directives: Vec<Directive> = (first_explicit_local..tracker.stack.base_bytes())
        .step_by(4)
        .map(|address| Directive::WasmOp {
            op: Operator::I32Const { value: 0 },
            inputs: vec![AllocatedVar {
                val_type: ValType::I32,
                address,
            }],
        })
        .collect();

    let mut labels = 0..;

    // The rest of the directives are taken from the function body definition.
    let mut op_reader = body.get_operators_reader()?.into_iter();
    while let Some(operator) = op_reader.next() {
        // Match first the control operators, which require special handling.
        match operator? {
            Operator::Block { blockty } => {
                tracker.assert_block_args(blockty);
                tracker.control_stack.push(Frame {
                    stack_height: tracker.frame_height(blockty),
                    blockty,
                    frame_kind: FrameKind::Block,
                    else_label: None,
                    target_label: labels.next().unwrap(),
                });
            }
            Operator::Loop { blockty } => {
                tracker.assert_block_args(blockty);
                let target_label = labels.next().unwrap();
                tracker.control_stack.push(Frame {
                    stack_height: tracker.frame_height(blockty),
                    blockty,
                    frame_kind: FrameKind::Loop,
                    else_label: None,
                    target_label,
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
                    frame_kind: FrameKind::If,
                    else_label: Some(else_label),
                    target_label: labels.next().unwrap(),
                });
            }
            Operator::Else => {
                // Else happens at the same level as the corresponding if.
                let last_frame = tracker.control_stack.last_mut().unwrap();

                assert!(last_frame.frame_kind == FrameKind::If);
                last_frame.frame_kind = FrameKind::Else;

                let else_label = last_frame.else_label.unwrap();
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
                    FrameKind::If => {
                        // The else is missing, so the else_label matches the target_label.
                        directives.push(Directive::Label(last_frame.else_label.unwrap()));
                        directives.push(Directive::Label(last_frame.target_label));
                    }
                    FrameKind::Block | FrameKind::Else => {
                        directives.push(Directive::Label(last_frame.target_label));
                    }
                    FrameKind::Loop => {
                        // Do nothing, as the loop label was already emited.
                    }
                    _ => panic!("unsupported webassembly feature"),
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
                directives.extend(tracker.br_code(tracker.control_stack.len() as u32));
                tracker.discard_unreachable_and_fix_the_stack(&mut op_reader)?;
            }
            Operator::Unreachable => {
                directives.push(Directive::WasmOp {
                    op: Operator::Unreachable,
                    inputs: vec![],
                });
                tracker.discard_unreachable_and_fix_the_stack(&mut op_reader)?;
            }
            Operator::Call { function_index } => {
                // For the lack of a better type, the return info is a i64, where
                // the first 32 bits (address-wise) are the saved frame pointer, and the
                // other 32 bits are the return address.
                let return_info = AllocatedVar {
                    val_type: ValType::I64,
                    address: tracker.stack.top_bytes(),
                };

                // Consume the function arguments and place the outputs on the stack.
                let func_type = module.get_func_type(function_index);
                let (bottom_addr, _) =
                    tracker.apply_operation_to_stack(func_type.params(), func_type.results());

                directives.push(Directive::Call {
                    function_index,
                    new_fp_delta: bottom_addr,
                    save_return_info_to: return_info,
                });
            }
            Operator::CallIndirect {
                type_index,
                table_index,
            } => todo!(),
            op => {
                let (inputs, outputs) = tracker.get_operator_type(&op).unwrap();

                let (_, inputs) = tracker.apply_operation_to_stack(&inputs, &outputs);
                directives.push(Directive::WasmOp { op, inputs });
            }
        }
    }

    // The function has ended, insert a return instruction.
    directives.push(Directive::Return {
        return_info: tracker.return_info,
    });

    Ok(directives)
}

/// Type size, in bytes
fn sz(val_type: ValType) -> u32 {
    match val_type {
        ValType::I32 => 4,
        ValType::I64 => 8,
        ValType::F32 => 4,
        ValType::F64 => 8,
        ValType::V128 => 16,
        ValType::Ref(_) => 4,
    }
}

/// Size of many types, in bytes
fn many_sz(val_types: &[ValType]) -> u32 {
    val_types.iter().map(|&ty| sz(ty)).sum()
}

struct Frame {
    blockty: BlockType,
    frame_kind: FrameKind,
    else_label: Option<u32>,
    target_label: u32,
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
    ///
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
        // frame pointer of the previous frame on the top of its stack, which ends up here.
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
                control_stack: Vec::new(),
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

    /// Generate the code of a break ("br"), ensuring the outputs are at the expected height.
    fn br_code<'b>(&self, relative_depth: u32) -> Vec<Directive<'b>> {
        // When breaking, the stack might be bigger than the required height for the target label.
        // If so, we must copy the outputs to the expected height.
        let cs_len = self.control_stack.len();
        assert!(relative_depth <= cs_len as u32);

        let single_arg;
        let (args, jump_directive, target_height) = if relative_depth == cs_len as u32 {
            // TODO: this code is all wrong! Return values are not placed on stack, but overlapping the inputs.
            todo!();

            // The target is the function itself, this is a return.
            (
                self.func_type.results(),
                Directive::Return {
                    return_info: self.return_info,
                },
                self.stack.base_bytes(),
            )
        } else {
            let target_frame = &self.control_stack[cs_len - relative_depth as usize - 1];

            let args = if target_frame.frame_kind == FrameKind::Loop {
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

            (
                args,
                Directive::Jump {
                    target: target_frame.target_label,
                },
                target_frame.stack_height,
            )
        };

        self.assert_types_on_stack(args);

        // Copy the outputs to the expected height, if needed.
        let mut directives = Vec::new();
        let stack = self.stack.slice();
        if stack[stack.len() - args.len()].address != target_height {
            let src_args = &stack[(stack.len() - args.len())..];

            let dest_pos = stack.partition_point(|v| v.address < target_height);

            let mut dest_stack_top = stack
                .get(dest_pos - 1)
                .map(|v| v.address + sz(v.val_type))
                .unwrap_or(self.stack.base_bytes());
            assert_eq!(dest_stack_top, target_height);

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
    ) -> (u32, Vec<AllocatedVar>) {
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

        for &ty in outputs {
            self.stack.push(ty);
        }

        (bottom_addr, input_vars)
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
    fn get_operator_type(&self, op: &Operator) -> Option<(Vec<ValType>, Vec<ValType>)> {
        let ty = match op {
            // # Numeric instructios
            // ## const
            Operator::I32Const { .. } => (vec![], vec![ValType::I32]),
            Operator::I64Const { .. } => (vec![], vec![ValType::I64]),
            Operator::F32Const { .. } => (vec![], vec![ValType::F32]),
            Operator::F64Const { .. } => (vec![], vec![ValType::F64]),
            // ## unop
            Operator::I32Clz
            | Operator::I32Ctz
            | Operator::I32Popcnt
            | Operator::I32Extend8S
            | Operator::I32Extend16S => (vec![ValType::I32], vec![ValType::I32]),
            Operator::I64Clz
            | Operator::I64Ctz
            | Operator::I64Popcnt
            | Operator::I64Extend8S
            | Operator::I64Extend16S
            | Operator::I64Extend32S => (vec![ValType::I64], vec![ValType::I64]),
            Operator::F32Abs
            | Operator::F32Neg
            | Operator::F32Sqrt
            | Operator::F32Ceil
            | Operator::F32Floor
            | Operator::F32Trunc
            | Operator::F32Nearest => (vec![ValType::F32], vec![ValType::F32]),
            Operator::F64Abs
            | Operator::F64Neg
            | Operator::F64Sqrt
            | Operator::F64Ceil
            | Operator::F64Floor
            | Operator::F64Trunc
            | Operator::F64Nearest => (vec![ValType::F64], vec![ValType::F64]),
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
            | Operator::I32Rotr => (vec![ValType::I32, ValType::I32], vec![ValType::I32]),
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
            | Operator::I64Rotr => (vec![ValType::I64, ValType::I64], vec![ValType::I64]),
            Operator::F32Add
            | Operator::F32Sub
            | Operator::F32Mul
            | Operator::F32Div
            | Operator::F32Min
            | Operator::F32Max
            | Operator::F32Copysign => (vec![ValType::F32, ValType::F32], vec![ValType::F32]),
            Operator::F64Add
            | Operator::F64Sub
            | Operator::F64Mul
            | Operator::F64Div
            | Operator::F64Min
            | Operator::F64Max
            | Operator::F64Copysign => (vec![ValType::F64, ValType::F64], vec![ValType::F64]),
            // ## testop
            Operator::I32Eqz => (vec![ValType::I32], vec![ValType::I32]),
            Operator::I64Eqz => (vec![ValType::I64], vec![ValType::I32]),
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
            | Operator::I32GeS => (vec![ValType::I32, ValType::I32], vec![ValType::I32]),
            Operator::I64Eq
            | Operator::I64Ne
            | Operator::I64LtU
            | Operator::I64LtS
            | Operator::I64GtU
            | Operator::I64GtS
            | Operator::I64LeU
            | Operator::I64LeS
            | Operator::I64GeU
            | Operator::I64GeS => (vec![ValType::I64, ValType::I64], vec![ValType::I32]),
            Operator::F32Eq
            | Operator::F32Ne
            | Operator::F32Lt
            | Operator::F32Gt
            | Operator::F32Le
            | Operator::F32Ge => (vec![ValType::F32, ValType::F32], vec![ValType::I32]),
            // ## cvtop
            Operator::I32WrapI64 => (vec![ValType::I64], vec![ValType::I32]),
            Operator::I64ExtendI32U | Operator::I64ExtendI32S => {
                (vec![ValType::I32], vec![ValType::I64])
            }
            Operator::I32TruncF32U
            | Operator::I32TruncF32S
            | Operator::I32TruncSatF32U
            | Operator::I32TruncSatF32S
            | Operator::I32ReinterpretF32 => (vec![ValType::F32], vec![ValType::I32]),
            Operator::I64TruncF32U
            | Operator::I64TruncF32S
            | Operator::I64TruncSatF32U
            | Operator::I64TruncSatF32S => (vec![ValType::F32], vec![ValType::I64]),
            Operator::I32TruncF64U
            | Operator::I32TruncF64S
            | Operator::I32TruncSatF64U
            | Operator::I32TruncSatF64S => (vec![ValType::F64], vec![ValType::I32]),
            Operator::I64TruncF64U
            | Operator::I64TruncF64S
            | Operator::I64TruncSatF64U
            | Operator::I64TruncSatF64S
            | Operator::I64ReinterpretF64 => (vec![ValType::F64], vec![ValType::I64]),
            Operator::F32DemoteF64 => (vec![ValType::F64], vec![ValType::F32]),
            Operator::F64PromoteF32 => (vec![ValType::F32], vec![ValType::F64]),
            Operator::F32ConvertI32U | Operator::F32ConvertI32S | Operator::F32ReinterpretI32 => {
                (vec![ValType::I32], vec![ValType::F32])
            }
            Operator::F64ConvertI32U | Operator::F64ConvertI32S => {
                (vec![ValType::I32], vec![ValType::F64])
            }
            Operator::F32ConvertI64U | Operator::F32ConvertI64S => {
                (vec![ValType::I64], vec![ValType::F32])
            }
            Operator::F64ConvertI64U | Operator::F64ConvertI64S | Operator::F64ReinterpretI64 => {
                (vec![ValType::I64], vec![ValType::F64])
            }

            // # Reference instructions
            Operator::RefNull { hty } => (
                vec![],
                vec![ValType::Ref(RefType::new(true, *hty).unwrap())],
            ),
            Operator::RefIsNull => {
                // This type is dependant on the input type, so we must look at the stack.
                let ValType::Ref(ref_type) = self.stack.last().unwrap().val_type else {
                    panic!("ref.is_null expects a reference type")
                };
                assert!(ref_type.is_func_ref() || ref_type.is_extern_ref());
                (vec![ValType::Ref(ref_type)], vec![ValType::I32])
            }
            Operator::RefFunc { .. } => (vec![], vec![ValType::Ref(RefType::FUNCREF)]),

            // TODO: # Vector instructions
            // lets skip vector instructions for now, as we can switch it off at LLVM...

            // # Parametric instructions
            Operator::Drop => (vec![self.stack.last().unwrap().val_type], vec![]),
            Operator::Select => {
                let len = self.stack.len();
                let choices = &self.stack.slice()[(len - 3)..(len - 1)];
                let ty = choices[0].val_type;
                assert_eq!(ty, choices[1].val_type);
                (vec![ty, ty, ValType::I32], vec![ty])
            }

            // # Variable instructions
            Operator::LocalGet { local_index } => {
                let local = &self.locals[*local_index as usize];
                (vec![], vec![local.val_type])
            }
            Operator::LocalSet { local_index } => {
                let local = &self.locals[*local_index as usize];
                (vec![local.val_type], vec![])
            }
            Operator::LocalTee { local_index } => {
                let local = &self.locals[*local_index as usize];
                (vec![local.val_type], vec![local.val_type])
            }
            Operator::GlobalGet { global_index } => {
                let global = &self.module.globals[*global_index as usize];
                (vec![], vec![global.val_type])
            }
            Operator::GlobalSet { global_index } => {
                let global = &self.module.globals[*global_index as usize];
                (vec![global.val_type], vec![])
            }

            // # Table instructions
            Operator::TableGet { table } => {
                let table = &self.module.tables[*table as usize];
                (vec![ValType::I32], vec![ValType::Ref(table.element_type)])
            }
            Operator::TableSet { table } => {
                let table = &self.module.tables[*table as usize];
                (vec![ValType::I32, ValType::Ref(table.element_type)], vec![])
            }
            Operator::TableSize { .. } => (vec![], vec![ValType::I32]),
            Operator::TableGrow { table } => {
                let table = &self.module.tables[*table as usize];
                (
                    vec![ValType::Ref(table.element_type), ValType::I32],
                    vec![ValType::I32],
                )
            }
            Operator::TableFill { table } => {
                let table = &self.module.tables[*table as usize];
                (
                    vec![ValType::I32, ValType::Ref(table.element_type), ValType::I32],
                    vec![],
                )
            }
            Operator::TableCopy { .. } | Operator::TableInit { .. } => {
                (vec![ValType::I32, ValType::I32, ValType::I32], vec![])
            }
            Operator::ElemDrop { .. } => (vec![], vec![]),

            // # Memory instructions
            // TODO: implement the vector instructions
            Operator::I32Load { .. }
            | Operator::I32Load8U { .. }
            | Operator::I32Load8S { .. }
            | Operator::I32Load16U { .. }
            | Operator::I32Load16S { .. }
            | Operator::MemoryGrow { .. } => (vec![ValType::I32], vec![ValType::I32]),
            Operator::I64Load { .. }
            | Operator::I64Load8U { .. }
            | Operator::I64Load8S { .. }
            | Operator::I64Load16U { .. }
            | Operator::I64Load16S { .. }
            | Operator::I64Load32U { .. }
            | Operator::I64Load32S { .. } => (vec![ValType::I32], vec![ValType::I64]),
            Operator::F32Load { .. } => (vec![ValType::I32], vec![ValType::F32]),
            Operator::F64Load { .. } => (vec![ValType::I32], vec![ValType::F64]),
            Operator::I32Store { .. }
            | Operator::I32Store8 { .. }
            | Operator::I32Store16 { .. } => (vec![ValType::I32, ValType::I32], vec![]),
            Operator::I64Store { .. }
            | Operator::I64Store8 { .. }
            | Operator::I64Store16 { .. }
            | Operator::I64Store32 { .. } => (vec![ValType::I32, ValType::I64], vec![]),
            Operator::F32Store { .. } => (vec![ValType::I32, ValType::F32], vec![]),
            Operator::F64Store { .. } => (vec![ValType::I32, ValType::F64], vec![]),
            Operator::MemorySize { .. } => (vec![], vec![ValType::I32]),
            Operator::MemoryFill { .. }
            | Operator::MemoryCopy { .. }
            | Operator::MemoryInit { .. } => {
                (vec![ValType::I32, ValType::I32, ValType::I32], vec![])
            }
            Operator::DataDrop { .. } => (vec![], vec![]),

            // # Control instructions
            Operator::Nop => (vec![], vec![]),
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
