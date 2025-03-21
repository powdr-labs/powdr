use std::iter::once;

use wasmparser::{
    BlockType, CompositeInnerType, FrameKind, FuncType, FunctionBody, LocalsReader, Operator,
    Parser, Payload, RefType, SubType, TableType, ValType, Validator,
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

    let mut func_idx = 0;

    // TODO: validate while parsing

    // The payloads are processed in the order they appear in the file, so each variable written
    // in one step is available in the next steps.
    for payload in parser.parse_all(&wasm_file) {
        match payload? {
            Payload::TypeSection(section) => {
                for rec_group in section {
                    let mut iter = rec_group?.into_types();
                    let ty = match (iter.next(), iter.next()) {
                        (Some(ty), None) => ty,
                        _ => {
                            // Apparently WebAssembly 3.0 is much more complicated, and has complex
                            // type definitions, and garbage collector, and exceptions. We should probably
                            // stick to the 2.0 version for Powdr.
                            panic!("gc proposal not supported")
                        }
                    };
                    ctx.types.push(ty);
                }
            }
            Payload::FunctionSection(section) => {
                for ty in section {
                    ctx.func_types.push(ty?);
                }
            }
            Payload::TableSection(section) => {
                for table in section {
                    ctx.tables.push(table?.ty);

                    // TODO: initialize the tables with their values
                }
            }
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
            Payload::CodeSectionEntry(function) => {
                // By the time we get here, the ctx will be complete,
                // because all previous sections have been processed.

                infinite_registers_allocation(&ctx, func_idx, function)?;
                func_idx += 1;
            }
            _ => {
                // uninteresting section
            }
        }
    }

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
        /// The value of the called function frame pointer.
        new_fp: u32,
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
        param: AllocatedVar,
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
    module: &'a ModuleContext,
    func_idx: u32,
    body: FunctionBody,
) -> wasmparser::Result<Vec<Directive<'a>>> {
    // Tracks the frame stack. Used to calculate arity.
    let (mut tracker, first_explicit_local) =
        StackTracker::new(module, func_idx, body.get_locals_reader()?)?;

    // The fist directives are zeroing the explicit locals.
    let mut directives: Vec<Directive> = (first_explicit_local..tracker.stack_base)
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
    for operator in body.get_operators_reader()? {
        // Match first the control operators, which require special handling.
        match operator? {
            Operator::Block { blockty } => {
                tracker.control_stack.push(Frame {
                    stack_height: tracker.frame_height(blockty),
                    blockty,
                    frame_kind: FrameKind::Block,
                    else_label: None,
                    target_label: labels.next().unwrap(),
                });
            }
            Operator::Loop { blockty } => {
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
                let condition = tracker.stack.pop().unwrap();
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
                tracker.discard_unreachable_and_fix_the_stack();
            }
            Operator::BrIf { relative_depth } => {
                let br_not_taken = labels.next().unwrap();

                let condition = tracker.stack.pop().unwrap();
                assert!(condition.val_type == ValType::I32);

                // Jump to after the br code if the condition is zero (false).
                directives.push(Directive::JumpIfZero {
                    target: br_not_taken,
                    condition,
                });

                directives.extend(tracker.br_code(relative_depth));
                directives.push(Directive::Label(br_not_taken));
            }
            Operator::BrTable { .. } => todo!(),
            Operator::Return => {
                directives.extend(tracker.br_code(tracker.control_stack.len() as u32));
                tracker.discard_unreachable_and_fix_the_stack();
            }
            Operator::Unreachable => todo!(),
            Operator::CallIndirect {
                type_index,
                table_index,
            } => todo!(),
            Operator::Call { function_index } => todo!(),
            op => {
                let op_type = tracker.get_operator_type(&op);
                todo!()
            }
        }

        todo!()
    }

    todo!()
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

struct StackTracker<'a> {
    module: &'a ModuleContext,
    func_type: &'a FuncType,
    locals: Vec<AllocatedVar>,
    /// In the middle of the locals, right after the function arguments, in a place
    /// "call" will be able to write, we have the return address and the frame pointer
    /// of the previous frame, which must be restored by the "return" instruction.
    return_info: AllocatedVar,
    stack: Vec<AllocatedVar>,
    /// We can't pop beyond the stack base.
    /// Before that the locals are allocated.
    stack_base: u32,
    stack_top: u32,
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
        stack_top += 4;

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
                stack: Vec::new(),
                stack_base: stack_top,
                stack_top,
            },
            first_explicit_local,
        ))
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

        self.stack_top - input_size
    }

    /// Generate the code of a break ("br"), ensuring the outputs are at the expected height.
    fn br_code(&self, relative_depth: u32) -> Vec<Directive<'a>> {
        // When breaking, the stack might be bigger than the required height for the target label.
        // If so, we must copy the outputs to the expected height.
        let cs_len = self.control_stack.len();
        assert!(relative_depth <= cs_len as u32);

        let single_arg;
        let (args, jump_directive, target_height) = if relative_depth == cs_len as u32 {
            // The target is the function itself, this is a return.
            (
                self.func_type.results(),
                Directive::Return {
                    return_info: self.return_info,
                },
                self.stack_base,
            )
        } else {
            let target_frame = &self.control_stack[cs_len - relative_depth as usize - 1];

            let args = if target_frame.frame_kind == FrameKind::Loop {
                // Loop is special because br sends the execution to
                // the top of the loop, so the arguments are the inputs.
                match target_frame.blockty {
                    BlockType::Empty | BlockType::Type(_) => &[],
                    BlockType::FuncType(idx) => {
                        let func_type = self.module.get_type(idx);
                        func_type.params()
                    }
                }
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

        let stack = &self.stack;
        assert!(stack.len() >= args.len());
        assert!(stack[stack.len() - args.len()..]
            .iter()
            .zip(args)
            .all(|(stack_var, ty)| stack_var.val_type == *ty));

        // Copy the outputs to the expected height, if needed
        let mut directives = Vec::new();
        if stack[stack.len() - args.len()].address != target_height {
            let src_args = &stack[(stack.len() - args.len())..];

            let dest_pos = stack.partition_point(|v| v.address < target_height);

            let mut dest_stack_top = stack
                .get(dest_pos - 1)
                .map(|v| v.address + sz(v.val_type))
                .unwrap_or(self.stack_base);
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

    /// Some instructions unconditionally divert the control flow, leaving everithing between
    /// themselves and the end of the current block unreachable, and leaving the stack in an
    /// undefined state.
    ///
    /// Call this function after processing such instructions to discard the unreachable code
    /// and fix the stack.
    fn discard_unreachable_and_fix_the_stack(&mut self) {
        todo!()
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
                let choices = &self.stack[(len - 3)..(len - 1)];
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
            Operator::Nop | Operator::Unreachable => (vec![], vec![]),
            Operator::Call { function_index } => {
                let func_type = self.module.get_func_type(*function_index);
                (func_type.params().to_vec(), func_type.results().to_vec())
            }
            Operator::CallIndirect { type_index, .. } => {
                let func_type = self.module.get_type(*type_index);
                (
                    func_type
                        .params()
                        .iter()
                        .cloned()
                        .chain(once(ValType::I32))
                        .collect(),
                    func_type.results().to_vec(),
                )
            }
            // Some control instructions are too complex to be handled here.
            // We return None for them.
            Operator::Block { .. }
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
