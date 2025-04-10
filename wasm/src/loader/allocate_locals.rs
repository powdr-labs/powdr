use std::{iter::Peekable, ops::RangeFrom};

use itertools::Itertools;
use powdr_syscalls::Syscall;
use wasmparser::{
    BlockType, ContType, FuncType, FunctionBody, LocalsReader, ModuleArity, Operator,
    OperatorsIterator, RefType, SubType, ValType,
};

use crate::loader::{sz, ModuleContext};

use super::many_sz;

#[derive(Clone, Copy)]
pub struct AllocatedVar {
    pub val_type: ValType,
    /// If it is a local or stack, this address is relative to the stack base.
    /// If it is a global, this address is absolute.
    pub address: u32,
}

pub enum Directive<'a> {
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
    IndirectCall {
        /// The table to look for the function reference.
        table_index: u32,
        /// The index of the function referece to be called in the table.
        function_ref_index: AllocatedVar,
        /// The index of the expected function type.
        func_type_index: u32,
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
        log::trace!("New stack");
        log::trace!("## {stack_base_bytes} |  | {stack_base_bytes}");
        Stack {
            stack: Vec::new(),
            stack_base_bytes,
            stack_top_bytes: stack_base_bytes,
        }
    }

    fn push(&mut self, val_type: ValType) {
        let old_len = self.stack.len();
        let address = self.stack_top_bytes;
        self.stack.push(AllocatedVar { val_type, address });
        self.stack_top_bytes += sz(val_type);
        log::trace!(
            "## {} | {} <= {address} | {}",
            self.stack_base_bytes,
            self.stack[..old_len]
                .iter()
                .map(|var| var.address)
                .format(", "),
            self.stack_top_bytes
        );
    }

    fn pop(&mut self) -> AllocatedVar {
        let var = self.stack.pop().unwrap();
        self.stack_top_bytes -= sz(var.val_type);
        assert_eq!(self.stack_top_bytes, var.address);
        assert!(self.stack_top_bytes >= self.stack_base_bytes);

        log::trace!(
            "## {} | {} | {} => {}",
            self.stack_base_bytes,
            self.stack[..self.stack.len()]
                .iter()
                .map(|var| var.address)
                .format(", "),
            self.stack_top_bytes,
            var.address,
        );

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

            log::trace!(
                "## {} | {} | {} => {}",
                self.stack_base_bytes,
                self.stack.iter().map(|var| var.address).format(", "),
                self.stack_top_bytes,
                vars.iter().map(|var| var.address).format(", "),
            );
        }
        vars
    }

    fn drop_n_from_top(&mut self, count: usize) {
        let new_len = self.stack.len() - count;
        if let Some(&var) = self.stack.get(new_len) {
            self.stack_top_bytes = var.address;
            assert!(self.stack_top_bytes >= self.stack_base_bytes);

            log::trace!(
                "## {} | {} | {} => {}",
                self.stack_base_bytes,
                self.stack[..new_len]
                    .iter()
                    .map(|var| var.address)
                    .format(", "),
                self.stack_top_bytes,
                self.stack[new_len..]
                    .iter()
                    .map(|var| var.address)
                    .format(", "),
            );

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
    module: &'a ModuleContext<'a>,
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
                control_stack: vec![Frame {
                    stack_height: 0,
                    blockty: BlockType::FuncType(module.func_types[func_idx as usize]),
                    frame_kind: FrameKind::Function,
                }],
                locals,
                return_info,
                stack: Stack::new(stack_top),
            },
            first_explicit_local,
        ))
    }

    /// Where to save the return info right before calling a function.
    fn next_return_info_address(&self, bottom_addr: u32, func_type: &FuncType) -> AllocatedVar {
        // Return info is written on the stack, after ther function inputs or outputs,
        // whichever is bigger.
        let address = self
            .stack
            .top_bytes()
            .max(bottom_addr + many_sz(func_type.results()));

        AllocatedVar {
            // For the lack of a better type, the return info is a i64, where
            // the first 32 bits (address-wise) are the saved frame pointer, and the
            // other 32 bits are the return address.
            val_type: ValType::I64,
            address,
        }
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

        // Copy the outputs to the expected height, if needed.
        let stack = self.stack.slice();
        if stack.len() > args.len() {
            let src_args = &stack[(stack.len() - args.len())..];

            let mut dest_stack_top = target_frame.stack_height;
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
        op_reader: &mut Peekable<OperatorsIterator<'_>>,
    ) -> wasmparser::Result<()> {
        // Discard unreachable code
        let mut stack_count = 0;

        // We do a peek loop so we leave Else and End operators in the iterator,
        // to be handled by the caller.
        while let Some(operator) = op_reader.peek() {
            match operator {
                Ok(Operator::Block { .. })
                | Ok(Operator::Loop { .. })
                | Ok(Operator::If { .. }) => {
                    stack_count += 1;
                }
                Ok(Operator::Else) => {
                    if stack_count == 0 {
                        break;
                    }
                }
                Ok(Operator::End) => {
                    if stack_count == 0 {
                        break;
                    }
                    stack_count -= 1;
                }
                Ok(_) => {}
                Err(_) => {
                    return Err(op_reader.next().unwrap().unwrap_err());
                }
            }
            op_reader.next();
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
                let global = &self.module.p.globals[*global_index as usize];
                (vec![], Some(global.val_type))
            }
            Operator::GlobalSet { global_index } => {
                let global = &self.module.p.globals[*global_index as usize];
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

impl ModuleArity for StackTracker<'_> {
    fn sub_type_at(&self, type_idx: u32) -> Option<&SubType> {
        self.module.types.get(type_idx as usize)
    }

    fn tag_type_arity(&self, _at: u32) -> Option<(u32, u32)> {
        panic!("exception handling proposal not supported")
    }

    fn type_index_of_function(&self, function_idx: u32) -> Option<u32> {
        self.module.func_types.get(function_idx as usize).copied()
    }

    fn func_type_of_cont_type(&self, _c: &ContType) -> Option<&FuncType> {
        panic!("continuations proposal not supported")
    }

    fn sub_type_of_ref_type(&self, _rt: &RefType) -> Option<&SubType> {
        panic!("gc proposal not supported")
    }

    fn control_stack_height(&self) -> u32 {
        self.control_stack.len() as u32
    }

    fn label_block(&self, depth: u32) -> Option<(wasmparser::BlockType, wasmparser::FrameKind)> {
        self.control_stack.get(depth as usize + 1).map(|frame| {
            (
                frame.blockty,
                match frame.frame_kind {
                    FrameKind::Block { .. } => wasmparser::FrameKind::Block,
                    FrameKind::Loop { .. } => wasmparser::FrameKind::Loop,
                    FrameKind::If { .. } => wasmparser::FrameKind::If,
                    FrameKind::Else { .. } => wasmparser::FrameKind::Else,
                    _ => unreachable!(),
                },
            )
        })
    }
}

/// Allocates the locals and the stack at addresses starting
/// from 0, assuming one byte per address.
pub fn infinite_registers_allocation<'a>(
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
    let mut op_reader = body.get_operators_reader()?.into_iter().peekable();
    while let Some(operator) = op_reader.next() {
        // There shouldn't be any more operators after the outmost
        // block (the function itself) has ended.
        assert!(!tracker.control_stack.is_empty());

        let operator = operator?;
        log::debug!("# {operator:?}");

        // Match first the control operators, which require special handling.
        match operator {
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
                let last_frame = tracker.control_stack.last().unwrap();
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
                tracker.control_stack.pop().unwrap();
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
                    directives.push(Directive::Call {
                        function_index,
                        new_fp_delta: bottom_addr,
                        save_return_info_to: tracker
                            .next_return_info_address(bottom_addr, func_type),
                    });
                }
            }
            Operator::CallIndirect {
                type_index,
                table_index,
            } => {
                // Consume the function index from the stack. It comes before the actual
                // function arguments.
                let function_ref_index = tracker.stack.pop();
                assert_eq!(function_ref_index.val_type, ValType::I32);

                // Consume the function arguments and place the outputs on the stack.
                let func_type = module.get_type(type_index);
                let (bottom_addr, _, _) =
                    tracker.apply_operation_to_stack(func_type.params(), func_type.results());

                // Emit the call
                directives.push(Directive::IndirectCall {
                    new_fp_delta: bottom_addr,
                    func_type_index: type_index,
                    table_index,
                    function_ref_index,
                    save_return_info_to: tracker.next_return_info_address(bottom_addr, func_type),
                });
            }
            op => {
                let (inputs, output) = tracker.get_operator_type(&op).unwrap();

                // Sanity check with wasmparser that we agree on the number of inputs and outputs.
                let Some((input_count, output_count)) = op.operator_arity(&tracker) else {
                    panic!("wasmparser could not determine the arity of {op:?}");
                };
                assert_eq!(inputs.len(), input_count as usize);
                assert_eq!(output.map_or(0, |_| 1), output_count);

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
    assert!(tracker.control_stack.is_empty());

    Ok(directives)
}
