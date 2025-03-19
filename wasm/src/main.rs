use std::iter::once;

use wasmparser::{
    CompositeInnerType, FuncType, FunctionBody, LocalsReader, Operator, Parser, Payload, RefType,
    SubType, TableType, ValType,
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

struct AllocatedVar {
    val_type: ValType,
    /// If it is a local or stack, this address is relative to the stack base.
    /// If it is a global, this address is absolute.
    address: u32,
}

enum Directive {}

/// Allocates the locals and the stack at addresses starting
/// from 0, assuming one byte per address.
fn infinite_registers_allocation(
    module: &ModuleContext,
    func_idx: u32,
    body: FunctionBody,
) -> wasmparser::Result<Vec<Directive>> {
    // Tracks the frame stack. Used to calculate arity.
    let mut tracker = StackTracker::new(module, func_idx, body.get_locals_reader()?)?;

    for operator in body.get_operators_reader()? {
        let operator = operator?;

        let op_type = tracker.get_operator_type(&operator);

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

struct StackTracker<'a> {
    module: &'a ModuleContext,
    locals: Vec<AllocatedVar>,
    stack: Vec<ValType>,
    /// We can't pop beyond the stack base.
    /// Before that the locals are allocated.
    stack_base: u32,
    stack_top: u32,
    control_stack: Vec<(wasmparser::BlockType, wasmparser::FrameKind)>,
}

impl<'a> StackTracker<'a> {
    fn new(
        module: &'a ModuleContext,
        func_idx: u32,
        locals_reader: LocalsReader<'a>,
    ) -> wasmparser::Result<Self> {
        // We start by reading the input and local variables.
        let func_type = module.get_func_type(func_idx);

        // The locals are the function arguments and the explicit locals declaration.
        let mut stack_top = 0;
        let mut locals = Vec::new();

        // Function arguments are the first locals.
        for &val_type in func_type.params() {
            locals.push(AllocatedVar {
                val_type,
                address: stack_top,
            });
            stack_top += sz(val_type);
        }

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

        Ok(StackTracker {
            module,
            control_stack: Vec::new(),
            locals,
            stack: Vec::new(),
            stack_base: stack_top,
            stack_top,
        })
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
                let ValType::Ref(ref_type) = self.stack.last().unwrap() else {
                    panic!("ref.is_null expects a reference type")
                };
                assert!(ref_type.is_func_ref() || ref_type.is_extern_ref());
                (vec![ValType::Ref(*ref_type)], vec![ValType::I32])
            }
            Operator::RefFunc { .. } => (vec![], vec![ValType::Ref(RefType::FUNCREF)]),

            // TODO: # Vector instructions
            // lets skip vector instructions for now, as we can switch it off at LLVM...

            // # Parametric instructions
            Operator::Drop => (vec![*self.stack.last().unwrap()], vec![]),
            Operator::Select => {
                let len = self.stack.len();
                let choices = &self.stack[(len - 3)..(len - 1)];
                assert_eq!(choices[0], choices[1]);
                (vec![choices[0], choices[1], ValType::I32], vec![choices[0]])
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

            _ => todo!(),
        };

        Some(ty)
    }
}
