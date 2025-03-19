use std::mem::MaybeUninit;

use wasmparser::{
    BlockType, CompositeInnerType, ContType, FuncType, FunctionBody, ModuleArity, Operator,
    OperatorsIterator, Parser, Payload, RefType, SubType, ValType,
};

fn main() -> wasmparser::Result<()> {
    // TODO: do proper command line argument parsing
    let args: Vec<String> = std::env::args().collect();
    let wasm_file = std::fs::read(&args[1]).unwrap();

    let parser = Parser::new(0);

    let mut types = Vec::new();
    let mut func_types = Vec::new();
    let mut func_idx = 0;

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
                    types.push(ty);
                }
            }
            Payload::FunctionSection(section) => {
                for ty in section.into_iter() {
                    func_types.push(ty?);
                }
            }
            Payload::CodeSectionEntry(function) => {
                infinite_registers_allocation(&types, &func_types, func_idx, function)?;
                func_idx += 1;
            }
            _ => {
                // uninteresting section
            }
        }
    }

    Ok(())
}

fn get_func_type(subtype: &SubType) -> &FuncType {
    match &subtype.composite_type.inner {
        CompositeInnerType::Func(f) => f,
        _ => panic!("gc proposal not supported"),
    }
}

struct ModuleState<'a> {
    types: &'a [SubType],
    func_types: &'a [u32],
    control_stack: Vec<(wasmparser::BlockType, wasmparser::FrameKind)>,
}

impl<'a> ModuleArity for ModuleState<'a> {
    fn sub_type_at(&self, type_idx: u32) -> Option<&SubType> {
        self.types.get(type_idx as usize)
    }

    fn tag_type_arity(&self, at: u32) -> Option<(u32, u32)> {
        panic!("exception handling proposal not supported")
    }

    fn type_index_of_function(&self, function_idx: u32) -> Option<u32> {
        self.func_types.get(function_idx as usize).copied()
    }

    fn func_type_of_cont_type(&self, c: &ContType) -> Option<&FuncType> {
        panic!("continuations proposal not supported")
    }

    fn sub_type_of_ref_type(&self, rt: &RefType) -> Option<&SubType> {
        panic!("gc proposal not supported")
    }

    fn control_stack_height(&self) -> u32 {
        self.control_stack.len() as u32
    }

    fn label_block(&self, depth: u32) -> Option<(wasmparser::BlockType, wasmparser::FrameKind)> {
        self.control_stack.get(depth as usize).copied()
    }
}

struct AllocatedVar {
    val_type: ValType,
    address: u32,
}

enum Directive {}

fn infinite_registers_allocation(
    types: &[SubType],
    func_types: &[u32],
    func_idx: u32,
    body: FunctionBody,
) -> wasmparser::Result<Vec<Directive>> {
    // This function allocates the locals and the stack at addresses starting
    // from 0, assuming one byte per address.

    // We start by reading the input and local variables.
    let func_type = get_func_type(&types[func_types[func_idx as usize] as usize]);
    let (mut locals, mut stack_top) = read_locals(func_type, &body)?;

    // We can't pop beyond the stack base.
    let stack_base = stack_top;
    let mut stack = Vec::new();

    // Tracks the frame stack. Used to calculate arity.
    let module = ModuleState {
        types,
        func_types,
        control_stack: Vec::new(),
    };

    for operator in body.get_operators_reader()? {
        let operator = operator?;

        let (inputs, outputs) = get_operator_type(&operator, &stack);

        todo!()
    }

    todo!()
}

/// Returns the list of input types and output types of an operator.
fn get_operator_type(op: &Operator, stack: &[ValType]) -> (Vec<ValType>, Vec<ValType>) {
    match op {
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
            let ValType::Ref(ref_type) = stack.last().unwrap() else {
                panic!("ref.is_null expects a reference type")
            };
            assert!(ref_type.is_func_ref() || ref_type.is_extern_ref());
            (vec![ValType::Ref(*ref_type)], vec![ValType::I32])
        }
        Operator::RefFunc { .. } => (vec![], vec![ValType::Ref(RefType::FUNCREF)]),

        // TODO: # Vector instructions
        // lets skip vector instructions for now, as we can switch it off at LLVM...

        // # Parametric instructions
        Operator::Drop => (vec![*stack.last().unwrap()], vec![]),
        Operator::Select => {
            let len = stack.len();
            let choices = &stack[(len - 3)..(len - 1)];
            assert_eq!(choices[0], choices[1]);
            (vec![choices[0], choices[1], ValType::I32], vec![choices[0]])
        }

        // # Variable instructions
        _ => todo!(),
    }
}

fn read_locals<'a>(
    func_type: &FuncType,
    body: &FunctionBody,
) -> wasmparser::Result<(Vec<AllocatedVar>, u32)> {
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
    for local in body.get_locals_reader()? {
        let (count, val_type) = local?;
        for _ in 0..count {
            locals.push(AllocatedVar {
                val_type,
                address: stack_top,
            });
            stack_top += sz(val_type);
        }
    }

    Ok((locals, stack_top))
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
