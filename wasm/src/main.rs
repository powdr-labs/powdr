use std::mem::MaybeUninit;

use wasmparser::{
    CompositeInnerType, ContType, FuncType, FunctionBody, ModuleArity, Operator, Parser, Payload,
    RefType, SubType, ValType,
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
                            // Apparently WebAssembly 2.0 is much more complicated, and has complex
                            // type definitions, and garbage collector, and exceptions. We should probably
                            // stick to the 1.0 version for Powdr.
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

enum Operation {
    FunctionArgs,
    WASMOp(Operator<'static>),
}

#[derive(Clone, Copy)]
struct ValueOrigin {
    node: usize,
    output_idx: usize,
}

struct Node {
    operation: Operation,
    inputs: Vec<ValueOrigin>,
    outputs: Vec<ValType>,
    // branch_target: Option<usize>,
}

fn infinite_registers_allocation(
    types: &[SubType],
    func_types: &[u32],
    func_idx: u32,
    body: FunctionBody,
) -> wasmparser::Result<()> {
    // Lets assemble a kind of directed graph, where the nodes are the operations that
    // can input and output values, and the edges are the values. It is directed in a
    // sense that variables have one origin and multiple destinations.

    // We start by reading the input and local variables.
    let func_type = get_func_type(&types[func_types[func_idx as usize] as usize]);
    let (mut nodes, mut locals) = read_locals(func_type, &body)?;

    // Now we follow the instructions in the function body.
    // For that we need to keep track of the stack and of which of our hypergraph edge is
    // currently each local variable. When a local is assigned, it becames a new edge.
    //
    // It is a little tricky with blocks, because we only know if a local is either read or
    // written after we have parsed the block. If a local is read, it becames a block input.
    // If it a block is written, it becames a block output.
    let mut stack: Vec<ValueOrigin> = Vec::new();

    let module = ModuleState {
        types,
        func_types,
        control_stack: Vec::new(),
    };

    for operator in body.get_operators_reader()? {
        let operator = operator?;
        let arity = operator.operator_arity(&module);

        // Most operators simply creates a new node that consumes some inputs and produces
        // some outputs. But local.* and drop are special that they simply move around
        // the references between the stack and the locals, and can be resolved statically.
        match operator {
            // Special stack and local manipulation operators that don't create nodes:
            Operator::LocalGet { local_index } => {
                // LocalGet simply pushes a local value to the stack, and it can be
                // resolved immediatelly without creating a new node.
                let val_ref = locals[local_index as usize];
                stack.push(val_ref);
                continue;
            }
            Operator::LocalSet { local_index } => {
                // LocalSet pops a value from the stack and assigns it to a local.
                // It also can be resolved statically without generating a node.
                let val_ref = stack.pop().unwrap();
                locals[local_index as usize] = val_ref;
                continue;
            }
            Operator::LocalTee { local_index } => {
                // LocalTee is like LocalSet, but keeps the value on the stack.
                let val_ref = *stack.last().unwrap();
                locals[local_index as usize] = val_ref;
                continue;
            }
            Operator::Drop => {
                // Drop could be a node that consumes a value and produces nothing,
                // but since it has no side effects, we can just resolve it statically.
                stack.pop().unwrap();
                continue;
            }
            // TODO: handle blocks
            // Regular operators that create nodes:
            _ => {}
        }

        // TODO: handle regular operators
    }

    Ok(())
}

fn read_locals(
    func_type: &FuncType,
    body: &FunctionBody,
) -> wasmparser::Result<(Vec<Node>, Vec<ValueOrigin>)> {
    // The locals are the function arguments and the explicit locals declaration.

    // Function arguments originates from the special FunctionArgs node.
    let args = Node {
        operation: Operation::FunctionArgs,
        inputs: Vec::new(),
        outputs: func_type.params().into(),
    };

    let mut locals: Vec<ValueOrigin> = (0..args.outputs.len())
        .map(|output_idx| ValueOrigin {
            node: 0,
            output_idx,
        })
        .collect();

    let mut nodes = vec![args];

    // Explicit locals are just const 0 operations.
    for local in body.get_locals_reader()? {
        let (count, value_type) = local?;
        for _ in 0..count {
            nodes.push(Node {
                operation: default_for_type(value_type),
                inputs: Vec::new(),
                outputs: vec![value_type],
            });
            locals.push(ValueOrigin {
                node: nodes.len() - 1,
                output_idx: 0,
            });
        }
    }

    Ok((nodes, locals))
}

fn default_for_type(value_type: ValType) -> Operation {
    match value_type {
        ValType::I32 => Operation::WASMOp(Operator::I32Const { value: 0 }),
        ValType::I64 => Operation::WASMOp(Operator::I64Const { value: 0 }),
        ValType::F32 => Operation::WASMOp(Operator::F32Const { value: 0.0.into() }),
        ValType::F64 => Operation::WASMOp(Operator::F64Const { value: 0.0.into() }),
        ValType::V128 => Operation::WASMOp(Operator::V128Const {
            // Apparently there is no way to instantiate a V128 value.
            // TODO: Fix this when issue is resolved: https://github.com/bytecodealliance/wasm-tools/issues/2101
            value: unsafe { MaybeUninit::zeroed().assume_init() },
        }),
        ValType::Ref(ref_type) => Operation::WASMOp(Operator::RefNull {
            hty: ref_type.heap_type(),
        }),
    }
}
