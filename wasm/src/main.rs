use wasmparser::{
    types, CompositeInnerType, ContType, FuncType, FunctionBody, LocalsReader, ModuleArity, Parser,
    Payload, RefType, SubType, ValType,
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
    Const(u32),
}

struct ValueOrigin {
    node: usize,
    output_idx: usize,
}

struct Node {
    operation: Operation,
    inputs: Vec<ValueOrigin>,
    outputs: Vec<ValType>,
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

        // TODO: to be continued...
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
                operation: Operation::Const(0),
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
