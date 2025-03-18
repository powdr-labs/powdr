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

enum Operation<'a> {
    FunctionArgs,
    Inputs,
    WASMOp(Operator<'a>),
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct ValueOrigin {
    node: usize,
    output_idx: u32,
}

struct Node<'a> {
    operation: Operation<'a>,
    inputs: Vec<ValueOrigin>,
    output_count: u32,
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
        let (input_count, output_count) = operator.operator_arity(&module).unwrap();

        // Most operators creates a new node that consumes some inputs and produces
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

        let node = nodes.len();
        nodes.push(Node {
            operation: Operation::WASMOp(operator),
            inputs: stack.split_off(stack.len().checked_sub(input_count as usize).unwrap()),
            output_count,
        });
        stack.extend((0..output_count).map(|output_idx| ValueOrigin { node, output_idx }));
    }

    Ok(())
}

#[derive(Default, Clone)]
struct Local {
    was_written: bool,
    val_ref: Option<ValueOrigin>,
}

// Helper struct with some operations.
struct DagBuilder<'a> {
    nodes: Vec<Node<'a>>,
    stack: Vec<ValueOrigin>,
    locals: Vec<Local>,
    total_locals_len: u32,
    hidden_inputs_map: Vec<u32>,
    hidden_outputs_map: Vec<u32>,
}

impl<'a> DagBuilder<'a> {
    fn read_local(&mut self, local_index: u32) -> ValueOrigin {
        let idx = local_index as usize;

        let local = match self.locals.get_mut(idx) {
            Some(local) => {
                if let Some(val) = local.val_ref {
                    return val;
                }
                local
            }
            None => {
                // Locals is not big enough, we have to grow it.
                assert!(idx < self.total_locals_len as usize);
                self.locals.resize(idx + 1, Default::default());
                &mut self.locals[idx]
            }
        };

        // This is a hidden input
        self.hidden_inputs_map.push(idx as u32);
        let val_ref = ValueOrigin {
            node: 0,
            output_idx: self.nodes[0].output_count,
        };

        self.nodes[0].output_count += 1;
        self.stack.push(val_ref);
        local.val_ref = Some(val_ref);

        val_ref
    }

    fn write_local(&mut self, local_index: u32, val_ref: ValueOrigin) {
        let idx = local_index as usize;
        if idx >= self.locals.len() {
            self.locals.resize(idx + 1, Default::default());
        }

        let local = &mut self.locals[idx];
        if local.val_ref == Some(val_ref) {
            // This write is a nop and can be statically elided.
            return;
        }

        local.val_ref = Some(val_ref);

        // This is a hidden output
        if !local.was_written {
            self.hidden_outputs_map.push(idx as u32);
            local.was_written = true;
        }
    }
}

/// Builds a DAG for a block of instructions.
///
/// The return values are, in order:
/// - The DAGs assembled from the block. One for most blocks, but 2 for if...else blocks.
/// - The local variables this blocks consumes as inputs.
/// - The local variables this blocks produces as outputs.
/// - Whether this block finishes with an unconditional "return" or "unreachable".
fn build_dag_for_block<'a>(
    stack_input: u32,
    local_inputs: u32,
    total_locals_len: u32,
    module: &mut ModuleState,
    operators: &'a mut OperatorsIterator,
) -> wasmparser::Result<(Vec<Vec<Node<'a>>>, Vec<u32>, Vec<u32>, bool)> {
    // We start a block by defining the input node, containing all the known inputs (stack and locals).
    let nodes = vec![Node {
        operation: Operation::Inputs,
        inputs: Vec::new(),
        output_count: stack_input + local_inputs,
    }];

    // Define the stack with the stack inputs
    let stack = (0..stack_input)
        .map(|output_idx| ValueOrigin {
            node: 0,
            output_idx,
        })
        .collect::<Vec<_>>();

    // Define the locals with the local inputs
    let locals = (0..local_inputs)
        .map(|output_idx| Local {
            val_ref: Some(ValueOrigin {
                node: 0,
                output_idx: stack_input + output_idx,
            }),
            was_written: false,
        })
        .collect::<Vec<_>>();

    // If a local that is not an input is read, we consider it as a hidden input.
    // We have to store the local index of this new input.
    let hidden_inputs_map = Vec::new();

    // If any local is written, we consider it as a hidden output.
    // We have to store the local index of this new output.
    let hidden_outputs_map = Vec::new();

    let mut b = DagBuilder {
        nodes,
        stack,
        locals,
        total_locals_len,
        hidden_inputs_map,
        hidden_outputs_map,
    };

    let mut else_nodes = None;

    while let Some(operator) = operators.next() {
        let operator = operator?;
        let (input_count, output_count) = operator.operator_arity(module).unwrap();

        // Most operators creates a new node that consumes some inputs and produces
        // some outputs. But local.* and drop are special that they simply move around
        // the references between the stack and the locals, and can be resolved statically.
        match operator {
            // Special stack and local manipulation operators that don't create nodes:
            Operator::LocalGet { local_index } => {
                // LocalGet simply pushes a local value to the stack, and it can be
                // resolved immediatelly without creating a new node.
                let val_ref = b.read_local(local_index);
                b.stack.push(val_ref);
                continue;
            }
            Operator::LocalSet { local_index } => {
                // LocalSet pops a value from the stack and assigns it to a local.
                // It also can be resolved statically without generating a node.
                let val_ref = b.stack.pop().unwrap();
                b.write_local(local_index, val_ref);
                continue;
            }
            Operator::LocalTee { local_index } => {
                // LocalTee is like LocalSet, but keeps the value on the stack.
                let val_ref = *b.stack.last().unwrap();
                b.write_local(local_index, val_ref);
                continue;
            }
            Operator::Drop => {
                // Drop could be a node that consumes a value and produces nothing,
                // but since it has no side effects, we can just resolve it statically.
                b.stack.pop().unwrap();
                continue;
            }
            // Block operators. They must be recursively handled, and we can't trust
            // the given arity because we consider locals used as a kind of input and output.
            Operator::Block { blockty } => {
                module
                    .control_stack
                    .push((blockty, wasmparser::FrameKind::Block));
                let _ = build_dag_for_block(input_count, 0, total_locals_len, module, operators)?;
                module.control_stack.pop();
            }
            Operator::Loop { blockty } => {
                module
                    .control_stack
                    .push((blockty, wasmparser::FrameKind::Loop));
                let _ = build_dag_for_block(input_count, 0, total_locals_len, module, operators)?;
                module.control_stack.pop();
            }
            Operator::If { blockty } => {
                module
                    .control_stack
                    .push((blockty, wasmparser::FrameKind::If));
                // If's block has one less input than other blocks, because of the condition.
                let _ =
                    build_dag_for_block(input_count - 1, 0, total_locals_len, module, operators)?;
                module.control_stack.pop();
            }
            Operator::Else => {
                // Else block runs at the same height as the if block.
                let last_control = module.control_stack.last_mut().unwrap();
                assert_eq!(last_control.1, wasmparser::FrameKind::If);
                last_control.1 = wasmparser::FrameKind::Else;

                // We parse the block recursivelly and merge the results with this if block.
                let (sub_nodes, sub_hidden_inputs, sub_hidden_outputs, sub_finishes) =
                    build_dag_for_block(input_count, 0, total_locals_len, module, operators)?;

                todo!()
            }
            Operator::End => {
                // End of this current block.
                break;
            }
            // "br", "br_if", "br_table" are special because their inputs might have to be fixed-up
            // to match the hidden outputs found after they have been processed.
            // TODO...
            _ => {}
        }

        // Take the inputs from the stack:
        let inputs = b
            .stack
            .split_off(b.stack.len().checked_sub(input_count as usize).unwrap());

        let node = b.nodes.len();
        b.nodes.push(Node {
            operation: Operation::WASMOp(operator),
            inputs,
            output_count,
        });

        // Push the outputs to the stack:
        b.stack
            .extend((0..output_count).map(|output_idx| ValueOrigin { node, output_idx }));
    }

    // TODO: add an implict "br 0" or "return" (depending on the control stack height) if the
    // last node is not a "br", "br_if", "br_table", "return" or "unreachable".

    // TODO: fixup the "br", "br_if", "br_table" nodes to match the hidden outputs.

    // TODO: organize the output.
    Ok(())
}

fn read_locals<'a>(
    func_type: &FuncType,
    body: &FunctionBody,
) -> wasmparser::Result<(Vec<Node<'a>>, Vec<ValueOrigin>)> {
    // The locals are the function arguments and the explicit locals declaration.

    // Function arguments originates from the special FunctionArgs node.
    let args = Node {
        operation: Operation::FunctionArgs,
        inputs: Vec::new(),
        output_count: func_type.params().len() as u32,
    };

    let mut locals: Vec<ValueOrigin> = (0..args.output_count)
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
                output_count: 1,
            });
            locals.push(ValueOrigin {
                node: nodes.len() - 1,
                output_idx: 0,
            });
        }
    }

    Ok((nodes, locals))
}

fn default_for_type<'a>(value_type: ValType) -> Operation<'a> {
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
