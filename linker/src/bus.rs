use powdr_ast::{
    asm_analysis::{combine_flags, MachineDegree},
    object::{Link, Location, MachineInstanceGraph, Object, Operation},
    parsed::{
        asm::SymbolPath,
        build::{index_access, namespaced_reference},
        ArrayLiteral, Expression, FunctionCall, NamespaceDegree, PilStatement,
    },
};
use powdr_parser_util::SourceRef;
use std::{
    collections::BTreeMap,
    hash::{DefaultHasher, Hash, Hasher},
    iter::once,
    str::FromStr,
};

use crate::{DegreeMode, LinkerBackend};

impl LinkerBackend for Linker {
    fn process_link(
        &mut self,
        link: Link,
        from_namespace: String,
        objects: &BTreeMap<Location, Object>,
    ) {
        let from = link.from;
        let to = link.to;
        let operation = &objects[&to.machine].operations[&to.operation];

        let interaction_id = interaction_id(&to.machine.to_string(), &to.operation);

        let op_id = operation.id.clone().unwrap().into();

        // lhs is `flag { operation_id, inputs, outputs }`
        let selector = combine_flags(from.instr_flag, from.link_flag);
        let tuple = ArrayLiteral {
            items: once(op_id)
                .chain(from.params.inputs)
                .chain(from.params.outputs)
                .collect(),
        }
        .into();

        self.insert_interaction(interaction_id, from_namespace, selector, tuple);
    }

    fn process_object(
        &mut self,
        location: &Location,
        object: Object,
        objects: &BTreeMap<Location, Object>,
    ) {
        let namespace_degree = try_into_namespace_degree(object.degree)
            .unwrap_or_else(|| panic!("machine at {location} must have an explicit degree"));

        let namespace = location.to_string();

        let (pil, _) = self.namespaces.entry(namespace.clone()).or_default();

        // create a namespace for this object
        pil.push(PilStatement::Namespace(
            SourceRef::unknown(),
            SymbolPath::from_identifier(namespace.clone()),
            Some(namespace_degree),
        ));

        pil.extend(object.pil);
        // process outgoing links
        for link in object.links {
            self.process_link(link, namespace.clone(), objects);
        }
        // receive incoming links
        // TODO: are unused operations removed?
        for (operation_name, operation) in object.operations {
            self.process_operation(
                operation_name,
                operation,
                namespace.clone(),
                object.latch.as_ref().unwrap(),
                object.operation_id.as_ref().unwrap().clone(),
            );
        }
    }

    fn add_to_namespace_links(&mut self, namespace: &Location, statement: PilStatement) {
        self.namespaces
            .entry(namespace.to_string())
            .or_default()
            .0
            .push(statement);
    }

    fn try_new(_: &MachineInstanceGraph, degree_mode: DegreeMode) -> Result<Self, Vec<String>> {
        match degree_mode {
            DegreeMode::Monolithic => {
                Err(vec!["Monolithic degree mode is not supported".to_string()])
            }
            DegreeMode::Vadcop => Ok(Self::default()),
        }
    }

    fn into_pil(self) -> Vec<PilStatement> {
        self.namespaces
            .into_iter()
            .flat_map(|(_, (statements, links))| statements.into_iter().chain(links))
            .collect()
    }
}

fn interaction_id(machine: &str, operation: &String) -> u32 {
    let mut hasher = DefaultHasher::new();
    format!("{machine}/{operation}").hash(&mut hasher);
    hasher.finish() as u32
}

#[derive(Default)]
pub struct Linker {
    /// for each namespace, we store the statements resulting from processing the links separately, because we need to make sure they do not come first.
    namespaces: BTreeMap<String, (Vec<PilStatement>, Vec<PilStatement>)>,
}

impl Linker {
    fn process_operation(
        &mut self,
        operation_name: String,
        operation: Operation,
        namespace: String,
        latch: &str,
        operation_id: String,
    ) {
        // TODO: we need to know if the operation is accessed via lookup or permutation!
        // we assume it's accessed via lookup for now, need to do a first pass on links and error out if mixed
        // or is mixed somehow okay, by receiving both lookup and permutation?

        let interaction_ty = InteractionType::Lookup;

        let interaction_id = interaction_id(&namespace, &operation_name);

        self.namespaces
            .entry(namespace.clone())
            .or_default()
            .1
            .push(PilStatement::Expression(
                SourceRef::unknown(),
                receive(
                    interaction_ty,
                    namespaced_reference(namespace.clone(), latch),
                    ArrayLiteral {
                        items: once(namespaced_reference(namespace.clone(), operation_id))
                            .chain(operation.params.inputs_and_outputs().map(|i| {
                                index_access(
                                    namespaced_reference(namespace.clone(), &i.name),
                                    i.index.clone(),
                                )
                            }))
                            .collect(),
                    }
                    .into(),
                    namespaced_reference(namespace.clone(), latch),
                    interaction_id,
                ),
            ));
    }

    fn insert_interaction(
        &mut self,
        interaction_id: u32,
        namespace: String,
        selector: Expression,
        tuple: Expression,
    ) {
        self.namespaces
            .entry(namespace.clone())
            .or_default()
            .1
            .push(PilStatement::Expression(
                SourceRef::unknown(),
                Expression::FunctionCall(
                    SourceRef::unknown(),
                    FunctionCall {
                        function: Box::new(Expression::Reference(
                            SourceRef::unknown(),
                            SymbolPath::from_str("std::protocols::bus::bus_send")
                                .unwrap()
                                .into(),
                        )),
                        arguments: vec![interaction_id.into(), tuple, selector],
                    },
                ),
            ));
    }
}

#[derive(Clone, Copy)]
enum InteractionType {
    Lookup,
    #[allow(dead_code)]
    Permutation,
}

fn receive(
    identity_type: InteractionType,
    selector: Expression,
    tuple: Expression,
    latch: Expression,
    interaction_id: u32,
) -> Expression {
    let function = match identity_type {
        InteractionType::Lookup => {
            SymbolPath::from_str("std::protocols::lookup_via_bus::lookup_receive")
                .unwrap()
                .into()
        }
        InteractionType::Permutation => {
            SymbolPath::from_str("std::protocols::permutation_via_bus::permutation_receive")
                .unwrap()
                .into()
        }
    };

    Expression::FunctionCall(
        SourceRef::unknown(),
        FunctionCall {
            function: Box::new(Expression::Reference(SourceRef::unknown(), function)),
            arguments: vec![interaction_id.into(), selector, tuple, latch],
        },
    )
}

/// Convert a [MachineDegree] into a [NamespaceDegree]
fn try_into_namespace_degree(d: MachineDegree) -> Option<NamespaceDegree> {
    let min = d.min?;
    let max = d.max?;
    Some(NamespaceDegree { min, max })
}

#[cfg(test)]
mod test {
    use std::{fs, path::PathBuf};

    use powdr_ast::object::MachineInstanceGraph;
    use powdr_number::{FieldElement, GoldilocksField};

    use powdr_analysis::convert_asm_to_pil;
    use powdr_parser::parse_asm;

    use pretty_assertions::assert_eq;

    use crate::LinkerBackend;

    use super::Linker;

    fn parse_analyze_and_compile_file<T: FieldElement>(file: &str) -> MachineInstanceGraph {
        let contents = fs::read_to_string(file).unwrap();
        let parsed = parse_asm(Some(file), &contents).unwrap_or_else(|e| {
            e.output_to_stderr();
            panic!();
        });
        let resolved =
            powdr_importer::load_dependencies_and_resolve(Some(PathBuf::from(file)), parsed)
                .unwrap();
        powdr_airgen::compile(convert_asm_to_pil::<T>(resolved).unwrap())
    }

    fn extract_main(code: &str) -> &str {
        let start = code.find("namespace main").unwrap();
        &code[start..]
    }

    #[test]
    fn compile_empty_vm() {
        let expectation = r#"namespace main(8);
    let _operation_id;
    query |__i| std::prover::provide_if_unknown(_operation_id, __i, || 2);
    pol constant _block_enforcer_last_step = [0]* + [1];
    let _operation_id_no_change = (1 - _block_enforcer_last_step) * (1 - instr_return);
    _operation_id_no_change * (_operation_id' - _operation_id) = 0;
    pol commit pc;
    pol commit instr__jump_to_operation;
    pol commit instr__reset;
    pol commit instr__loop;
    pol commit instr_return;
    pol constant first_step = [1] + [0]*;
    pol commit pc_update;
    pc_update = instr__jump_to_operation * _operation_id + instr__loop * pc + instr_return * 0 + (1 - (instr__jump_to_operation + instr__loop + instr_return)) * (pc + 1);
    pc' = (1 - first_step') * pc_update;
    std::protocols::bus::bus_send(2981556482, [0, pc, instr__jump_to_operation, instr__reset, instr__loop, instr_return], 1);
namespace main__rom(4);
    pol constant p_line = [0, 1, 2] + [2]*;
    pol constant p_instr__jump_to_operation = [0, 1, 0] + [0]*;
    pol constant p_instr__loop = [0, 0, 1] + [1]*;
    pol constant p_instr__reset = [1, 0, 0] + [0]*;
    pol constant p_instr_return = [0]*;
    pol constant operation_id = [0]*;
    pol constant latch = [1]*;
    std::protocols::lookup_via_bus::lookup_receive(2981556482, main__rom::latch, [main__rom::operation_id, main__rom::p_line, main__rom::p_instr__jump_to_operation, main__rom::p_instr__reset, main__rom::p_instr__loop, main__rom::p_instr_return], main__rom::latch);
"#;

        let file_name = "../test_data/asm/empty_vm.asm";
        let graph = parse_analyze_and_compile_file::<GoldilocksField>(file_name);
        let pil = Linker::link(graph, crate::DegreeMode::Vadcop).unwrap();
        assert_eq!(extract_main(&format!("{pil}")), expectation);
    }
}
