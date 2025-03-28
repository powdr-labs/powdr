use itertools::Itertools;
use powdr_analysis::utils::parse_pil_statement;
use powdr_ast::{
    asm_analysis::combine_flags,
    object::{Link, LinkTo, Location, MachineInstanceGraph, Object, Operation},
    parsed::{
        asm::SymbolPath,
        build::{index_access, namespaced_reference},
        ArrayLiteral, Expression, FunctionCall, NamespacedPolynomialReference, PilStatement,
    },
};
use powdr_parser_util::SourceRef;
use std::{
    collections::{BTreeMap, HashSet},
    str::FromStr,
};

use crate::{call, try_into_namespace_degree, DegreeMode, LinkerBackend, MAIN_OPERATION_NAME};

pub struct BusLinker {
    /// the pil statements
    pil: Vec<PilStatement>,
    /// for each machine instance, the size of the selector array, based on the number of used operations accessed via permutation
    selector_array_size_by_instance: BTreeMap<Location, usize>,
    /// for each used operation, the index in the selector array. For operations accessed via lookups, this is None.
    selector_array_index_by_operation: BTreeMap<LinkTo, Option<usize>>,
    /// arguments for `bus_multi_linker`
    bus_multi_linker_args: ArrayLiteral,
    /// interaction id map for each link.to
    interaction_id_map: BTreeMap<LinkTo, u32>,
}

impl LinkerBackend for BusLinker {
    fn try_new(graph: &MachineInstanceGraph, degree_mode: DegreeMode) -> Result<Self, Vec<String>> {
        match degree_mode {
            DegreeMode::Monolithic => {
                Err(vec!["Monolithic degree mode is not supported".to_string()])
            }
            DegreeMode::Vadcop => Ok(()),
        }?;

        // generate the maps of selector array sizes and indices
        let (selector_array_index_by_operation, selector_array_size_by_instance) = graph
            .objects
            .values()
            // go through all calls
            .flat_map(|object| object.links.iter())
            // keep only the unique link destinations
            .unique_by(|link| (&link.to, link.is_permutation))
            // check that the same operation is not called both through lookup and permutation
            .scan(Default::default(), |state: &mut HashSet<_>, link| {
                let existing_value = state.insert(&link.to);
                assert!(
                    existing_value,
                    "Operation should not be called both as a lookup and a permutation"
                );
                Some(link)
            })
            .fold(
                (BTreeMap::new(), BTreeMap::new()),
                |(mut indices, mut sizes), link| {
                    let to_insert = link.is_permutation.then(|| {
                        // get the current size of the array
                        let size = sizes.entry(link.to.machine.clone()).or_default();
                        // that is the index of the next selector
                        let index = *size;
                        // increment the size of the array
                        *size += 1;
                        // return the index
                        index
                    });

                    assert!(indices.insert(link.to.clone(), to_insert).is_none());

                    (indices, sizes)
                },
            );
        // generate a map of interaction id for each link.to
        let interaction_id_map: BTreeMap<LinkTo, u32> = graph
            .objects
            .iter()
            .flat_map(|(location, object)| {
                object
                    .operations
                    .keys()
                    .map(move |operation_name| (location, operation_name.clone()))
            })
            .enumerate()
            .map(|(id, (location, operation_name))| {
                (
                    LinkTo {
                        machine: location.clone(),
                        operation: operation_name.clone(),
                    },
                    id as u32,
                )
            })
            .collect();

        Ok(Self {
            pil: Default::default(),
            selector_array_size_by_instance,
            selector_array_index_by_operation,
            bus_multi_linker_args: ArrayLiteral {
                items: Default::default(),
            },
            interaction_id_map,
        })
    }

    fn process_link(&mut self, link: &Link, _: &Location, objects: &BTreeMap<Location, Object>) {
        let from = &link.from;
        let to = &link.to;

        let operation = &objects[&to.machine].operations[&to.operation];

        let interaction_id = self.interaction_id_map[to];

        let op_id = operation.id.clone().map(|operation_id| operation_id.into());

        // we send `flag $ { operation_id, inputs, outputs }`
        let selector = combine_flags(from.instr_flag.clone(), from.link_flag.clone());
        let tuple = ArrayLiteral {
            items: op_id
                .into_iter()
                .chain(from.params.inputs.clone())
                .chain(from.params.outputs.clone())
                .collect(),
        }
        .into();

        let bus_linker_type = NamespacedPolynomialReference::from(
            SymbolPath::from_str("std::protocols::bus::BusLinkerType::Send").unwrap(),
        )
        .into();

        self.bus_multi_linker_args.items.push(Expression::Tuple(
            SourceRef::unknown(),
            vec![interaction_id.into(), selector, tuple, bus_linker_type],
        ));
    }

    fn process_object(&mut self, location: &Location, objects: &BTreeMap<Location, Object>) {
        let object = &objects[location];

        let namespace_degree = try_into_namespace_degree(object.degree.clone())
            .unwrap_or_else(|| panic!("machine at {location} must have an explicit degree"));

        // create a namespace for this object
        self.pil.push(PilStatement::Namespace(
            SourceRef::unknown(),
            SymbolPath::from_identifier(location.to_string()),
            Some(namespace_degree),
        ));

        self.pil.extend(object.pil.clone());

        if let Some(call_selectors) = &object.call_selectors {
            // declare the call selectors array. In the case no permutation links point to this object, the array is empty and removed by the optimizer.
            let count = self
                .selector_array_size_by_instance
                .get(location)
                .cloned()
                .unwrap_or_default();
            self.pil.extend([
                parse_pil_statement(&format!("col witness {call_selectors}[{count}];")),
                parse_pil_statement(&format!(
                    "std::array::map({call_selectors}, std::utils::force_bool);"
                )),
            ])
        }

        // process outgoing links
        for link in &object.links {
            self.process_link(link, location, objects);
        }
        // receive incoming links
        for (name, operation) in &object.operations {
            self.process_operation(name, operation, location, object);
        }

        // add pil for bus_multi_linker
        if !self.bus_multi_linker_args.items.is_empty() {
            self.pil.push(PilStatement::Expression(
                SourceRef::unknown(),
                Expression::FunctionCall(
                    SourceRef::unknown(),
                    FunctionCall {
                        function: Box::new(Expression::Reference(
                            SourceRef::unknown(),
                            SymbolPath::from_str("std::protocols::bus::bus_multi_linker")
                                .unwrap()
                                .into(),
                        )),
                        arguments: vec![ArrayLiteral {
                            items: std::mem::take(&mut self.bus_multi_linker_args.items),
                        }
                        .into()],
                    },
                ),
            ));
        }

        // if this is the main object, call the main operation
        if *location == Location::main() {
            let operation_id = object.operation_id.clone();
            let main_operation_id = object
                .operations
                .get(MAIN_OPERATION_NAME)
                .and_then(|operation| operation.id.as_ref());

            if let (Some(operation_id_name), Some(operation_id_value)) =
                (operation_id, main_operation_id)
            {
                self.pil
                    .extend(call(&operation_id_name, operation_id_value));
            }
        }
    }

    fn into_pil(self) -> Vec<PilStatement> {
        self.pil
    }
}

impl BusLinker {
    /// Process an operation, which means receive from the bus with the correct [InteractionType]
    fn process_operation(
        &mut self,
        operation_name: &str,
        operation: &Operation,
        location: &Location,
        object: &Object,
    ) {
        let link_to = LinkTo {
            machine: location.clone(),
            operation: operation_name.to_string(),
        };

        let selector_index = self.selector_array_index_by_operation.get(&link_to);

        // By construction, all operations *which are called* have an optional selector index. The others can be safely ignored.
        if let Some(selector_index) = selector_index {
            // get the interaction id for the link.to
            let interaction_id = self.interaction_id_map[&link_to];

            let namespace = location.to_string();

            let op_id = object
                .operation_id
                .clone()
                .map(|oid| namespaced_reference(namespace.clone(), oid))
                .into_iter();

            let tuple = ArrayLiteral {
                items: op_id
                    .chain(operation.params.inputs_and_outputs().map(|i| {
                        index_access(
                            namespaced_reference(namespace.clone(), &i.name),
                            i.index.clone(),
                        )
                    }))
                    .collect(),
            }
            .into();

            let latch = namespaced_reference(namespace.clone(), object.latch.clone().unwrap());

            let arguments = match selector_index {
                // a selector index of None means this operation is called via lookup
                None => vec![
                    interaction_id.into(),
                    latch,
                    tuple,
                    NamespacedPolynomialReference::from(
                        SymbolPath::from_str("std::protocols::bus::BusLinkerType::LookupReceive")
                            .unwrap(),
                    )
                    .into(),
                ],
                // a selector index of Some means this operation is called via permutation
                Some(selector_index) => {
                    let call_selector_array = namespaced_reference(
                        location.to_string(),
                        object
                            .call_selectors
                            .as_ref()
                            .unwrap_or_else(|| panic!("{location} has incoming permutations but doesn't declare call_selectors")),
                        );
                    let call_selector =
                        index_access(call_selector_array, Some((*selector_index).into()));
                    let rhs_selector = latch * call_selector;
                    let bus_linker_type = NamespacedPolynomialReference::from(
                        SymbolPath::from_str(
                            "std::protocols::bus::BusLinkerType::PermutationReceive",
                        )
                        .unwrap(),
                    )
                    .into();
                    vec![interaction_id.into(), rhs_selector, tuple, bus_linker_type]
                }
            };

            self.bus_multi_linker_args
                .items
                .push(Expression::Tuple(SourceRef::unknown(), arguments));
        }
    }
}

#[cfg(test)]
mod test {
    use std::{fs, path::PathBuf};

    use powdr_ast::analyzed::{Analyzed, Identity};
    use powdr_ast::object::MachineInstanceGraph;
    use powdr_number::{FieldElement, GoldilocksField};

    use powdr_analysis::convert_asm_to_pil;
    use powdr_parser::parse_asm;

    use pretty_assertions::assert_eq;

    use crate::LinkerBackend;

    use super::BusLinker;

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
    pol commit call_selectors[0];
    std::array::map(call_selectors, std::utils::force_bool);
    std::protocols::bus::bus_multi_linker([(0, 1, [0, pc, instr__jump_to_operation, instr__reset, instr__loop, instr_return], std::protocols::bus::BusLinkerType::Send)]);
namespace main__rom(4);
    pol constant p_line = [0, 1, 2] + [2]*;
    pol constant p_instr__jump_to_operation = [0, 1, 0] + [0]*;
    pol constant p_instr__loop = [0, 0, 1] + [1]*;
    pol constant p_instr__reset = [1, 0, 0] + [0]*;
    pol constant p_instr_return = [0]*;
    pol constant operation_id = [0]*;
    pol constant latch = [1]*;
    std::protocols::bus::bus_multi_linker([(0, main__rom::latch, [main__rom::operation_id, main__rom::p_line, main__rom::p_instr__jump_to_operation, main__rom::p_instr__reset, main__rom::p_instr__loop, main__rom::p_instr_return], std::protocols::bus::BusLinkerType::LookupReceive)]);
"#;

        let file_name = "../test_data/asm/empty_vm.asm";
        let graph = parse_analyze_and_compile_file::<GoldilocksField>(file_name);
        let pil = BusLinker::link(graph, crate::DegreeMode::Vadcop).unwrap();
        assert_eq!(extract_main(&format!("{pil}")), expectation);
    }

    #[test]
    fn pass_native_bus_interaction_to_backend() {
        let file_name = "../test_data/asm/empty_vm.asm";
        let graph = parse_analyze_and_compile_file::<GoldilocksField>(file_name);
        let pil = BusLinker::link(graph, crate::DegreeMode::Vadcop).unwrap();
        let analyzed: Analyzed<GoldilocksField> = powdr_pil_analyzer::analyze_ast(pil).unwrap();
        let optimized = powdr_pilopt::optimize(analyzed);
        let native_bus_interactions = optimized
            .identities
            .iter()
            .filter_map(|i| match i {
                Identity::BusInteraction(interaction) => Some(interaction),
                _ => None,
            })
            .collect::<Vec<_>>();

        let expectation = r#"Constr::BusInteraction(1, 0, [0, main::pc, main::instr__jump_to_operation, main::instr__reset, main::instr__loop, main::instr_return], 1);
Constr::BusInteraction(-main__rom::multiplicity, 0, [0, main__rom::p_line, main__rom::p_instr__jump_to_operation, main__rom::p_instr__reset, main__rom::p_instr__loop, 0], 1);
"#;
        let native_bus_interactions =
            native_bus_interactions
                .iter()
                .fold(String::new(), |mut acc, i| {
                    acc.push_str(&format!("{i}\n"));
                    acc
                });
        assert_eq!(native_bus_interactions, expectation);
    }
}
