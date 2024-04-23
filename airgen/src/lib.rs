//! Compilation from powdr machines to AIRs

#![deny(clippy::print_stdout)]

use std::collections::BTreeMap;

use powdr_ast::{
    asm_analysis::{AnalysisASMFile, Item, LinkDefinitionStatement, SubmachineDeclaration},
    object::{
        Link, LinkFrom, LinkTo, Location, Machine, Object, Operation, PILGraph, TypeOrExpression,
    },
    parsed::{
        asm::{parse_absolute_path, AbsoluteSymbolPath, CallableRef},
        PilStatement,
    },
};

use powdr_analysis::utils::parse_pil_statement;

const MAIN_MACHINE: &str = "::Main";
const MAIN_FUNCTION: &str = "main";

pub fn compile(input: AnalysisASMFile) -> PILGraph {
    let main_location = Location::main();

    let non_std_machines = input
        .machines()
        .filter(|(k, _)| k.parts().next() != Some("std"))
        .collect::<BTreeMap<_, _>>();

    // we start from the main machine
    let main_ty = match non_std_machines.len() {
        0 => {
            // There is no machine. Create an empty main machine but retain
            // all PIL utility definitions.
            let main = Machine {
                location: main_location.clone(),
                latch: None,
                operation_id: None,
                call_selectors: None,
            };
            return PILGraph {
                main,
                entry_points: Default::default(),
                objects: [(main_location, Default::default())].into(),
                definitions: utility_functions(input),
            };
        }
        // if there is a single machine, treat it as main
        1 => (*non_std_machines.keys().next().unwrap()).clone(),
        // otherwise, use the machine called `MAIN`
        _ => {
            let p = parse_absolute_path(MAIN_MACHINE);
            assert!(input.items.contains_key(&p));
            p
        }
    };

    // get a list of all machines to instantiate. The order does not matter.
    let mut queue = vec![(main_location.clone(), main_ty.clone())];

    let mut instances = vec![];

    while let Some((location, ty)) = queue.pop() {
        let machine = input.items.get(&ty).unwrap().try_to_machine().unwrap();

        queue.extend(machine.submachines.iter().map(|def| {
            (
                // get the absolute name for this submachine
                location.clone().join(def.name.clone()),
                // get its type
                def.ty.clone(),
            )
        }));

        instances.push((location, ty));
    }

    // count incoming permutations for each machine.
    let mut incoming_permutations = instances
        .iter()
        .map(|(location, _)| (location.clone(), 0))
        .collect();

    // visit the tree compiling the machines
    let mut objects: BTreeMap<_, _> = instances
        .into_iter()
        .map(|(location, ty)| {
            let object = ASMPILConverter::convert_machine(
                &location,
                &ty,
                &input,
                &mut incoming_permutations,
            );
            (location, object)
        })
        .collect();

    // add pil code for the selector array and related constraints
    for (location, count) in incoming_permutations {
        let obj = objects.get_mut(&location).unwrap();
        if obj.has_pc {
            // VMs don't have call_selectors
            continue;
        }
        assert!(
            count == 0 || obj.call_selectors.is_some(),
            "block machine {location} has incoming permutations but doesn't declare call_selectors"
        );
        if let Some(call_selectors) = obj.call_selectors.as_deref() {
            obj.pil.extend([
                parse_pil_statement(&format!("col witness {call_selectors}[{count}];")),
                parse_pil_statement(&format!(
                    "std::array::map({call_selectors}, std::utils::force_bool);"
                )),
            ]);
        }
    }

    let Item::Machine(main_ty) = input.items.get(&main_ty).unwrap() else {
        panic!()
    };

    let main = powdr_ast::object::Machine {
        location: main_location,
        latch: main_ty.latch.clone(),
        operation_id: main_ty.operation_id.clone(),
        call_selectors: main_ty.call_selectors.clone(),
    };
    let entry_points = main_ty
        .operations()
        .map(|o| Operation {
            name: MAIN_FUNCTION.to_string(),
            id: o.id.id.clone(),
            params: o.params.clone(),
        })
        .collect();

    PILGraph {
        main,
        entry_points,
        objects,
        definitions: utility_functions(input),
    }
}

fn utility_functions(asm_file: AnalysisASMFile) -> BTreeMap<AbsoluteSymbolPath, TypeOrExpression> {
    asm_file
        .items
        .into_iter()
        .filter_map(|(n, v)| match v {
            Item::Expression(e) => Some((n, TypeOrExpression::Expression(e))),
            Item::TypeDeclaration(type_decl) => Some((n, TypeOrExpression::Type(type_decl))),
            _ => None,
        })
        .collect()
}

struct ASMPILConverter<'a> {
    /// Location in the machine tree
    location: &'a Location,
    /// Input definitions and machines.
    items: &'a BTreeMap<AbsoluteSymbolPath, Item>,
    pil: Vec<PilStatement>,
    submachines: Vec<SubmachineDeclaration>,
    /// keeps track of the total count of incoming permutations for a given machine.
    incoming_permutations: &'a mut BTreeMap<Location, u64>,
}

impl<'a> ASMPILConverter<'a> {
    fn new(
        location: &'a Location,
        input: &'a AnalysisASMFile,
        incoming_permutations: &'a mut BTreeMap<Location, u64>,
    ) -> Self {
        Self {
            location,
            items: &input.items,
            pil: Default::default(),
            submachines: Default::default(),
            incoming_permutations,
        }
    }

    fn handle_pil_statement(&mut self, statement: PilStatement) {
        self.pil.push(statement);
    }

    fn convert_machine(
        location: &'a Location,
        ty: &'a AbsoluteSymbolPath,
        input: &'a AnalysisASMFile,
        incoming_permutations: &'a mut BTreeMap<Location, u64>,
    ) -> Object {
        Self::new(location, input, incoming_permutations).convert_machine_inner(ty)
    }

    fn convert_machine_inner(mut self, ty: &AbsoluteSymbolPath) -> Object {
        // TODO: This clone doubles the current memory usage
        let Item::Machine(input) = self.items.get(ty).unwrap().clone() else {
            panic!();
        };

        let degree = input.degree.map(|s| s.degree);

        self.submachines = input.submachines;

        // machines should only have constraints, operations and links at this point
        assert!(input.instructions.is_empty());
        assert!(input.registers.is_empty());
        assert!(input.callable.is_only_operations());

        for block in input.pil {
            self.handle_pil_statement(block);
        }

        let call_selectors = input.call_selectors;
        let has_pc = input.pc.is_some();
        let links = input
            .links
            .into_iter()
            .map(|d| self.handle_link_def(d))
            .collect();

        Object {
            degree,
            pil: self.pil,
            links,
            latch: input.latch,
            call_selectors,
            has_pc,
        }
    }

    fn handle_link_def(
        &mut self,
        LinkDefinitionStatement {
            source: _,
            flag,
            to:
                CallableRef {
                    instance,
                    callable,
                    params,
                },
            is_permutation,
        }: LinkDefinitionStatement,
    ) -> Link {
        let from = LinkFrom {
            params,
            flag: flag.clone(),
        };

        // get the machine type name for this submachine from the submachine declarations
        let instance_ty_name = self
            .submachines
            .iter()
            .find(|s| s.name == instance)
            .unwrap()
            .ty
            .clone();
        // get the machine type from the machine map
        let Item::Machine(instance_ty) = self.items.get(&instance_ty_name).unwrap() else {
            panic!();
        };
        // get the instance location from the current location joined with the instance name
        let instance_location = self.location.clone().join(instance);

        let mut selector_idx = None;

        if is_permutation {
            // increase the permutation count into the destination machine
            let count = self
                .incoming_permutations
                .get_mut(&instance_location)
                .unwrap();
            selector_idx = Some(*count);
            *count += 1;
        }

        Link {
            from,
            to: instance_ty
                .operation_definitions()
                .find(|o| o.name == callable)
                .map(|d| LinkTo {
                    machine: powdr_ast::object::Machine {
                        location: instance_location,
                        latch: instance_ty.latch.clone(),
                        call_selectors: instance_ty.call_selectors.clone(),
                        operation_id: instance_ty.operation_id.clone(),
                    },
                    operation: Operation {
                        name: d.name.to_string(),
                        id: d.operation.id.id.clone(),
                        params: d.operation.params.clone(),
                    },
                    selector_idx,
                })
                .unwrap()
                .clone(),
            is_permutation,
        }
    }
}
