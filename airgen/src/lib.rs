//! Compilation from powdr machines to AIRs

use std::collections::BTreeMap;

use ast::{
    asm_analysis::{
        AnalysisASMFile, LinkDefinitionStatement, Machine, PilBlock, SubmachineDeclaration,
    },
    object::{Link, LinkFrom, LinkTo, Location, Object, Operation, PILGraph},
    parsed::{asm::CallableRef, PilStatement},
};

const MAIN_MACHINE: &str = "Main";
const MAIN_FUNCTION: &str = "main";

use number::FieldElement;

pub fn compile<T: FieldElement>(input: AnalysisASMFile<T>) -> PILGraph<T> {
    let main_location = Location::main();

    // we start from the main machine
    let main_ty = match input.machines.len() {
        // if there is a single machine, treat it as main
        1 => input.machines.keys().next().unwrap().clone(),
        // otherwise, use the machine called `MAIN`
        _ => {
            assert!(input.machines.contains_key(MAIN_MACHINE));
            MAIN_MACHINE.into()
        }
    };

    // get a list of all machines to instantiate. The order does not matter.
    let mut queue = vec![(main_location.clone(), main_ty.clone())];

    let mut instances = vec![];

    while let Some((location, ty)) = queue.pop() {
        let machine = input.machines.get(&ty).unwrap();

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

    // visit the tree compiling the machines
    let objects = instances
        .into_iter()
        .map(|(location, ty)| {
            let object = ASMPILConverter::convert_machine(&location, &ty, &input);
            (location, object)
        })
        .collect();

    let main_ty = input.machines.get(&main_ty).unwrap();

    PILGraph {
        main: ast::object::Machine {
            location: main_location,
            latch: main_ty.latch.clone().unwrap(),
            operation_id: main_ty.operation_id.clone().unwrap(),
        },
        entry_points: main_ty
            .operations()
            .map(|o| Operation {
                name: MAIN_FUNCTION.to_string(),
                id: o.id.id,
                params: o.params.clone(),
            })
            .collect(),
        objects,
    }
}

struct ASMPILConverter<'a, T> {
    /// Location in the machine tree
    location: &'a Location,
    /// Machine types
    machines: &'a BTreeMap<String, Machine<T>>,
    pil: Vec<PilStatement<T>>,
    submachines: Vec<SubmachineDeclaration>,
}

impl<'a, T: FieldElement> ASMPILConverter<'a, T> {
    fn new(location: &'a Location, input: &'a AnalysisASMFile<T>) -> Self {
        Self {
            location,
            machines: &input.machines,
            pil: Default::default(),
            submachines: Default::default(),
        }
    }

    fn handle_inline_pil(&mut self, PilBlock { statements, .. }: PilBlock<T>) {
        self.pil.extend(statements)
    }

    fn convert_machine(
        location: &'a Location,
        ty: &'a str,
        input: &'a AnalysisASMFile<T>,
    ) -> Object<T> {
        Self::new(location, input).convert_machine_inner(ty)
    }

    fn convert_machine_inner(mut self, ty: &str) -> Object<T> {
        let input = self.machines.get(ty).unwrap().clone();

        let degree = input.degree.map(|s| T::from(s.degree).to_degree());

        self.submachines = input.submachines;

        // machines should only have constraints, operations and links at this point
        assert!(input.instructions.is_empty());
        assert!(input.latch.is_some());
        assert!(input.operation_id.is_some());
        assert!(input.registers.is_empty());
        assert!(input.callable.is_only_operations());

        for block in input.constraints {
            self.handle_inline_pil(block);
        }

        let links = input
            .links
            .into_iter()
            .map(|d| self.handle_link_def(d))
            .collect();

        Object {
            degree,
            pil: self.pil,
            links,
        }
    }

    fn handle_link_def(
        &mut self,
        LinkDefinitionStatement {
            start: _,
            flag,
            params,
            to: CallableRef { instance, callable },
        }: LinkDefinitionStatement<T>,
    ) -> Link<T> {
        let from = LinkFrom {
            params: params.clone(),
            flag: flag.clone(),
        };

        // get the machine type name for this submachine from the submachine delcarations
        let instance_ty_name = self
            .submachines
            .iter()
            .find(|s| s.name == instance)
            .unwrap()
            .ty
            .clone();
        // get the machine type from the machine map
        let instance_ty = self.machines.get(&instance_ty_name).unwrap();
        // get the instance location from the current location joined with the instance name
        let instance_location = self.location.clone().join(instance);

        Link {
            from,
            to: instance_ty
                .operation_definitions()
                .find(|o| o.name == callable)
                .map(|d| LinkTo {
                    machine: ast::object::Machine {
                        location: instance_location,
                        latch: instance_ty.latch.clone().unwrap(),
                        operation_id: instance_ty.operation_id.clone().unwrap(),
                    },
                    operation: Operation {
                        name: d.name.to_string(),
                        id: d.operation.id.id,
                        params: d.operation.params.clone(),
                    },
                })
                .unwrap()
                .clone(),
        }
    }
}
