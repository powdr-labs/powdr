//! Compilation from powdr machines to AIRs

use std::{collections::BTreeMap, marker::PhantomData};

use ast::{
    asm_analysis::{
        AnalysisASMFile, LinkDefinitionStatement, Machine, PilBlock, SubmachineDeclaration,
    },
    object::{Function, Link, LinkFrom, LinkTo, Location, Object, PILGraph},
    parsed::{asm::FunctionRef, PilStatement},
};

const MAIN: &str = "Main";

use number::FieldElement;

pub fn compile<T: FieldElement>(analysis: AnalysisASMFile<T>) -> PILGraph<T> {
    Session::default().compile(analysis)
}

#[derive(Default)]
/// A compilation session
struct Session<T> {
    marker: PhantomData<T>,
}

impl<T: FieldElement> Session<T> {
    fn instantiate_machine(
        &mut self,
        location: &Location,
        ty: String,
        file: &AnalysisASMFile<T>,
    ) -> Object<T> {
        ASMPILConverter::new(location, &file.machines)
            .convert_machine(file.machines.get(&ty).unwrap().clone())
    }

    fn compile(&mut self, input: AnalysisASMFile<T>) -> PILGraph<T> {
        let main_location = Location::default().join("main");

        // we start from the main machine
        let main_ty = match input.machines.len() {
            // if there is a single machine, treat it as main
            1 => input.machines.keys().next().unwrap().clone(),
            // otherwise, use the machine called `MAIN`
            _ => {
                assert!(input.machines.contains_key(MAIN));
                MAIN.into()
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
                let object = self.instantiate_machine(&location, ty, &input);
                (location, object)
            })
            .collect();

        let main_ty = input.machines.get(&main_ty).unwrap();

        PILGraph {
            entry_points: main_ty
                .functions
                .iter()
                .map(|f| LinkTo {
                    loc: main_location.clone(),
                    latch: main_ty.latch.clone().unwrap(),
                    function_id: main_ty.function_id.clone().unwrap(),
                    function: Function {
                        name: "main".to_string(),
                        id: f.id.unwrap(),
                        params: f.params.clone(),
                    },
                })
                .collect(),
            objects,
        }
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
    fn new(location: &'a Location, machines: &'a BTreeMap<String, Machine<T>>) -> Self {
        Self {
            location,
            machines,
            pil: Default::default(),
            submachines: Default::default(),
        }
    }

    fn handle_inline_pil(&mut self, PilBlock { statements, .. }: PilBlock<T>) {
        self.pil.extend(statements)
    }

    fn convert_machine(mut self, input: Machine<T>) -> Object<T> {
        let degree = input.degree.map(|s| T::from(s.degree.clone()).to_degree());

        self.submachines = input.submachines;

        for block in input.constraints {
            self.handle_inline_pil(block);
        }

        let links = input
            .links
            .into_iter()
            .map(|instr| self.handle_link_def(instr))
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
            to: FunctionRef { instance, function },
        }: LinkDefinitionStatement,
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
                .functions
                .iter()
                .find(|f| f.name == function)
                .map(|d| LinkTo {
                    loc: instance_location,
                    latch: instance_ty.latch.clone().unwrap(),
                    function_id: instance_ty.function_id.clone().unwrap(),
                    function: Function {
                        name: d.name.clone(),
                        id: d.id.unwrap(),
                        params: d.params.clone(),
                    },
                })
                .unwrap()
                .clone(),
        }
    }
}
