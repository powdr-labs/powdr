//! Compilation from powdr assembly to PIL

// changes:
// - unconstrain registers on first line, as the ASM takes care of resetting them
// - unconstrain the inputs when _reset is called

use std::collections::BTreeMap;

use ast::{
    asm_analysis::{
        AnalysisASMFile, InstructionDefinitionStatement, Machine, PilBlock, SubmachineDeclaration,
    },
    object::{Function, Instr, Link, LinkFrom, LinkTo, Location, Object, PILGraph},
    parsed::{asm::InstructionBody, PilStatement},
};

const MAIN: &str = "Main";

use number::FieldElement;

pub fn compile<T: FieldElement>(analysis: AnalysisASMFile<T>) -> PILGraph<T> {
    Session::default().compile(analysis)
}

#[derive(Default)]
/// A compilation session
struct Session<T> {
    /// the machine types used in the tree
    machine_types: BTreeMap<String, Machine<T>>,
}

impl<T: FieldElement> Session<T> {
    fn instantiate_machine(&mut self, location: &Location, ty: String) -> Object<T> {
        ASMPILConverter::new(location, &self.machine_types)
            .convert_machine(self.machine_types.get(&ty).unwrap().clone())
    }

    fn compile(&mut self, mut input: AnalysisASMFile<T>) -> PILGraph<T> {
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

        self.machine_types = std::mem::take(&mut input.machines);

        // get a list of all machines to instantiate. The order does not matter.
        let mut queue = vec![(main_location.clone(), main_ty.clone())];

        let mut instances = vec![];

        while let Some((location, ty)) = queue.pop() {
            let machine = self.machine_types.get(&ty).unwrap();

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
                let object = self.instantiate_machine(&location, ty);
                (location, object)
            })
            .collect();

        let main_ty = self.machine_types.get(&main_ty).unwrap();
        let main_function = &main_ty.functions[0];

        PILGraph {
            entry_point: LinkTo {
                loc: main_location,
                latch: main_ty.latch.clone().unwrap(),
                function_id: main_ty.function_id.clone().unwrap(),
                function: Function {
                    name: "main".to_string(),
                    id: main_function.id.unwrap(),
                    params: main_function.params.clone(),
                },
            },
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
        let degree = if let Some(s) = &input.degree {
            T::from(s.degree.clone()).to_degree()
        } else {
            1024
        };

        self.submachines = input.submachines;

        for block in input.constraints {
            self.handle_inline_pil(block);
        }

        let links = input
            .instructions
            .into_iter()
            .filter_map(|instr| self.handle_instruction_def(instr))
            .collect();

        Object {
            degree,
            pil: self.pil,
            links,
        }
    }

    fn handle_instruction_def(
        &mut self,
        InstructionDefinitionStatement {
            start: _,
            body,
            name,
            params,
        }: InstructionDefinitionStatement<T>,
    ) -> Option<Link<T>> {
        let instruction_flag = format!("instr_{name}");
        let instr = Instr {
            name: name.clone(),
            params: params.clone(),
            flag: instruction_flag.clone(),
        };

        let link = match body {
            InstructionBody::Local(_body) => None,
            InstructionBody::External(instance, function) => {
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

                Some(Link {
                    from: LinkFrom { instr },
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
                })
            }
        };
        link
    }
}
