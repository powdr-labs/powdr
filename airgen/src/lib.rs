//! Compilation from powdr machines to AIRs

#![deny(clippy::print_stdout)]

use std::collections::BTreeMap;

use powdr_ast::{
    asm_analysis::{AnalysisASMFile, Item, LinkDefinitionStatement},
    object::{
        Link, LinkFrom, LinkTo, Location, Machine, Object, Operation, PILGraph, TypeOrExpression,
    },
    parsed::{
        asm::{parse_absolute_path, AbsoluteSymbolPath, CallableRef, MachineParams},
        Expression, PilStatement,
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
    let mut queue = vec![(main_location.clone(), main_ty.clone(), vec![])];

    // map instance location to (type, arguments)
    let mut instances = BTreeMap::default();

    while let Some((location, ty, args)) = queue.pop() {
        let machine = input.items.get(&ty).unwrap().try_to_machine().unwrap();

        queue.extend(machine.submachines.iter().map(|def| {
            (
                // get the absolute name for this submachine
                location.clone().join(def.name.clone()),
                // get its type
                def.ty.clone(),
                def.args.clone(),
            )
        }));

        instances.insert(location, (ty, args));
    }

    // count incoming permutations for each machine.
    let mut incoming_permutations = instances
        .keys()
        .map(|location| (location.clone(), 0))
        .collect();

    // visit the tree compiling the machines
    let mut objects: BTreeMap<_, _> = instances
        .keys()
        .map(|location| {
            let object = ASMPILConverter::convert_machine(
                &instances,
                location,
                &input,
                &mut incoming_permutations,
            );
            (location.clone(), object)
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

struct Submachine {
    /// the name of this instance
    pub name: String,
    /// the type of the submachine
    pub ty: AbsoluteSymbolPath,
    /// machine instance location
    pub location: Location,
}

struct ASMPILConverter<'a> {
    /// Map of all machine instances
    instances: &'a BTreeMap<Location, (AbsoluteSymbolPath, Vec<Expression>)>,
    /// Current machine instance
    location: &'a Location,
    /// Input definitions and machines.
    items: &'a BTreeMap<AbsoluteSymbolPath, Item>,
    pil: Vec<PilStatement>,
    /// Machine parameters declaration
    params: MachineParams,
    /// Submachine instances declared by the machine
    submachines: Vec<Submachine>,
    /// keeps track of the total count of incoming permutations for a given machine.
    incoming_permutations: &'a mut BTreeMap<Location, u64>,
}

impl<'a> ASMPILConverter<'a> {
    fn new(
        instances: &'a BTreeMap<Location, (AbsoluteSymbolPath, Vec<Expression>)>,
        location: &'a Location,
        input: &'a AnalysisASMFile,
        incoming_permutations: &'a mut BTreeMap<Location, u64>,
    ) -> Self {
        Self {
            instances,
            location,
            items: &input.items,
            pil: Default::default(),
            params: Default::default(),
            submachines: Default::default(),
            incoming_permutations,
        }
    }

    fn handle_pil_statement(&mut self, statement: PilStatement) {
        self.pil.push(statement);
    }

    fn convert_machine(
        instances: &'a BTreeMap<Location, (AbsoluteSymbolPath, Vec<Expression>)>,
        location: &'a Location,
        input: &'a AnalysisASMFile,
        incoming_permutations: &'a mut BTreeMap<Location, u64>,
    ) -> Object {
        Self::new(instances, location, input, incoming_permutations).convert_machine_inner()
    }

    fn convert_machine_inner(mut self) -> Object {
        let (ty, args) = self.instances.get(self.location).as_ref().unwrap();
        // TODO: This clone doubles the current memory usage
        let Item::Machine(input) = self.items.get(ty).unwrap().clone() else {
            panic!();
        };

        let degree = input.degree;

        self.params = input.params;
        self.submachines = input
            .submachines
            .into_iter()
            .map(|m| Submachine {
                location: self.location.clone().join(m.name.clone()),
                name: m.name,
                ty: m.ty,
            })
            .collect();

        // machines should only have constraints, operations and links at this point
        assert!(input.instructions.is_empty());
        assert!(input.registers.is_empty());
        assert!(input.callable.is_only_operations());

        // process machine parameters
        self.handle_parameters(args);

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

        // get the type name for this submachine from the submachine declarations and parameters
        let instance = self
            .submachines
            .iter()
            .find(|s| s.name == instance)
            .unwrap_or_else(|| {
                let (ty, _) = self.instances.get(self.location).unwrap();
                panic!(
                    "could not find submachine named `{}` in machine `{ty}`",
                    instance
                );
            });
        // get the machine type from the machine map
        let Item::Machine(instance_ty) = self.items.get(&instance.ty).unwrap() else {
            panic!();
        };

        let mut selector_idx = None;

        if is_permutation {
            // increase the permutation count into the destination machine
            let count = self
                .incoming_permutations
                .get_mut(&instance.location)
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
                        location: instance.location.clone(),
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

    // Process machine parameters.
    // Allows machines passed as argument to be referenced.
    // TODO: support some elementary pil types as a machine parameter?
    fn handle_parameters(&mut self, args: &Vec<Expression>) {
        for (param, value) in self.params.0.iter().zip(args) {
            let ty = AbsoluteSymbolPath::default().join(param.ty.clone().unwrap());
            if let Some(Item::Machine(_)) = self.items.get(&ty) {
                // param is a machine, we need to find the actual instance
                // location by going up the machine declaration hierarchy, then
                // make it referenceable as a submachine

                // current location
                let mut loc = self.location.clone();
                // local name of the machine we're trying to find
                let mut loc_submachine =
                    value.try_to_identifier().expect("invalid machine argument");
                let mut found = false;
                while let Some(parent) = loc.parent() {
                    loc = parent;
                    let (loc_ty, loc_args) = self.instances.get(&loc).as_ref().unwrap();
                    let Item::Machine(loc_ty) = self.items.get(loc_ty).unwrap() else {
                        panic!()
                    };
                    // is the name declared as an actual submachine?
                    if loc_ty
                        .submachines
                        .iter()
                        .any(|sm| &sm.name == loc_submachine)
                    {
                        // found the submachine
                        let name = param.name.clone();
                        self.submachines.push(Submachine {
                            location: loc.join(loc_submachine),
                            name,
                            ty,
                        });
                        found = true;
                        break;
                    } else {
                        // submachine is a machine parameter, go up the machine tree
                        let Some((param, value)) = loc_ty
                            .params
                            .0
                            .iter()
                            .zip(loc_args)
                            .find(|(p, _)| &p.name == loc_submachine)
                        else {
                            panic!();
                        };
                        assert_eq!(
                            ty,
                            AbsoluteSymbolPath::default().join(param.ty.clone().unwrap())
                        );
                        loc_submachine =
                            value.try_to_identifier().expect("invalid submachine name");
                    }
                }
                if !found {
                    panic!("could not find submachine");
                }
            } else {
                // maybe we could handle "non-machine" parameters by generating
                // a let statement `let {param.name}: {param.ty} = value`?
                unimplemented!("asm machines do not support non-machine parameters");
            };
        }
    }
}
