//! Compilation from powdr machines to AIRs

#![deny(clippy::print_stdout)]

use std::collections::{BTreeMap, BTreeSet};

use powdr_ast::{
    asm_analysis::{
        combine_flags, AnalysisASMFile, Item, LinkDefinition, MachineInstance,
        MachineInstanceExpression,
    },
    object::{Link, LinkFrom, LinkTo, Location, Object, Operation, PILGraph, TypeOrExpression},
    parsed::{
        asm::{parse_absolute_path, AbsoluteSymbolPath, CallableRef},
        Expression, PilStatement,
    },
};

use itertools::Either;
use itertools::Itertools;

use powdr_analysis::utils::parse_pil_statement;

const MAIN_MACHINE_INSTANCE: &str = "::main";
const MAIN_MACHINE: &str = "::Main";
const MAIN_FUNCTION: &str = "main";

#[derive(Default, Debug)]
struct Instance {
    ty: AbsoluteSymbolPath,
    members: Vec<Location>,
}

#[derive(Default, Debug)]
struct Instances {
    map: Vec<(Location, Instance)>,
}

impl Instances {
    fn get(&self, l: &Location) -> Option<&Instance> {
        self.map
            .iter()
            .find_map(|(location, value)| (l == location).then_some(value))
    }

    fn retain(&mut self, f: impl FnMut(&(Location, Instance)) -> bool) {
        self.map.retain(f);
    }

    fn insert(&mut self, key: Location, value: Instance) {
        self.map.push((key, value));
    }

    // iterate through instances so that any instance used as a member is visited first. in this implementation, it's based on source ordering but we should remove that requirement
    fn iter(&self) -> impl Iterator<Item = (&Location, &Instance)> {
        self.map.iter().map(|(l, v)| (l, v))
    }
}

#[derive(Default, Debug)]
struct SourceInstances {
    abs_to_loc: BTreeMap<AbsoluteSymbolPath, Location>,
    map: Instances,
}

impl SourceInstances {
    /// Trim to remove instances unreachable from a given Location
    /// Returns the instance map
    /// untested!
    fn trim(self, from: &Location) -> Instances {
        let mut reached: BTreeSet<Location> = Default::default();
        let mut queue = vec![from];
        let mut map = self.map;
        while let Some(location) = queue.pop() {
            // `location` is reached
            reached.insert(location.clone());
            let instance = &map.get(location).unwrap();
            // visit all params
            queue.extend(&instance.members);
        }
        // remove all keys that aren't reached
        map.retain(|(l, _)| reached.contains(l));
        map
    }

    fn fold_instance(
        &mut self,
        file: &AnalysisASMFile,
        location: Location,
        instance: &MachineInstance,
    ) -> Location {
        match &instance.value {
            MachineInstanceExpression::Value(v) => {
                let ty = if let Item::Machine(ty) = &file.items[&instance.ty] {
                    ty
                } else {
                    panic!()
                };
                assert_eq!(ty.params.0.len() + ty.submachines.len(), v.len());

                let members = ty
                    .params
                    .0
                    .iter()
                    .map(|param| &param.name)
                    .chain(ty.submachines.iter().map(|d| &d.name))
                    .zip(v)
                    .map(|(name, instance)| {
                        let new_loc =
                            self.fold_instance(file, location.clone().join(name.clone()), instance);
                        new_loc
                    })
                    .collect();
                self.map.insert(
                    location.clone(),
                    Instance {
                        ty: instance.ty.clone(),
                        members,
                    },
                );
                location.clone()
            }
            MachineInstanceExpression::Reference(r) => {
                // do nothing, it's all in abs_to_loc
                self.abs_to_loc[r].clone()
            }
        }
    }
}

/// Instantiate machine type at `ty_path` by instantiating all submachines recursively
fn instantiate(
    input: &AnalysisASMFile,
    instances: &mut Vec<(AbsoluteSymbolPath, MachineInstance)>,
    path: AbsoluteSymbolPath,
    ty_path: &AbsoluteSymbolPath,
    args: &Vec<Expression>,
) {
    let ty = if let Item::Machine(machine) = &input.items[ty_path] {
        machine
    } else {
        unreachable!()
    };

    let submachines: Vec<MachineInstance> = ty
        .params
        .0
        .iter()
        .zip(args)
        .map(|(param, argument)| {
            let ty = AbsoluteSymbolPath::default().join(param.ty.clone().unwrap());
            match input.items.get(&ty) {
                Some(Item::Machine(_)) => MachineInstance {
                    ty,
                    value: MachineInstanceExpression::Reference(parse_absolute_path(&format!(
                        "{}_{}",
                        path.to_string()
                            .split('_')
                            .rev()
                            .skip(1)
                            .collect::<Vec<_>>()
                            .into_iter()
                            .rev()
                            .collect::<Vec<_>>()
                            .join("_"),
                        argument.try_to_identifier().unwrap().clone()
                    ))),
                },
                _ => unimplemented!(),
            }
        })
        .collect::<Vec<_>>()
        .into_iter()
        .chain(ty.submachines.iter().map(|d| {
            let path = parse_absolute_path(&format!("{}_{}", path, d.name));
            instantiate(input, instances, path.clone(), &d.ty, &d.args);
            MachineInstance {
                ty: d.ty.clone(),
                value: MachineInstanceExpression::Reference(path),
            }
        }))
        .collect();

    let instance = MachineInstance {
        ty: ty_path.clone(),
        value: MachineInstanceExpression::Value(submachines),
    };

    instances.push((path, instance));
}

pub fn compile(input: AnalysisASMFile) -> PILGraph {
    let mut input = input;
    let non_std_non_rom_machines = input
        .machines()
        .filter(|(k, _)| k.parts().next() != Some("std"))
        .filter(|(k, _)| !k.parts().last().unwrap().ends_with("ROM"))
        .collect::<BTreeMap<_, _>>();

    // if no instances are defined, inject some automatically
    let src_instances = if input.instances().count() == 0 {
        // get the main machine type
        let main_ty = match non_std_non_rom_machines.len() {
            // if there is a single machine, treat it as main
            1 => (*non_std_non_rom_machines.keys().next().unwrap()).clone(),
            // otherwise, use the machine called `MAIN_MACHINE` and declare it if needed
            _ => {
                let p = parse_absolute_path(MAIN_MACHINE);
                input
                    .items
                    .entry(p.clone())
                    .or_insert_with(|| Item::Machine(Default::default()));
                p
            }
        };

        // instantiate the main machine in the naive way (no reuse of submachines)
        let mut instances = vec![];
        instantiate(
            &input,
            &mut instances,
            parse_absolute_path(MAIN_MACHINE_INSTANCE),
            &main_ty,
            &vec![],
        );

        instances
    } else {
        input
            .instances()
            .map(|(path, instance)| (path.clone(), instance.clone()))
            .collect()
    };

    // generate the trimmed instance map
    let instances = src_instances
        .iter()
        .fold(
            SourceInstances::default(),
            |mut instances, (path, instance)| {
                assert_eq!(path.len(), 1);
                let location = Location::default().join(path.parts().next().unwrap());
                instances.abs_to_loc.insert(path.clone(), location.clone());
                let new_location = instances.fold_instance(&input, location.clone(), instance);
                assert_eq!(new_location, location);
                instances
            },
        )
        .trim(&Location::main());

    // count incoming permutations for each machine.
    let mut incoming_permutations = instances
        .iter()
        .map(|(path, _)| path)
        .map(|location| (location.clone(), 0))
        .collect();

    // visit the tree compiling the machines
    let mut objects: BTreeMap<_, _> = instances
        .iter()
        .map(|(path, _)| path)
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

    let (_, main_instance) = src_instances
        .iter()
        .find(|(l, _)| l == &parse_absolute_path(MAIN_MACHINE_INSTANCE))
        .unwrap();

    let Item::Machine(main_ty) = input.items.get(&main_instance.ty).unwrap() else {
        panic!()
    };

    let main = powdr_ast::object::Machine {
        // TODO: this is only correct because Path "::main" happens to point to Location "main"
        // clean up
        location: Location::main(),
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

// // resolve argument in a submachine declaration to a machine instance location
// fn resolve_submachine_arg(
//     location: &Location,
//     machine: &asm_analysis::Machine,
//     args: &[Location],
//     submachine_arg: &Expression,
// ) -> Location {
//     // We only support machine instances as arguments. This has already been checked before
//     let id = submachine_arg.try_to_identifier().unwrap().clone();
//     if let Some((_, arg)) = machine
//         .params
//         .0
//         .iter()
//         .zip(args.iter())
//         .find(|(param, _)| param.name == id)
//     {
//         // argument is the name of a parameter, pass it forward
//         arg.clone()
//     } else {
//         // argument is the name of another submachine, join with current location
//         location.clone().join(id)
//     }
// }

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

struct SubmachineRef {
    /// local name for this instance
    pub name: String,
    /// machine instance location
    pub location: Location,
    /// type of the submachine
    pub ty: AbsoluteSymbolPath,
}

struct ASMPILConverter<'a> {
    /// Map of all machine instances to their type and passed arguments
    instances: &'a Instances,
    /// Current machine instance
    location: &'a Location,
    /// Input definitions and machines.
    items: &'a BTreeMap<AbsoluteSymbolPath, Item>,
    /// Pil statements generated for the machine
    pil: Vec<PilStatement>,
    /// Submachine instances accessible to the machine (includes those passed as a parameter)
    submachines: Vec<SubmachineRef>,
    /// keeps track of the total count of incoming permutations for a given machine.
    incoming_permutations: &'a mut BTreeMap<Location, u64>,
}

impl<'a> ASMPILConverter<'a> {
    fn new(
        instances: &'a Instances,
        location: &'a Location,
        input: &'a AnalysisASMFile,
        incoming_permutations: &'a mut BTreeMap<Location, u64>,
    ) -> Self {
        Self {
            instances,
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
        instances: &'a Instances,
        location: &'a Location,
        input: &'a AnalysisASMFile,
        incoming_permutations: &'a mut BTreeMap<Location, u64>,
    ) -> Object {
        Self::new(instances, location, input, incoming_permutations).convert_machine_inner()
    }

    fn convert_machine_inner(mut self) -> Object {
        let instance = self.instances.get(self.location).unwrap();
        // TODO: This clone doubles the current memory usage
        let Item::Machine(input) = self.items.get(&instance.ty).unwrap().clone() else {
            panic!();
        };

        let degree = input.degree;

        self.submachines = instance
            .members
            .iter()
            .zip(
                input
                    .params
                    .0
                    .iter()
                    .map(|p| &p.name)
                    .chain(input.submachines.iter().map(|d| &d.name)),
            )
            .map(|(location, name)| SubmachineRef {
                location: location.clone(),
                name: name.clone(),
                ty: self.instances.get(location).unwrap().ty.clone(),
            })
            .collect();

        // machines should only have constraints, operations and links at this point
        assert!(input.instructions.is_empty());
        assert!(input.registers.is_empty());
        assert!(input.callable.is_only_operations());

        for block in input.pil {
            self.handle_pil_statement(block);
        }

        let mut links = self.process_and_merge_links(&input.links[..]);

        // for each permutation link, increase the permutation count in the destination machine and set its selector index
        for link in &mut links {
            if link.is_permutation {
                let count = self
                    .incoming_permutations
                    .get_mut(&link.to.machine.location)
                    .unwrap();
                link.to.selector_idx = Some(*count);
                *count += 1;
            }
        }

        Object {
            degree,
            pil: self.pil,
            links,
            latch: input.latch,
            call_selectors: input.call_selectors,
            has_pc: input.pc.is_some(),
        }
    }

    // Convert a link definition to a link, doing some basic checks in the process
    fn handle_link_def(
        &self,
        LinkDefinition {
            source: _,
            instr_flag,
            link_flag,
            to:
                CallableRef {
                    instance,
                    callable,
                    params,
                },
            is_permutation,
        }: LinkDefinition,
    ) -> Link {
        let from = LinkFrom {
            params,
            instr_flag,
            link_flag,
        };

        // get the type name for this submachine from the submachine declarations and parameters
        let instance = self
            .submachines
            .iter()
            .find(|s| s.name == instance)
            .unwrap_or_else(|| {
                let ty = &self.instances.get(self.location).unwrap().ty;
                panic!("could not find submachine named `{instance}` in machine `{ty}`");
            });
        // get the machine type from the machine map
        let Item::Machine(instance_ty) = self.items.get(&instance.ty).unwrap() else {
            panic!();
        };

        // check that the operation exists and that it has the same number of inputs/outputs as the link
        let operation = instance_ty
            .operation_definitions()
            .find(|o| o.name == callable)
            .unwrap_or_else(|| {
                panic!(
                    "function/operation not found: {}.{}",
                    &instance.name, callable
                )
            });
        assert_eq!(
            operation.operation.params.inputs.len(),
            from.params.inputs.len(),
            "link and operation have different number of inputs"
        );
        assert_eq!(
            operation.operation.params.outputs.len(),
            from.params.outputs.len(),
            "link and operation have different number of outputs"
        );

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
                    // this will be set later, after compatible links are merged
                    selector_idx: None,
                })
                .unwrap()
                .clone(),
            is_permutation,
        }
    }

    /// Process each link and then combine compatible links.
    /// Links can be merged iff:
    /// - they originate from the same machine instance
    /// - they target the same instance.operation
    /// - they are of the same kind (permutation/lookup)
    /// - their flags are mutually exclusive
    /// Right now we only consider links from different instructions,
    /// as a single instruction can be active at a time.
    fn process_and_merge_links(&self, defs: &[LinkDefinition]) -> Vec<Link> {
        /// Helper struct to group links that can potentially be merged.
        /// Besides these being equal, the links must be mutually exclusive (e.g., come from different instructions)
        #[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
        struct LinkInfo {
            from: Location,
            to: Location,
            operation: Operation,
            is_permutation: bool,
        }

        // process links, partitioning them into (mergeable, non-mergeable)
        let (mergeable_links, mut links): (Vec<_>, Vec<_>) = defs.iter().partition_map(|l| {
            let link = self.handle_link_def(l.clone());
            let info = LinkInfo {
                from: self.location.clone(),
                to: link.to.machine.location.clone(),
                operation: link.to.operation.clone(),
                is_permutation: link.is_permutation,
            };

            if link.from.instr_flag.is_none() {
                // only merge links that from instructions
                Either::Right(link)
            } else if link
                .from
                .params
                .inputs_and_outputs()
                .any(|p| p.contains_next_ref())
            {
                // TODO: links with next references can't be merged due to a witgen limitation.
                // This else if can be removed when witgen supports it.
                Either::Right(link)
            } else {
                // mergeable
                Either::Left((info, link))
            }
        });

        // group links into compatible sets, the idea here is:
        // - group by LinkInfo
        // - inside each group, separate links into sets of mutually exclusive flags (that is, from different instructions)
        let mut grouped_links: BTreeMap<LinkInfo, Vec<BTreeMap<Expression, Link>>> =
            Default::default();
        for (info, link) in mergeable_links {
            // add to an existing compatible set where the instr flag is not yet present
            let e = grouped_links.entry(info).or_default();
            if let Some(link_set) = e
                .iter_mut()
                .find(|link_set| !link_set.contains_key(link.from.instr_flag.as_ref().unwrap()))
            {
                link_set.insert(link.from.instr_flag.clone().unwrap(), link);
            } else {
                // otherwise, create a new set
                let mut new_set = BTreeMap::new();
                new_set.insert(link.from.instr_flag.clone().unwrap(), link);
                e.push(new_set);
            }
        }

        // merge link sets
        let merged_links = grouped_links
            .into_values()
            .flatten()
            .filter_map(|link_set| {
                // single link set, we don't need to combine the flag with inputs/outputs
                if link_set.len() == 1 {
                    return link_set.into_values().next();
                }

                // Merge links in set. Merging two links consists of adding their respective flags and inputs/outputs.
                // For example (asm and respective pil):
                //    instr foo X, Y -> Z link => Z = m.add(X, Y);
                //    instr_foo { 0, X, Y, Z } in m.latch { m.op_id, m.x, m.y, m.z };
                // and:
                //    instr bar X, Z -> Y link => Y = m.add(X, Z);
                //    instr_bar { 0, X, Z, Y } in m.latch { m.op_id, m.x, m.y, m.z };
                // would be combined into the following link:
                //    instr_foo + instr_bar { 0, X * instr_foo + X * instr_bar, Y * instr_foo + Z * instr_bar, Z * instr_bar + Y * instr_foo }
                //          in m.latch { m.op_id, m.x, m.y, m.z };
                link_set
                    .into_values()
                    .map(|mut link| {
                        // clear instruction flag by combining into the link flag, then combine it with inputs/outputs
                        link.from.link_flag =
                            combine_flags(link.from.instr_flag.take(), link.from.link_flag.clone());
                        link.from.params.inputs_and_outputs_mut().for_each(|p| {
                            *p = p.clone() * link.from.link_flag.clone();
                        });
                        link
                    })
                    .reduce(|mut a, b| {
                        // add flags and inputs/outputs of the two links
                        assert_eq!(a.from.params.inputs.len(), b.from.params.inputs.len());
                        assert_eq!(a.from.params.outputs.len(), b.from.params.outputs.len());
                        a.from.link_flag = a.from.link_flag + b.from.link_flag;
                        a.from
                            .params
                            .inputs_and_outputs_mut()
                            .zip(b.from.params.inputs_and_outputs())
                            .for_each(|(pa, pb)| {
                                *pa = pa.clone() + pb.clone();
                            });
                        a
                    })
            });
        links.extend(merged_links);
        links
    }
}

#[cfg(test)]
mod tests {
    use std::iter::once;

    use pretty_assertions::assert_eq;

    use powdr_analysis::convert_asm_to_pil;
    use powdr_ast::{
        asm_analysis::{AnalysisASMFile, Item, MachineInstance, MachineInstanceExpression},
        object::{Location, Machine, Object, PILGraph},
        parsed::asm::parse_absolute_path,
    };
    use powdr_number::{FieldElement, GoldilocksField};
    use powdr_parser::parse_asm;

    use crate::{compile, utility_functions};

    fn parse_analyze_and_convert_file<T: FieldElement>(file: &str) -> AnalysisASMFile {
        let contents = std::fs::read_to_string(file).unwrap();
        let parsed = parse_asm(None, &contents).unwrap_or_else(|e| {
            e.output_to_stderr();
            panic!();
        });
        let resolved = powdr_importer::load_dependencies_and_resolve(None, parsed).unwrap();
        convert_asm_to_pil::<T>(resolved).unwrap()
    }

    #[test]
    fn single_instance() {
        let asm = "test_data/program.asm";
        let mut asm = parse_analyze_and_convert_file::<GoldilocksField>(asm);
        // let main = Bar {};
        asm.items.insert(
            parse_absolute_path("::main"),
            Item::Instance(MachineInstance {
                ty: parse_absolute_path("::Bar"),
                // no arguments
                value: MachineInstanceExpression::Value(Default::default()),
            }),
        );
        let definitions = utility_functions(asm.clone());
        let compiled = compile(asm);
        assert_eq!(
            compiled.to_string(),
            PILGraph {
                main: Machine {
                    location: Location::main(),
                    latch: None,
                    call_selectors: None,
                    operation_id: None
                },
                entry_points: vec![],
                objects: once((Location::main(), Object::default())).collect(),
                definitions
            }
            .to_string()
        );
    }

    #[test]
    fn one_submachine() {
        let asm = "test_data/program.asm";
        let mut asm = parse_analyze_and_convert_file::<GoldilocksField>(asm);
        // let bar = Bar {};
        // let main = Foo { bar };
        asm.items.extend([
            (
                parse_absolute_path("::bar"),
                Item::Instance(MachineInstance {
                    ty: parse_absolute_path("::Bar"),
                    // no arguments
                    value: MachineInstanceExpression::Value(Default::default()),
                }),
            ),
            (
                parse_absolute_path("::main"),
                Item::Instance(MachineInstance {
                    ty: parse_absolute_path("::Foo"),
                    // pass bar
                    value: MachineInstanceExpression::Value(
                        once(MachineInstance {
                            ty: parse_absolute_path("::Bar"),
                            value: MachineInstanceExpression::Reference(parse_absolute_path(
                                "::bar",
                            )),
                        })
                        .collect(),
                    ),
                }),
            ),
        ]);
        let definitions = utility_functions(asm.clone());
        let compiled = compile(asm);
        assert_eq!(
            compiled.to_string(),
            PILGraph {
                main: Machine {
                    location: Location::main(),
                    latch: None,
                    call_selectors: None,
                    operation_id: None
                },
                entry_points: vec![],
                objects: once((Location::from("main"), Object::default(),))
                    .chain(once((Location::from("bar"), Object::default(),)))
                    .collect(),
                definitions
            }
            .to_string()
        );
    }

    #[test]
    fn share_submachine() {
        let asm = "test_data/program.asm";
        let mut asm = parse_analyze_and_convert_file::<GoldilocksField>(asm);
        // let bar = Bar {};
        // let foo = Foo { bar };
        // let main = Baz { foo, bar };
        asm.items.extend([
            (
                parse_absolute_path("::bar"),
                Item::Instance(MachineInstance {
                    ty: parse_absolute_path("::Bar"),
                    // no arguments
                    value: MachineInstanceExpression::Value(Default::default()),
                }),
            ),
            (
                parse_absolute_path("::foo"),
                Item::Instance(MachineInstance {
                    ty: parse_absolute_path("::Foo"),
                    // pass bar
                    value: MachineInstanceExpression::Value(
                        once(MachineInstance {
                            ty: parse_absolute_path("::Bar"),
                            value: MachineInstanceExpression::Reference(parse_absolute_path(
                                "::bar",
                            )),
                        })
                        .collect(),
                    ),
                }),
            ),
            (
                parse_absolute_path("::main"),
                Item::Instance(MachineInstance {
                    ty: parse_absolute_path("::Baz"),
                    // pass bar
                    value: MachineInstanceExpression::Value(
                        once(MachineInstance {
                            ty: parse_absolute_path("::Foo"),
                            value: MachineInstanceExpression::Reference(parse_absolute_path(
                                "::foo",
                            )),
                        })
                        .chain(once(MachineInstance {
                            ty: parse_absolute_path("::Bar"),
                            value: MachineInstanceExpression::Reference(parse_absolute_path(
                                "::bar",
                            )),
                        }))
                        .collect(),
                    ),
                }),
            ),
        ]);
        let definitions = utility_functions(asm.clone());
        let compiled = compile(asm);
        assert_eq!(
            compiled.to_string(),
            PILGraph {
                main: Machine {
                    location: Location::main(),
                    latch: None,
                    call_selectors: None,
                    operation_id: None
                },
                entry_points: vec![],
                objects: [
                    (Location::from("main"), Object::default()),
                    (Location::from("bar"), Object::default()),
                    (Location::from("foo"), Object::default())
                ]
                .into_iter()
                .collect(),
                definitions
            }
            .to_string()
        );
    }
}
