use std::collections::{HashMap, HashSet};

use powdr_ast::asm_analysis::{AnalysisASMFile, FunctionSymbol, Machine};
use powdr_pilopt::referenced_symbols::ReferencedSymbols;

pub fn optimize(mut analyzed_asm: AnalysisASMFile) -> AnalysisASMFile {
    println!(
        "Optimizing machine count: {:?}",
        analyzed_asm.machines().count()
    );
    remove_unreferenced_machines(&mut analyzed_asm);
    remove_unused_instructions(&mut analyzed_asm);
    remove_unreferenced_machines(&mut analyzed_asm);
    println!(
        "Optimized machines count: {:?}",
        analyzed_asm.machines().count()
    );
    analyzed_asm
}

fn remove_unreferenced_machines(asm_file: &mut AnalysisASMFile) {
    let deps = build_machine_dependencies(&asm_file);
    let all_machines = collect_all_dependent_machines(&deps, "Main");
    asm_file
        .modules
        .iter_mut()
        .for_each(|(_, module)| module.retain_machines(all_machines.clone()));
}

fn build_machine_dependencies(asm_file: &AnalysisASMFile) -> HashMap<String, Vec<String>> {
    let mut dependencies = HashMap::new();

    for (path, machine) in asm_file.machines() {
        let submachine_to_decl: HashMap<String, String> = machine
            .submachines
            .iter()
            .map(|sub| (sub.name.clone(), sub.ty.clone().pop().unwrap().to_string()))
            .collect();

        println!("Submachine to decl: {:?}", submachine_to_decl);

        let submachine_names: Vec<String> = machine
            .submachines
            .iter()
            .map(|sub| sub.ty.clone().pop().unwrap().to_string())
            .chain(
                machine
                    .params
                    .0
                    .iter()
                    .map(|param| param.ty.clone().unwrap().pop().unwrap().to_string()),
            )
            .chain(
                machine
                    .links
                    .iter()
                    .filter_map(|ld| submachine_to_decl.get(&ld.to.instance))
                    .cloned(),
            )
            .collect();
        dependencies.insert(path.clone().pop().unwrap().to_string(), submachine_names);
    }

    dependencies
}

fn collect_all_dependent_machines(
    dependencies: &HashMap<String, Vec<String>>,
    start: &str,
) -> Vec<String> {
    let mut result = Vec::new();
    let mut to_visit = vec![start.to_string()];
    let mut visited = HashSet::new();

    while let Some(machine) = to_visit.pop() {
        if visited.insert(machine.clone()) {
            result.push(machine.clone());

            if let Some(submachines) = dependencies.get(&machine) {
                to_visit.extend(submachines.iter().cloned());
            }
        }
    }

    result
}

fn remove_unused_instructions(asm_file: &mut AnalysisASMFile) {
    for (_, machine) in asm_file.machines_mut() {
        let symbols_in_callable = machine_callable_body_symbols(machine);
        let mut used_submachines = HashSet::new();

        machine.instructions.retain(|ins| {
            let keep = symbols_in_callable.contains(&ins.name);
            if keep {
                for link in &ins.instruction.links {
                    used_submachines.insert(link.link.instance.clone());
                }
            }
            keep
        });

        let symbols_in_links = machine_in_links(machine);
        used_submachines.extend(symbols_in_links);

        machine
            .submachines
            .retain(|sub| used_submachines.contains(&sub.name));

        let mut symbols_in_callable = machine_callable_body_symbols(machine);
        let symbols_in_instructions = machine_instructions_symbols(machine);

        symbols_in_callable.insert("pc".to_string());
        symbols_in_callable.extend(symbols_in_instructions);

        machine
            .registers
            .retain(|reg| symbols_in_callable.contains(&reg.name));
    }
}

fn machine_callable_body_symbols(machine: &Machine) -> HashSet<String> {
    let mut callable_symbols = HashSet::new();

    for defs in machine.callable.function_definitions() {
        let FunctionSymbol { body, .. } = defs.function;

        for stms in body.statements.iter() {
            callable_symbols.extend(stms.symbols().map(|s| s.name.to_string()));
        }
    }
    callable_symbols
}

fn machine_instructions_symbols(machine: &Machine) -> HashSet<String> {
    let mut symbols = HashSet::new();
    for ins in machine.instructions.iter() {
        symbols.extend(ins.symbols().map(|s| s.name.to_string()));
    }

    symbols
}

fn machine_in_links(machine: &Machine) -> HashSet<String> {
    let submachine_to_decl: HashMap<String, String> = machine
        .submachines
        .iter()
        .map(|sub| (sub.name.clone(), sub.ty.clone().pop().unwrap().to_string()))
        .collect();

    machine
        .links
        .iter()
        .filter_map(|ld| submachine_to_decl.get(&ld.to.instance))
        .cloned() // fix
        .collect()
}
