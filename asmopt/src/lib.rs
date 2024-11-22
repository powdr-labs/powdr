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
        let mut submachine_names: Vec<String> = machine
            .submachines
            .iter()
            .map(|sub| sub.ty.clone().pop().unwrap().to_string())
            .collect();

        let param_names = machine
            .params
            .0
            .iter()
            .map(|param| param.ty.clone().unwrap().pop().unwrap().to_string()); //fix this
        submachine_names.extend(param_names);

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
        let mut defined_instructions = HashMap::new();

        //primero remuevo de main, en base a lo que me queda remuevo de insts y en base a eso de submachines
        //let callable_names: HashSet<_> =
        //    machine.submachines.iter().map(|s| s.name.clone()).collect();

        let callable_symbols = machine_callable_symbols(machine);

        for ins_def in &machine.instructions {
            let delegated_calls: Vec<_> = ins_def
                .instruction
                .links
                .iter()
                .map(|l| (l.link.instance.clone(), l.link.callable.clone()))
                .collect();
            defined_instructions.insert(ins_def.name.clone(), delegated_calls); //link to avoid empty?
        }

        let instr_to_remove: HashSet<_> = defined_instructions
            .keys()
            .cloned()
            .filter(|name| !callable_symbols.contains(name))
            .collect();

        let mut subs_to_remove = HashSet::new();
        for instr in &instr_to_remove {
            let tuples_to_check = defined_instructions.get(instr).unwrap();

            for (first, second) in tuples_to_check {
                let still_used = defined_instructions
                    .iter()
                    .filter(|(name, _)| !instr_to_remove.contains(*name))
                    .any(|(_, tuples)| tuples.iter().any(|(f, _)| f == first));

                if !still_used {
                    subs_to_remove.insert(second.clone());
                }
            }
        }

        machine
            .instructions
            .retain(|ins| !instr_to_remove.contains(&ins.name));
        machine
            .submachines
            .retain(|sub| !subs_to_remove.contains(&sub.name));
    }
}

fn machine_callable_symbols(machine: &Machine) -> HashSet<String> {
    let mut callable_symbols = HashSet::new();

    for defs in machine.callable.function_definitions() {
        let FunctionSymbol { body, .. } = defs.function;

        for stms in body.statements.iter() {
            callable_symbols.extend(stms.symbols().map(|s| s.name.to_string()));
        }
    }

    callable_symbols
}
