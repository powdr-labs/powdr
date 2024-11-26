use std::collections::{HashMap, HashSet};

use powdr_ast::{
    asm_analysis::{AnalysisASMFile, Machine},
    parsed::{asm::AbsoluteSymbolPath, types::Type, NamespacedPolynomialReference, PilStatement},
};
use powdr_pilopt::referenced_symbols::ReferencedSymbols;

type Expression = powdr_ast::asm_analysis::Expression<NamespacedPolynomialReference>;

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
    let mut all_machines = collect_all_dependent_machines(&deps, "::Main"); //do it once and remove useless for first deps
    let machines_with_cols = collect_machine_with_cols(&asm_file); //better names for everything xD
    all_machines.extend(machines_with_cols);

    //println!("All machines: {:?}", all_machines);
    asm_file.modules.iter_mut().for_each(|(path, module)| {
        let machines_in_module = machines_in_module(all_machines.clone(), path);
        //println!("Machines in path {:?}: {:?}", path, machines_in_module);
        module.retain_machines(machines_in_module);
    });
}

fn collect_machine_with_cols(asm_file: &AnalysisASMFile) -> HashSet<String> {
    let mut res = HashSet::new();
    for (path, machine) in asm_file.machines() {
        for pil in machine.pil.iter() {
            if let PilStatement::LetStatement(_, _, Some(args), _) = pil {
                if args.ty == Type::Col {
                    res.insert(path.to_string());
                }
            }
        }
    }
    res
}

fn machines_in_module(all_machines: HashSet<String>, path: &AbsoluteSymbolPath) -> HashSet<String> {
    //this can can be optimized, also
    // better with symbolpaths inst of string
    let mut res = HashSet::new();
    let path_str = path.to_string();
    for m in all_machines {
        let m_without_last = match m.rsplitn(2, "::").nth(1) {
            Some("") => "::".to_string(),
            Some(s) => s.to_string(),
            None => "::".to_string(),
        };
        if m_without_last == path_str {
            let name = m.rsplit("::").next().unwrap_or("");
            res.insert(name.to_string());
        }
    }

    res
}

fn build_machine_dependencies(asm_file: &AnalysisASMFile) -> HashMap<String, HashSet<String>> {
    let mut dependencies = HashMap::new();

    for (path, machine) in asm_file.machines() {
        let submachine_to_decl: HashMap<String, String> = machine
            .submachines
            .iter()
            .map(|sub| (sub.name.clone(), sub.ty.to_string())) // TODO full name
            .collect();

        let submachine_names: HashSet<String> = machine
            .submachines
            .iter()
            .map(|sub| sub.ty.to_string())
            .chain(machine.submachines.iter().flat_map(|sub| {
                sub.args.iter().filter_map(|expr| {
                    expr_to_ref(expr)
                        .and_then(|ref_name| submachine_to_decl.get(&ref_name).cloned())
                })
            }))
            .chain(
                machine
                    .params
                    .0
                    .iter()
                    .map(|param| param.ty.as_ref().unwrap().to_string()),
            )
            .chain(
                machine
                    .links
                    .iter()
                    .filter_map(|ld| submachine_to_decl.get(&ld.to.instance))
                    .cloned(),
            )
            .collect();
        dependencies.insert(path.to_string(), submachine_names);
    }

    //println!("Dependencies: {:?}", dependencies);

    dependencies
}

fn expr_to_ref(expr: &Expression) -> Option<String> {
    match expr {
        Expression::Reference(_, NamespacedPolynomialReference { path, .. }) => {
            Some(path.to_string())
        }
        Expression::PublicReference(_, pref) => Some(pref.clone()),
        _ => None,
    }
}

fn collect_all_dependent_machines(
    dependencies: &HashMap<String, HashSet<String>>,
    start: &str,
) -> HashSet<String> {
    let mut result = HashSet::new();
    let mut to_visit = vec![start.to_string()];
    let mut visited = HashSet::new();

    while let Some(machine) = to_visit.pop() {
        if visited.insert(machine.clone()) {
            result.insert(machine.clone());

            if let Some(submachines) = dependencies.get(&machine) {
                to_visit.extend(submachines.iter().cloned());
            }
        }
    }
    //println!("Collected machines: {:?}", result);
    //println!("Visited machines: {:?}", visited);

    result
}

fn remove_unused_instructions(asm_file: &mut AnalysisASMFile) {
    let machine_with_col = collect_machine_with_cols(&asm_file);

    for (pp, machine) in asm_file.machines_mut() {
        let symbols_in_callable = machine_callable_body_symbols(machine);
        let mut used_submachines: HashSet<String> = HashSet::new();

        let submachine_to_decl: HashMap<String, String> = machine
            .submachines
            .iter()
            .map(|sub| (sub.name.clone(), sub.ty.to_string()))
            .collect();

        machine.instructions.retain(|ins| {
            let keep = symbols_in_callable.contains(&ins.name);
            if keep {
                for link in &ins.instruction.links {
                    if let Some(submachine) = submachine_to_decl.get(&link.link.instance) {
                        used_submachines.insert(submachine.to_string());
                    }
                }
            }
            keep
        });

        let symbols_in_links = machine_in_links(machine);
        used_submachines.extend(symbols_in_links.clone());
        used_submachines.extend(machine_with_col.clone());

        let machhines_as_args: Vec<_> = machine
            .submachines
            .iter()
            .map(|sm| sm.args.iter().filter_map(|arg| expr_to_ref(arg)))
            .flatten()
            .filter_map(|ref_name| submachine_to_decl.get(&ref_name))
            .cloned() //fixme
            .collect();

        used_submachines.extend(machhines_as_args);

        machine
            .submachines
            .retain(|sub| used_submachines.contains(&sub.ty.to_string()));

        let mut symbols_in_callable = machine_callable_body_symbols(machine);
        println!("Symbols in callable body of {pp:?}: {symbols_in_callable:?}",); // a
        let symbols_in_instructions = machine_instructions_symbols(machine);

        println!("Symbols in instructions of {pp:?}: {symbols_in_instructions:?}",); // x y
        println!("Registers in {pp:?}: {:?}", machine.registers);

        symbols_in_callable.insert("pc".to_string());
        symbols_in_callable.extend(symbols_in_instructions);
        symbols_in_callable.extend(symbols_in_links);

        machine
            .registers
            .retain(|reg| symbols_in_callable.contains(&reg.name));
    }
}

fn machine_callable_body_symbols(machine: &Machine) -> HashSet<String> {
    machine
        .callable
        .function_definitions()
        .flat_map(|def| {
            def.symbols()
                .map(|s| s.name.to_string())
                .collect::<Vec<_>>()
        })
        .collect()
}

fn machine_instructions_symbols(machine: &Machine) -> HashSet<String> {
    let mut symbols = HashSet::new();
    for ins in machine.instructions.iter() {
        println!("Instruction: {ins:?}");
        symbols.extend(ins.symbols().map(|s| s.name.to_string()));
    }

    symbols
}

fn machine_in_links(machine: &Machine) -> HashSet<String> {
    let submachine_to_decl: HashMap<String, String> = machine
        .submachines
        .iter()
        .map(|sub| (sub.name.clone(), sub.ty.to_string()))
        .collect();

    machine
        .links
        .iter()
        .filter_map(|ld| submachine_to_decl.get(&ld.to.instance))
        .cloned() // fix
        .collect()
}
